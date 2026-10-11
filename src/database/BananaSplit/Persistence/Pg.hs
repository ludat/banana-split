{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module BananaSplit.Persistence.Pg (
  Pg,
  MonadPg (..),
  liftPg,

  -- * Correr una acción
  conConexionDelPool,
  conTransaccionDeEscritura,
  conTransaccionDeLecturaRapida,
  correrEnLaTransaccionDeAfuera,
  conQueriesEnElSpan,
  anotarQueriesEnElSpan,
  registrarStatement,
  resumirQuery,
) where

import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Pool qualified as Pool
import Data.String (String)
import Data.Text qualified as Text
import Database.Beam.Backend.SQL (BeamSqlBackendSyntax, FromBackendRow, MonadBeam (..))
import Database.Beam.Postgres (Connection, Postgres, liftIOWithHandle, runBeamPostgres, runBeamPostgresDebug)
import Database.Beam.Postgres qualified as Beam
import Database.PostgreSQL.Simple.Errors (isSerializationError)
import Database.PostgreSQL.Simple.Transaction qualified as Transaction
import Katip qualified
import OpenTelemetry.Trace.Core (Span)
import OpenTelemetry.Trace.Core qualified as Otel

import BananaSplit.Telemetry (Telemetry (..))
import BananaSplit.Telemetry qualified as Telemetry
import Preludat

newtype Pg a = Pg (ReaderT Telemetry Beam.Pg a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader Telemetry
    )
  deriving
    (Katip.Katip, Katip.KatipContext)
    via (Telemetry.ConTelemetria (ReaderT Telemetry Beam.Pg))

liftPg :: Beam.Pg a -> Pg a
liftPg = Pg . lift

correr :: Telemetry -> Pg a -> Beam.Pg a
correr telemetria (Pg accion) = runReaderT accion telemetria

class (MonadIO m) => MonadPg m where
  -- | Para cualquier cosa que escriba. Ver 'conTransaccionDeEscritura'.
  runBeamWrite :: Pg a -> m a

  -- | Para leer y mostrar, nada más. Ver 'conTransaccionDeLecturaRapida'.
  runBeamFastRead :: Pg a -> m a

instance MonadPg Pg where
  runBeamWrite = identity
  runBeamFastRead = identity

instance MonadBeam Postgres Pg where
  runNoReturn sintaxis = liftPg $ runNoReturn sintaxis

  runReturningMany ::
    forall x a.
    (FromBackendRow Postgres x) =>
    BeamSqlBackendSyntax Postgres
    -> (Pg (Maybe x) -> Pg a)
    -> Pg a
  runReturningMany sintaxis callback = do
    telemetria <- ask
    liftPg $ runReturningMany sintaxis $ \siguiente ->
      correr telemetria (callback (liftPg siguiente))

instance Telemetry.MonadTracer Pg where
  getTracer = asks (.tracer)

instance Telemetry.MonadTelemetry Pg where
  inSpan' nombre accion = do
    telemetria <- ask
    liftPg $ liftIOWithHandle $ \conn ->
      Otel.inSpan' telemetria.tracer nombre Otel.defaultSpanArguments $ \span ->
        conQueriesEnElSpan span telemetria conn (accion span)

conTransaccionDeEscritura :: Telemetry -> Connection -> Pg a -> IO a
conTransaccionDeEscritura telemetria conn accion =
  enElSpanDeLaTransaccion telemetria "db.write" $ \span ->
    Transaction.withTransactionModeRetry
      Transaction.TransactionMode
        { Transaction.isolationLevel = Transaction.RepeatableRead
        , Transaction.readWriteMode = Transaction.ReadWrite
        }
      isSerializationError
      conn
      (conQueriesEnElSpan span telemetria conn accion)

conTransaccionDeLecturaRapida :: Telemetry -> Connection -> Pg a -> IO a
conTransaccionDeLecturaRapida telemetria conn accion =
  enElSpanDeLaTransaccion telemetria "db.read" $ \span ->
    Transaction.withTransactionMode
      Transaction.TransactionMode
        { Transaction.isolationLevel = Transaction.ReadCommitted
        , Transaction.readWriteMode = Transaction.ReadOnly
        }
      conn
      (conQueriesEnElSpan span telemetria conn accion)

conConexionDelPool :: Telemetry -> Pool.Pool Connection -> (Connection -> IO a) -> IO a
conConexionDelPool telemetria pool usar = do
  pedida <- Otel.getTimestamp
  Pool.withResource pool $ \conn -> do
    Otel.inSpan
      telemetria.tracer
      "db.pool.acquire"
      spanDeCliente{Otel.startTime = Just pedida}
      (pure ())
    usar conn

enElSpanDeLaTransaccion :: Telemetry -> Text -> (Span -> IO a) -> IO a
enElSpanDeLaTransaccion telemetria nombre =
  Otel.inSpan' telemetria.tracer nombre spanDeCliente

spanDeCliente :: Otel.SpanArguments
spanDeCliente = Otel.defaultSpanArguments{Otel.kind = Otel.Client}

correrEnLaTransaccionDeAfuera :: Telemetry -> Connection -> Pg a -> IO a
correrEnLaTransaccionDeAfuera telemetria conn accion =
  runBeamPostgres conn (correr telemetria accion)

conQueriesEnElSpan :: Span -> Telemetry -> Connection -> Pg a -> IO a
conQueriesEnElSpan span telemetria conn accion = do
  statements <- newIORef []
  runBeamPostgresDebug (registrarStatement span statements) conn (correr telemetria accion)
    `finally` anotarQueriesEnElSpan span statements

registrarStatement :: Span -> IORef [Text] -> String -> IO ()
registrarStatement span statements sql = do
  let resumen = resumirQuery (toS sql)
  modifyIORef' statements (resumen :)
  Otel.addEvent span
    $ Otel.NewEvent
      { Otel.newEventName = "db.query"
      , Otel.newEventAttributes =
          HashMap.fromList [("db.query.summary", Otel.toAttribute resumen)]
      , Otel.newEventTimestamp = Nothing
      }

anotarQueriesEnElSpan :: Span -> IORef [Text] -> IO ()
anotarQueriesEnElSpan span statements = do
  queries <- reverse <$> readIORef statements
  unless (null queries)
    $ Otel.addAttributes span
    $ HashMap.fromList
      [ ("db.query.summary", Otel.toAttribute $ recortar $ Text.intercalate "; " queries)
      , ("db.query.count", Otel.toAttribute (fromIntegral (length queries) :: Int64))
      ]
  where
    recortar :: Text -> Text
    recortar texto
      | Text.length texto <= maxLargoSql = texto
      | otherwise = Text.take maxLargoSql texto <> "…"

    maxLargoSql :: Int
    maxLargoSql = 2000

resumirQuery :: Text -> Text
resumirQuery sql =
  case palabras of
    [] -> "(vacía)"
    (verbo : resto) -> case tabla verbo resto of
      Just nombre -> acotar (Text.toUpper verbo) <> " " <> acotar nombre
      Nothing -> acotar (Text.toUpper verbo)
  where
    maxLargoToken :: Int
    maxLargoToken = 40

    acotar = Text.take maxLargoToken

    palabras = Text.words $ Text.map (\c -> if c == '\n' || c == '\t' then ' ' else c) sql

    tabla verbo resto = case Text.toUpper verbo of
      "SELECT" -> despuesDe "FROM" resto
      "DELETE" -> despuesDe "FROM" resto
      "INSERT" -> despuesDe "INTO" resto
      "UPDATE" -> identificador =<< head resto
      _ -> Nothing

    despuesDe palabra resto =
      case dropWhile ((/= palabra) . Text.toUpper) resto of
        (_ : siguiente : _) -> identificador siguiente
        _ -> Nothing

    identificador palabra = do
      sinComillas <- Text.stripSuffix "\"" =<< Text.stripPrefix "\"" palabra
      guard $ not $ Text.null sinComillas
      pure sinComillas
