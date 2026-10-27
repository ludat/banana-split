{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module BananaSplit.Persistence.Pg (
  Pg,
  MonadPg (..),
  liftPg,
  tryJustPg,
  uniqueViolation,
  foreignKeyViolation,

  -- * Correr una acción
  conConexionDelPool,
  conTransaccionDeEscritura,
  conTransaccionDeLecturaRapida,
  correrEnLaTransaccionDeAfuera,
  contandoQueries,
  conQueriesEnElSpan,

  -- * Registrar las queries en el span
  RegistroDeQueries,
  nuevoRegistro,
  registrarStatement,
  terminarStatement,
  fallarStatement,
  taparDetalle,
  anotarQueriesEnElSpan,
  resumirQuery,
  taparValores,
) where

import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Pool qualified as Pool
import Data.Text qualified as Text
import Database.Beam.Backend.SQL (BeamSqlBackendSyntax, FromBackendRow, MonadBeam (..))
import Database.Beam.Postgres (Connection, Postgres, liftIOWithHandle, runBeamPostgres, runBeamPostgresDebug)
import Database.Beam.Postgres qualified as Beam
import Database.Beam.Postgres.Syntax (fromPgCommand, pgRenderSyntaxScript)
import Database.PostgreSQL.Simple (SqlError (..))
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (..), constraintViolation, isSerializationError)
import Database.PostgreSQL.Simple.Transaction qualified as Transaction
import Katip qualified
import OpenTelemetry.Trace.Core (Span)
import OpenTelemetry.Trace.Core qualified as Otel

import BananaSplit.Telemetry (Telemetry (..))
import BananaSplit.Telemetry qualified as Telemetry
import Preludat

-- | Lo que 'Pg' lleva en el reader.
data EntornoPg = EntornoPg
  { telemetry :: Telemetry
  , registro :: Maybe RegistroDeQueries
  -- ^ Dónde anotar las queries que corren: el registro del span que las
  -- contiene, o nada si no hay ninguno (los tests, que corren en la
  -- transacción de afuera).
  }

instance Telemetry.HasTelemetry EntornoPg where
  getTelemetry entorno = entorno.telemetry
  overTelemetry f entorno = entorno{telemetry = f entorno.telemetry}

newtype Pg a = Pg (ReaderT EntornoPg Beam.Pg a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader EntornoPg
    )
  deriving
    (Katip.Katip, Katip.KatipContext, Telemetry.MonadTracer)
    via (Telemetry.ViaTelemetry (ReaderT EntornoPg Beam.Pg))

liftPg :: Beam.Pg a -> Pg a
liftPg = Pg . lift

-- | 'tryJust' adentro de la transacción. Cuando una sentencia falla, Postgres
-- aborta la transacción entera, así que la acción corre en un savepoint: si
-- tira, la base vuelve a él y la transacción sigue usable. Lo que @reconocer@
-- no reconoce se relanza.
--
-- Como en 'Telemetry.inSpan'', el 'Pg' de Beam no tiene cómo atrapar nada, así
-- que se toma la conexión y la acción se interpreta en 'IO'. Corre con el mismo
-- entorno, así que sus queries se cuentan en el span de afuera.
tryJustPg :: (Exception e) => (e -> Maybe b) -> Pg a -> Pg (Either b a)
tryJustPg reconocer accion = do
  entorno <- ask
  resultado <- liftPg $ liftIOWithHandle $ \conn ->
    tryJust (\e -> (e,) <$> reconocer e)
      $ Transaction.withSavepoint conn (runBeamPostgres conn (correr entorno accion))
  case resultado of
    Right a -> pure (Right a)
    Left (e, b) -> do
      -- La query que falló ya está manejada: deja su evento como cualquier
      -- otra, y no es la que hizo explotar al span.
      for_ entorno.registro $ \registro -> liftIO $ fallarStatement registro (toException e)
      pure (Left b)

-- | Para 'tryJustPg': la escritura chocó con ese índice único.
uniqueViolation :: ByteString -> b -> SqlError -> Maybe b
uniqueViolation indice resultado sqlError =
  resultado <$ guard (constraintViolation sqlError == Just (UniqueViolation indice))

-- | Para 'tryJustPg': no se pudo borrar de esa tabla porque una foreign key la
-- sigue referenciando. Cuál es la decide @reconocer@ por su nombre; una que no
-- reconoce se relanza.
foreignKeyViolation :: ByteString -> (ByteString -> Maybe b) -> SqlError -> Maybe b
foreignKeyViolation tabla reconocer sqlError =
  case constraintViolation sqlError of
    Just (ForeignKeyViolation tablaBorrada constraint) | tablaBorrada == tabla -> reconocer constraint
    _ -> Nothing

correr :: EntornoPg -> Pg a -> Beam.Pg a
correr entorno (Pg accion) = runReaderT accion entorno

class (MonadIO m) => MonadPg m where
  -- | Para cualquier cosa que escriba. Ver 'conTransaccionDeEscritura'.
  runBeamWrite :: Pg a -> m a

  -- | Para leer y mostrar, nada más. Ver 'conTransaccionDeLecturaRapida'.
  runBeamFastRead :: Pg a -> m a

instance MonadPg Pg where
  runBeamWrite = identity
  runBeamFastRead = identity

-- | Acá, y no en el hook de debug de Beam, es donde se registran las queries:
-- es el único lugar que tiene a la vez el SQL y las filas que volvieron.
instance MonadBeam Postgres Pg where
  runNoReturn sintaxis =
    instrumentarStatement sintaxis $ do
      liftPg $ runNoReturn sintaxis
      pure ((), Nothing)

  runReturningMany ::
    forall x a.
    (FromBackendRow Postgres x) =>
    BeamSqlBackendSyntax Postgres
    -> (Pg (Maybe x) -> Pg a)
    -> Pg a
  runReturningMany sintaxis callback =
    instrumentarStatement sintaxis $ do
      entorno <- ask
      filas <- liftIO $ newIORef 0
      -- Cuenta las filas que el callback efectivamente pidió: las que se leyeron,
      -- que es lo que nos interesa medir.
      let siguienteContando siguiente = do
            fila <- liftPg siguiente
            when (isJust fila) $ liftIO $ modifyIORef' filas (+ 1)
            pure fila
      resultado <-
        liftPg $ runReturningMany sintaxis $ \siguiente ->
          correr entorno (callback (siguienteContando siguiente))
      cantidad <- liftIO $ readIORef filas
      pure (resultado, Just cantidad)

-- | Corre un statement anotándolo en el registro del span, si hay uno. La
-- acción devuelve, además del resultado, cuántas filas trajo, o 'Nothing' si
-- es un statement que no devuelve filas.
instrumentarStatement :: BeamSqlBackendSyntax Postgres -> Pg (a, Maybe Int64) -> Pg a
instrumentarStatement sintaxis correrlo =
  asks (.registro) >>= \case
    Nothing -> fst <$> correrlo
    Just registro -> do
      enCurso <- liftIO $ registrarStatement registro (sqlDe sintaxis)
      (resultado, filas) <- correrlo
      liftIO $ terminarStatement registro enCurso filas
      pure resultado

-- | El SQL de un statement de Beam. Solo se usa para resumirlo, así que un byte
-- que no sea UTF-8 no puede hacer que falle la query: se reemplaza.
sqlDe :: BeamSqlBackendSyntax Postgres -> Text
sqlDe sintaxis =
  decodeUtf8With lenientDecode (toS (pgRenderSyntaxScript (fromPgCommand sintaxis)) :: ByteString)

instance Telemetry.MonadTelemetry Pg where
  inSpan' nombre accion = do
    telemetria <- asks (.telemetry)
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
  runBeamPostgres conn (correr EntornoPg{telemetry = telemetria, registro = Nothing} accion)

-- | Corre la acción y devuelve también el resumen de cada statement que
-- mandó, en orden. Es para los tests que fijan cuántas queries hace algo: usa
-- la misma conexión y la misma transacción, así que ve lo que escribió el
-- test antes.
contandoQueries :: Pg a -> Pg (a, [Text])
contandoQueries accion = do
  entorno <- ask
  liftPg $ liftIOWithHandle $ \conn -> do
    statements <- newIORef []
    resultado <-
      runBeamPostgresDebug
        (\sql -> modifyIORef' statements (resumirQuery (toS sql) :))
        conn
        (correr entorno accion)
    (resultado,) . reverse <$> readIORef statements

conQueriesEnElSpan :: Span -> Telemetry -> Connection -> Pg a -> IO a
conQueriesEnElSpan span telemetria conn accion = do
  registro <- nuevoRegistro span
  runBeamPostgres conn (correr EntornoPg{telemetry = telemetria, registro = Just registro} accion)
    `finally` anotarQueriesEnElSpan registro

-- | Lo que se va juntando de las queries de un span mientras corren, para
-- dejarlo como atributos del span cuando termina.
data RegistroDeQueries = RegistroDeQueries
  { span :: Span
  , resumenes :: IORef [Text]
  , filasDevueltas :: IORef Int64
  , sinTerminar :: IORef (Maybe QueryEnCurso)
  -- ^ La query que se mandó y todavía no terminó. Si el span se cierra con
  -- algo acá, es la que explotó.
  }

nuevoRegistro :: Span -> IO RegistroDeQueries
nuevoRegistro span = do
  resumenes <- newIORef []
  filasDevueltas <- newIORef 0
  sinTerminar <- newIORef Nothing
  pure RegistroDeQueries{span, resumenes, filasDevueltas, sinTerminar}

-- | Una query que ya se mandó y todavía no terminó.
data QueryEnCurso = QueryEnCurso
  { sql :: Text
  , resumen :: Text
  , inicio :: Otel.Timestamp
  }

-- | Anota la query al mandarla, antes de saber cómo le fue: así una que falla
-- sigue apareciendo en el resumen del span. El evento lo deja
-- 'terminarStatement', que es el que sabe cuántas filas volvieron.
registrarStatement :: RegistroDeQueries -> Text -> IO QueryEnCurso
registrarStatement registro sqlConValores = do
  inicio <- Otel.getTimestamp
  let resumen = resumirQuery sqlConValores
      sql = taparValores sqlConValores
  let enCurso = QueryEnCurso{sql, resumen, inicio}
  modifyIORef' registro.resumenes (resumen :)
  writeIORef registro.sinTerminar (Just enCurso)
  pure enCurso

-- | Deja el evento @db.query@ de una query que terminó, fechado cuando empezó,
-- con las filas que devolvió si es de las que devuelven filas.
--
-- El evento lleva también la query entera con los valores tapados
-- ('taparValores'), para debuggear. El resumen sigue estando para agrupar por
-- query.
terminarStatement :: RegistroDeQueries -> QueryEnCurso -> Maybe Int64 -> IO ()
terminarStatement registro enCurso filas = do
  writeIORef registro.sinTerminar Nothing
  forM_ filas $ \cantidad -> modifyIORef' registro.filasDevueltas (+ cantidad)
  agregarEventoDeQuery registro enCurso
    [("db.response.returned_rows", Otel.toAttribute cantidad) | Just cantidad <- [filas]]

-- | Deja el evento @db.query@ de la query que falló con una excepción que se
-- atrapó y manejó ('tryJustPg'): como no hace explotar al span, no queda en el
-- span sino en su evento, junto con el error entero ('atributosDelError').
fallarStatement :: RegistroDeQueries -> SomeException -> IO ()
fallarStatement registro excepcion =
  readIORef registro.sinTerminar >>= \case
    Nothing -> pure ()
    Just enCurso -> do
      writeIORef registro.sinTerminar Nothing
      agregarEventoDeQuery registro enCurso (atributosDelError excepcion)

-- | Todo lo que se sabe del error, con los nombres de la convención de
-- OpenTelemetry: @exception.type@ y @exception.message@, y para uno de Postgres
-- el SQLSTATE en @db.response.status_code@ y también en @error.type@. El detalle
-- y la pista de Postgres no tienen nombre en la convención, van solo si vienen,
-- y el detalle con los valores tapados ('taparDetalle').
atributosDelError :: SomeException -> [(Text, Otel.Attribute)]
atributosDelError excepcion@(SomeException interna) =
  case fromException excepcion of
    Just (sqlError :: SqlError) ->
      -- No 'interna': postgresql-simple envuelve sus errores en su jerarquía de
      -- excepciones, y lo que hay adentro es un 'SomePostgreSqlException'.
      let codigo = Otel.toAttribute (texto sqlError.sqlState)
       in ("exception.type", tipo sqlError)
            : ("db.response.status_code", codigo)
            : ("error.type", codigo)
            : ("exception.message", Otel.toAttribute (texto sqlError.sqlErrorMsg))
            : opcional "db.postgresql.error.detail" (taparDetalle (texto sqlError.sqlErrorDetail))
            <> opcional "db.postgresql.error.hint" (texto sqlError.sqlErrorHint)
    Nothing ->
      [ ("exception.type", tipo interna)
      , ("error.type", Otel.toAttribute ("_OTHER" :: Text))
      , ("exception.message", Otel.toAttribute (toS (displayException excepcion) :: Text))
      ]
  where
    tipo :: (Typeable t) => t -> Otel.Attribute
    tipo valor = Otel.toAttribute (show (typeOf valor) :: Text)

    texto :: ByteString -> Text
    texto = decodeUtf8With lenientDecode

    opcional :: Text -> Text -> [(Text, Otel.Attribute)]
    opcional clave valor = [(clave, Otel.toAttribute valor) | not (Text.null valor)]

-- | El detalle de un error de Postgres trae los valores de la fila que chocó,
-- como @Key (nombre)=(Ana) already exists.@: se tapan como en 'taparValores' y
-- queda @Key (nombre)=(?) already exists.@ Ante la duda se tapa de más: un valor
-- que no cierra tapa todo lo que sigue, y uno con paréntesis adentro se tapa
-- hasta el último.
taparDetalle :: Text -> Text
taparDetalle detalle =
  case Text.splitOn "=(" detalle of
    primero : valores@(_ : _) -> Text.intercalate "=(" (primero : map taparValor valores)
    _ -> detalle
  where
    taparValor resto =
      case Text.breakOnEnd ")" resto of
        ("", _) -> "?"
        (_, despues) -> "?)" <> despues

agregarEventoDeQuery :: RegistroDeQueries -> QueryEnCurso -> [(Text, Otel.Attribute)] -> IO ()
agregarEventoDeQuery registro enCurso atributos =
  Otel.addEvent registro.span
    $ Otel.NewEvent
      { Otel.newEventName = "db.query"
      , Otel.newEventAttributes =
          HashMap.fromList
            $ ("db.query.summary", Otel.toAttribute enCurso.resumen)
            : ("db.query.text", Otel.toAttribute enCurso.sql)
            : atributos
      , Otel.newEventTimestamp = Just enCurso.inicio
      }

anotarQueriesEnElSpan :: RegistroDeQueries -> IO ()
anotarQueriesEnElSpan registro = do
  queries <- reverse <$> readIORef registro.resumenes
  filas <- readIORef registro.filasDevueltas
  -- La que no terminó no tiene evento: su SQL va en el span, que es donde
  -- también queda la excepción.
  readIORef registro.sinTerminar >>= \case
    Nothing -> pure ()
    Just enCurso -> Otel.addAttribute registro.span "db.query.text" enCurso.sql
  unless (null queries)
    $ Otel.addAttributes registro.span
    $ HashMap.fromList
      [ ("db.query.summary", Otel.toAttribute $ recortar $ Text.intercalate "; " queries)
      , ("db.query.count", Otel.toAttribute (fromIntegral (length queries) :: Int64))
      , ("db.response.returned_rows", Otel.toAttribute filas)
      ]
  where
    recortar :: Text -> Text
    recortar texto
      | Text.length texto <= maxLargoSql = texto
      | otherwise = Text.take maxLargoSql texto <> "…"

    maxLargoSql :: Int
    maxLargoSql = 2000

-- | La query con cada valor entre comillas simples cambiado por @?@, como si
-- fuera una con parámetros: @WHERE ("t0"."email") = (?)@.
--
-- Beam escribe entre comillas simples todo lo que es texto, los ULIDs, las
-- fechas, los jsonb y los decimales, que es todo lo que puede decir algo de
-- una persona. Los enteros, los booleanos y @null@ van sin comillas y quedan a
-- la vista: los montos se guardan en unidades mínimas, así que un número
-- suelto se ve, pero no de quién es ni de qué gasto.
--
-- Las comillas dobles no se tocan: son nombres de tablas y columnas.
--
-- Una comilla adentro de un valor va doble (@'o''brien'@), y la barra no
-- escapa nada. Si una comilla no cierra, se tapa todo lo que sigue: ante la
-- duda, no se muestra.
taparValores :: Text -> Text
taparValores sql =
  case Text.breakOn "'" sql of
    (antes, "") -> antes
    (antes, desdeLaComilla) -> antes <> "?" <> taparValores (despuesDelValor (Text.drop 1 desdeLaComilla))
  where
    -- Lo que queda después de la comilla que cierra el valor.
    despuesDelValor adentro =
      case Text.breakOn "'" adentro of
        (_, "") -> ""
        (_, desdeLaComilla) ->
          let despues = Text.drop 1 desdeLaComilla
          in if "'" `Text.isPrefixOf` despues
               then despuesDelValor (Text.drop 1 despues)
               else despues

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
