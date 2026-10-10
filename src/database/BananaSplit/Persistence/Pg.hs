{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | La mónada de las acciones contra la base, y qué queda registrado de ellas.
--
-- Todo lo que toca la base se escribe en el 'Pg' de acá, que le tapa el nombre al de
-- Beam a propósito: es el que hay que usar. Lo que lo distingue del de Beam
-- —accesible como @Beam.Pg@— es que puede instrumentarse: abre spans y loguea,
-- porque alguien le da el 'Telemetry' al abrir la transacción.
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

-- | Una acción contra la base, instrumentable.
--
-- Es el @Beam.Pg@ con el 'Telemetry' colgado: por abajo sigue siendo Beam —cumple
-- 'MonadBeam', así que @runSelectReturningOne@ y compañía andan igual— y por arriba
-- cumple las capacidades de telemetría, que es lo que el de Beam no puede dar.
--
-- El tipo existe porque a @Beam.Pg@ no se le pueden agregar instancias útiles: es un
-- free monad de otra librería, sin reader donde poner el 'Telemetry'. Y tapa su
-- nombre para que nadie lo elija por descuido.
--
-- Es la excepción a que las mónadas concretas vivan sólo en los entry points: el
-- entry point de una transacción es 'conTransaccionDeEscritura', que está acá.
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

-- | Una acción de Beam sin instrumentar, metida en una 'Pg'.
--
-- Para lo poco que está tipado como @Beam.Pg@ y no es nuestro: las migraciones, lo
-- que devuelve una librería, @liftIOWithHandle@.
liftPg :: Beam.Pg a -> Pg a
liftPg = Pg . lift

correr :: Telemetry -> Pg a -> Beam.Pg a
correr telemetria (Pg accion) = runReaderT accion telemetria

-- | Poder correr una 'Pg'.
--
-- La capacidad que necesita cualquier cosa que toque la base, sin decir desde dónde:
-- un handler la cumple sacando una conexión del pool y abriendo la transacción, y
-- 'Pg' la cumple sin hacer nada, porque adentro ya está abierta y no hay
-- nada que elegir.
--
-- Esa es también la razón de que la clase tenga los dos métodos en vez de uno con un
-- parámetro: el modo de la transacción lo decide quien la abre, y quien ya está
-- adentro no puede cambiarlo.
class (MonadIO m) => MonadPg m where
  -- | Para cualquier cosa que escriba. Ver 'conTransaccionDeEscritura'.
  runBeamWrite :: Pg a -> m a

  -- | Para leer y mostrar, nada más. Ver 'conTransaccionDeLecturaRapida'.
  runBeamFastRead :: Pg a -> m a

instance MonadPg Pg where
  runBeamWrite = identity
  runBeamFastRead = identity

-- | Beam, sin cambios: cada método es el de @Beam.Pg@ con el 'Telemetry' paseado.
--
-- 'runReturningMany' es el único que da trabajo porque le pasa la mónada a un
-- callback, así que hay que bajar y volver a subir el reader.
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

-- | Un span alrededor de un pedazo de transacción.
--
-- Hace falta hacerlo a mano porque abajo hay un free monad: tiene 'MonadIO' pero no
-- 'MonadUnliftIO', así que 'Otel.inSpan'' no se le puede aplicar derecho. La vuelta es
-- 'liftIOWithHandle', que presta la conexión en 'IO': con la conexión en la mano, la
-- acción de adentro se interpreta con un runner anidado, ya dentro del span y dentro
-- del @bracket@ de 'Otel.inSpan'', que es lo que hace que una excepción igual lo
-- cierre.
--
-- Las queries que corren adentro quedan en el span de adentro, no en el de afuera:
-- las interpreta el runner anidado, que tiene su propio hook. O sea que el resumen
-- del span de afuera deja de contarlas; están, un nivel más abajo.
instance Telemetry.MonadTracer Pg where
  getTracer = asks (.tracer)

instance Telemetry.MonadTelemetry Pg where
  inSpan' nombre accion = do
    telemetria <- ask
    liftPg $ liftIOWithHandle $ \conn ->
      Otel.inSpan' telemetria.tracer nombre Otel.defaultSpanArguments $ \span ->
        conQueriesEnElSpan span telemetria conn (accion span)

-- | La transacción de cualquier cosa que escriba.
--
-- Va en SERIALIZABLE porque el cache del resumen de un gasto se calcula leyendo
-- cosas que otra transacción puede estar cambiando al mismo tiempo (los claims
-- de una repartija, sobre todo). Con READ COMMITTED cada statement ve un
-- snapshot nuevo, así que dos escrituras pueden leer los mismos insumos y
-- pisarse; en SERIALIZABLE el resultado tiene que ser equivalente a haberlas
-- corrido una después de la otra, y si no lo es Postgres aborta una.
--
-- De ahí el reintento: fallar con @40001@ es parte del protocolo, no un error.
-- La transacción que aborta no dejó nada escrito, así que volver a correr el
-- bloque entero es seguro.
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

-- | La transacción de leer para mostrar, que es lo más barato que hay: no toma
-- predicate locks, no puede abortar por conflicto y no le agrega trabajo a las
-- escrituras que corren en paralelo.
--
-- El @READ ONLY@ no es sólo una pista: Postgres rechaza cualquier escritura que
-- entre por acá, así que una que se cuele rompe en el momento en vez de perder
-- en silencio las garantías de arriba.
--
-- Lo que se resigna es que cada statement ve un snapshot distinto, así que dos
-- queries de la misma transacción pueden ver estados distintos de la base.
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

-- | Una conexión del pool, midiendo cuánto costó conseguirla.
--
-- Esperar a que se libere una conexión es justo lo que querés ver cuando un endpoint
-- tarda y las queries son rápidas, y con un span propio no sólo se ve: se mide.
--
-- El span se arma con 'Otel.startTime' puesto a mano, al momento en que se pidió, y se
-- cierra en el momento en que llegó. Es la forma de medir sólo la espera sin
-- reimplementar el @bracket@ de 'Pool.withResource', que es lo que se encarga de
-- devolver la conexión —o de tirarla— pase lo que pase. Si conseguirla falla no hay
-- span, pero tampoco hay transacción: la excepción queda en el span de afuera.
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

-- | El span de una transacción, que es el que se le cuelga todo lo de las queries.
--
-- Está acá y no en cada 'MonadPg' para que el nombre, el @kind@ y lo que el span
-- abarca sean los mismos para un handler y para un comando. Antes el handler lo abría
-- a mano y el comando no abría ninguno, así que las queries de un comando se colgaban
-- del span que hubiera quedado activo.
enElSpanDeLaTransaccion :: Telemetry -> Text -> (Span -> IO a) -> IO a
enElSpanDeLaTransaccion telemetria nombre =
  Otel.inSpan' telemetria.tracer nombre spanDeCliente

spanDeCliente :: Otel.SpanArguments
spanDeCliente = Otel.defaultSpanArguments{Otel.kind = Otel.Client}

-- | Correr una acción sin abrir transacción, porque el de afuera ya abrió una.
--
-- Es para los tests: envuelven cada caso en un @BEGIN@ / @ROLLBACK@ propio, que es lo
-- que hace que uno no le deje nada escrito al siguiente. Si usaran
-- 'conTransaccionDeEscritura', el @BEGIN@ de adentro sería un no-op y su @COMMIT@
-- commitearía el de afuera, o sea que se perdería el rollback.
--
-- Nadie más debería necesitarla: en autocommit cada statement se commitea solo, así
-- que una acción de varios pasos puede quedar hecha a medias.
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

-- | Pasa lo acumulado a atributos del span, en el orden en que corrieron.
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

    -- Que esto esté acotado es media razón de ser del resumen, así que no
    -- depende de que el SQL venga con la forma esperada: un token larguísimo se
    -- corta igual.
    acotar = Text.take maxLargoToken

    palabras = Text.words $ Text.map (\c -> if c == '\n' || c == '\t' then ' ' else c) sql

    -- Dónde está la tabla depende del verbo, y el nombre viene entrecomillado
    -- porque Beam siempre cita los identificadores.
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

    -- Si no es un identificador citado es una subquery, un paréntesis o algo que
    -- no vale la pena adivinar.
    identificador palabra = do
      sinComillas <- Text.stripSuffix "\"" =<< Text.stripPrefix "\"" palabra
      guard $ not $ Text.null sinComillas
      pure sinComillas
