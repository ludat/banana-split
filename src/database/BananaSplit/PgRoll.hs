module BananaSplit.PgRoll (
  getLatestSchema,
  init,
  rawCall,
  rollback,
  start,
  startAndComplete,
) where

import Conferer
import Data.String
import Data.Text qualified as Text
import OpenTelemetry.Trace.Core qualified as Otel
import Protolude
import System.Process (callProcess, readProcess)

import BananaSplit.Telemetry (Telemetry, inSpan', logAttr, logError, logInfo, withLogging)

-- | Corre @pgroll@ con la conexión que sale de la config.
--
-- El span abarca el subproceso entero, que es lo único que se puede medir desde
-- acá: @pgroll@ no reporta nada a OpenTelemetry, así que de una migración larga
-- se ve cuánto tardó y si falló, no en qué paso está.
--
-- Solo se registran los @args@, nunca la URL de conexión: lleva la contraseña.
-- Y por eso mismo la excepción se ataja /adentro/ del span y se re-lanza
-- afuera, en lugar de dejarla atravesarlo: cuando @callProcess@ falla, el
-- mensaje de la 'IOException' incluye el argv completo — con la URL y la
-- contraseña — y 'inSpan'' graba el mensaje de cualquier excepción que lo cruce
-- como @exception.message@. Dejándola pasar, la contraseña termina en Tempo.
rawCall :: Telemetry -> Config -> [String] -> IO ()
rawCall telemetry config args = do
  connString <- Conferer.fetchFromConfig "database.url" config
  let connectionArgs = ["--postgres-url", connString ++ "?sslmode=disable"]
      comando = Text.unwords $ fmap toS args
  outcome <- inSpan' telemetry ("pgroll " <> comando) $ \span -> withLogging telemetry $ do
    Otel.addAttribute span "app.pgroll.args" comando
    logInfo "migration.pgroll.start" [logAttr "app.pgroll.args" comando]
    result <- liftIO $ try @SomeException $ callProcess "pgroll" $ connectionArgs ++ args
    case result of
      Right () ->
        logInfo "migration.pgroll.done" [logAttr "app.pgroll.args" comando]
      Left _ -> do
        Otel.setStatus span (Otel.Error "pgroll failed")
        logError "migration.pgroll.failed" [logAttr "app.pgroll.args" comando]
    pure result
  either throwIO pure outcome

getLatestSchema :: IO String
getLatestSchema =
  readProcess "pgroll" ["latest", "schema", "--local", "./migrations"] ""
    <&> filter (not . isControl)

init :: Telemetry -> Config -> IO ()
init telemetry config = do
  rawCall telemetry config ["init"]

start :: Telemetry -> Config -> IO ()
start telemetry config = do
  rawCall telemetry config ["migrate", "./migrations"]

rollback :: Telemetry -> Config -> IO ()
rollback telemetry config = do
  rawCall telemetry config ["rollback"]

startAndComplete :: Telemetry -> Config -> IO ()
startAndComplete telemetry config = do
  rawCall telemetry config ["migrate", "--complete", "./migrations"]
