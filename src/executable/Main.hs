module Main (
  main,
) where

import Protolude

import BananaSplit.Elm qualified as Elm
import BananaSplit.Persistence qualified as Persistence
import BananaSplit.PgRoll qualified as PgRoll
import BananaSplit.Telemetry (withTelemetry)
import RunServer qualified
import Site.Config (createConfig)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      -- Generar los archivos de Elm es trabajo de desarrollo, sin nada que
      -- observar: no vale levantar el SDK para eso.
      Elm.generateElmFiles
      RunServer.runBackend
    ["server"] -> do
      RunServer.runBackend
    ["generate"] -> do
      Elm.generateElmFiles
    -- Los comandos de migración sí van instrumentados: se corren a mano, pueden
    -- tardar, y sin esto lo único que queda de una corrida es lo que haya
    -- quedado en la terminal de quien la ejecutó. 'withTelemetry' los envuelve
    -- porque su shutdown es el que hace el flush final.
    "migrations" : rest -> withTelemetry $ \telemetry -> do
      config <- createConfig "dev"
      PgRoll.rawCall telemetry config rest
    "run-migration" : rest -> withTelemetry $ \telemetry -> do
      config <- createConfig "dev"
      Persistence.runMigration telemetry config rest
    _ -> do
      putText $ "Unknown command: " <> show args
      exitFailure
