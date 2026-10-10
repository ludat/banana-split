module Main (
  main,
) where

import Protolude

import AppRunner (runApp)
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
    --
    -- La conexión se abre y se cierra acá: es el único lado que tiene por qué saber
    -- que hay una. Los comandos piden las capacidades que usan y 'AppRunner' es
    -- quien las cumple.
    "migrations" : rest -> do
      config <- createConfig "dev"
      withTelemetry config $ \telemetry ->
        Persistence.conUnaConexion config $ \conn ->
          runApp telemetry conn $ PgRoll.rawCall config rest
    "run-migration" : rest -> do
      config <- createConfig "dev"
      withTelemetry config $ \telemetry ->
        Persistence.conUnaConexion config $ \conn ->
          runApp telemetry conn $ Persistence.runMigration rest
    _ -> do
      putText $ "Unknown command: " <> show args
      exitFailure
