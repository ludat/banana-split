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
      Elm.generateElmFiles
      RunServer.runBackend
    ["server"] -> do
      RunServer.runBackend
    ["generate"] -> do
      Elm.generateElmFiles
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
