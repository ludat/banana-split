module Main (
  main,
) where

import Protolude

import BananaSplit.Elm qualified as Elm
import BananaSplit.Persistence qualified as Persistence
import BananaSplit.PgRoll qualified as PgRoll
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
      PgRoll.rawCall config rest
    "run-migration" : rest -> do
      config <- createConfig "dev"
      Persistence.runMigration config rest
    _ -> do
      putText $ "Unknown command: " <> show args
      exitFailure
