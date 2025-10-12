module Main
    ( main
    ) where

import BananaSplit.Elm qualified as Elm
import BananaSplit.PgRoll qualified as PgRoll

import Protolude

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
    "migrations":rest -> do
      config <- createConfig "dev"
      PgRoll.rawCall config rest
    _ -> do
      putText $ "Unknown command: " <> show args
      exitFailure
