{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
module Main
    ( main
    ) where

import BananaSplit.Elm (generateElmFiles)

import Conferer qualified

import Data.Pool qualified as Pool
import Data.String (String)
import Data.String.Interpolate (i)

import Database.Beam.Postgres
import Database.PostgreSQL.Simple

import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Protolude

import Site.Config (createConfig)
import Site.Server qualified

import System.Posix (Handler (..), installHandler, sigTERM)
import System.Process (callProcess, readProcess, showCommandForUser)

import Types

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      generateElmFiles
      runBackend
    ["server"] -> do
      runBackend
    "migrations":rest -> do
      runMigrations rest
    _ -> do
      putText $ "Unknown command: " <> show args
      exitFailure

runMigrations :: [String] -> IO ()
runMigrations args = do
  config <- createConfig
  connString <- Conferer.fetchFromConfig "database.url" config
  let connectionArgs = ["--postgres-url", connString ++ "?sslmode=disable"]
  callProcess "pgroll" $ connectionArgs ++ args

runBackend :: IO ()
runBackend = do
  config <- createConfig

  connString <- Conferer.fetchFromConfig "database.url" config

  schema <- readProcess "pgroll" ["latest", "schema", "--local", "./migrations"] ""
    <&> filter (not . isControl)

  beamPool <- Pool.newPool $ Pool.defaultPoolConfig (do
      conn <- connectPostgreSQL connString
      _ <- execute conn "SET search_path TO ?" (Only schema)
      pure conn
    ) close 60 60

  _ <- Pool.withResource beamPool $ \conn -> do
    actualSchema <- query @_ @(Only Text) conn "SELECT schema_name FROM information_schema.schemata WHERE schema_name = ?;" (Only schema)
    when (length actualSchema /= 1) $
      throwIO $ MissingPGRollSchema schema

  let appState = App beamPool

  let shutdownAction = do
        Pool.destroyAllResources beamPool
  let shutdownHandler closeSocket = void $ installHandler sigTERM (Catch $ shutdownAction >> closeSocket) Nothing
  settings <-
    liftIO $
      Conferer.fetchKey @Settings
        config
        "server"
        ( Warp.defaultSettings
            & Warp.setInstallShutdownHandler shutdownHandler
            & Warp.setPort 8000)

  putText [i|Listening on port #{Warp.getPort settings}...|]
  Warp.runSettings settings $ logStdoutDev $ Site.Server.app appState

data MissingPGRollMigrations =
  MissingPGRollMigrations
  deriving (Exception, Show)

newtype MissingPGRollSchema = MissingPGRollSchema
  { schemaName :: String
  }
  deriving stock (Show)
  deriving anyclass (Exception)
