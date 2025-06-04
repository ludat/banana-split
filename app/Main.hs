{-# LANGUAGE QuasiQuotes #-}
module Main
    ( main
    ) where

import BananaSplit.Elm (generateElmFiles)

import Conferer qualified

import Data.Pool qualified as Pool
import Data.String (fromString)
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
import System.Process (readProcess)

import Types

main :: IO ()
main = do
  generateElmFiles
  runBackend

runBackend :: IO ()
runBackend = do
  config <- createConfig

  connString <- liftIO $ Conferer.fetchFromConfig "database.url" config


  setSearchPath <- readProcess "reshape" ["schema-query"] ""

  beamPool <- Pool.newPool $ Pool.defaultPoolConfig (do
      conn <- connectPostgreSQL connString
      _ <- execute_ conn (fromString setSearchPath)
      pure conn
    ) close 60 60

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
