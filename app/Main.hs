{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conferer qualified

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Function ((&))

import Database.PostgreSQL.Simple qualified as PG

import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Site.Api qualified
import Site.Config (createConfig)

import System.Posix (Handler (..), installHandler, sigTERM)

import Types
import Database.Selda.PostgreSQL (pgOpen')
import Debug.Trace
import Database.Selda.Backend (runSeldaT)
import RompePiernas.Persistence (createTables)

main :: IO ()
main = runBackend

runBackend :: IO ()
runBackend = do
  config <- createConfig

  connString <- liftIO $ Conferer.fetchFromConfig "database.url" config

  let shutdownAction = pure ()
  let shutdownHandler closeSocket = void $ installHandler sigTERM (Catch $ shutdownAction >> closeSocket) Nothing
  settings <-
    liftIO $
      Conferer.fetchKey @Settings
        config
        "server"
        ( Warp.defaultSettings
            & Warp.setInstallShutdownHandler shutdownHandler
            & Warp.setPort 8000
        )

  conn <- pgOpen' Nothing $ traceShowId connString
  let appState = App conn

  runSeldaT createTables conn
  putStrLn $ "Listening on port " ++ show (Warp.getPort settings) ++ " ..."
  Warp.runSettings settings $ logStdoutDev $ Site.Api.app appState
