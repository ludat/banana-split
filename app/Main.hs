{-# LANGUAGE QuasiQuotes #-}
module Main
    ( main
    ) where

import BananaSplit.Elm (generateElmFiles)

import Conferer qualified

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Function ((&))
import Data.Pool qualified as Pool

import Database.Selda.Backend (runSeldaT, SeldaBackend (closeConnection))
import Database.Selda.PostgreSQL (pgOpen', seldaClose)

import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Site.Config (createConfig)
import Site.Server qualified

import System.Posix (Handler (..), installHandler, sigTERM)
import System.Process (readProcess)

import Types
import Database.Selda.Unsafe (rawStm)
import Data.String (fromString)
import Data.String.Interpolate (i)

main :: IO ()
main = do
  -- generateElmFiles
  runBackend

runBackend :: IO ()
runBackend = do
  config <- createConfig

  connString <- liftIO $ Conferer.fetchFromConfig "database.url" config


  setSearchPath <- readProcess "reshape" ["schema-query"] ""

  pool <- Pool.newPool $ Pool.defaultPoolConfig (do
      conn <- pgOpen' Nothing connString
      runSeldaT (rawStm $ fromString setSearchPath) conn
      pure conn
    ) seldaClose 60 60
  let appState = App pool

  let shutdownAction = do
        Pool.destroyAllResources pool
  let shutdownHandler closeSocket = void $ installHandler sigTERM (Catch $ shutdownAction >> closeSocket) Nothing
  settings <-
    liftIO $
      Conferer.fetchKey @Settings
        config
        "server"
        ( Warp.defaultSettings
            & Warp.setInstallShutdownHandler shutdownHandler
            & Warp.setPort 8000)

  putStrLn [i|Listening on port #{Warp.getPort settings}...|]
  Warp.runSettings settings $ logStdoutDev $ Site.Server.app appState
