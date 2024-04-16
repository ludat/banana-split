module Main
    ( main
    ) where

import BananaSplit.Elm (generateElmFiles)
import BananaSplit.Persistence (createTables)

import Conferer qualified

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import Data.Function ((&))
import Data.Pool qualified as Pool

import Database.Selda.Backend (runSeldaT)
import Database.Selda.PostgreSQL (pgOpen', seldaClose)

import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Site.Config (createConfig)
import Site.Server qualified

import System.Posix (Handler (..), installHandler, sigTERM)

import Types

main :: IO ()
main = do
  -- generateElmFiles
  runBackend

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
            & Warp.setPort 8000)

  pool <- Pool.newPool $ Pool.defaultPoolConfig (pgOpen' Nothing connString) seldaClose 60 60
  let appState = App pool

  Pool.withResource pool $ \conn -> runSeldaT createTables conn
  putStrLn $ "Listening on port " ++ show (Warp.getPort settings) ++ " ..."
  Warp.runSettings settings $ logStdoutDev $ Site.Server.app appState
