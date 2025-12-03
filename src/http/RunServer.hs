{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
module RunServer
    ( runBackend
    ) where

import BananaSplit.PgRoll qualified as PgRoll
import BananaSplit.Receipts (ReceiptsReaderConfig(..))

import Conferer qualified

import Data.Pool qualified as Pool
import Data.String (String)
import Data.String.Interpolate (i)

import Database.Beam.Postgres
import Database.PostgreSQL.Simple

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Protolude

import Site.Config (createConfig)
import Site.Server qualified
import Site.Types

import System.Posix (Handler (..), installHandler, sigTERM)

runBackend :: IO ()
runBackend = do
  config <- createConfig "dev"

  connString <- Conferer.fetchFromConfig "database.url" config
  openRouterKey <- Conferer.fetchFromConfig "openrouter.apikey" config
  openRouterModels <- Conferer.fetchFromConfig "openrouter.models" config

  schema <- PgRoll.getLatestSchema

  beamPool <- Pool.newPool $ Pool.defaultPoolConfig (do
      conn <- connectPostgreSQL connString
      _ <- execute conn "SET search_path TO ?" (Only schema)
      pure conn
    ) close 60 60

  _ <- Pool.withResource beamPool $ \conn -> do
    actualSchema <- query @_ @(Only Text) conn "SELECT schema_name FROM information_schema.schemata WHERE schema_name = ?;" (Only schema)
    when (length actualSchema /= 1) $
      throwIO $ MissingPGRollSchema schema

  httpManager <- newManager tlsManagerSettings

  let appState = App
        { beamConnectionPool = beamPool
        , receipts = ReceiptsReaderConfig
          { apiKey = openRouterKey
          , models = openRouterModels
          , manager = httpManager
          }
        }

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

newtype MissingPGRollSchema = MissingPGRollSchema
  { schemaName :: String
  }
  deriving stock (Show)
  deriving anyclass (Exception)
