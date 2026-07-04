{-# LANGUAGE QuasiQuotes #-}

module RunServer (
  runBackend,
) where

import Conferer qualified
import Conferer.FromConfig.Warp ()
import Data.Pool qualified as Pool
import Data.String.Interpolate (i)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Protolude
import System.Posix (Handler (..), installHandler, sigTERM)

import BananaSplit.Persistence qualified as Persistence
import BananaSplit.Receipts (ReceiptsReaderConfig (..))
import Site.Auth (mkSessionKey)
import Site.Config (createConfig)
import Site.Mailer (mkMailer)
import Site.Server qualified
import Site.Types

runBackend :: IO ()
runBackend = do
  config <- createConfig "dev"

  openRouterKey <- Conferer.fetchFromConfig "openrouter.apikey" config
  openRouterModels <- Conferer.fetchFromConfig "openrouter.models" config

  jwtSecret <- Conferer.fetchFromConfig "auth.jwtsecret" config
  cookieSecure' <- Conferer.fetchFromConfig "auth.securecookie" config

  beamPool <- Persistence.makePool config

  httpManager <- newManager tlsManagerSettings

  mailer <- mkMailer config

  let appState =
        App
          { beamConnectionPool = beamPool
          , receipts =
              ReceiptsReaderConfig
                { apiKey = openRouterKey
                , models = openRouterModels
                , manager = httpManager
                }
          , jwk = mkSessionKey jwtSecret
          , authPepper = encodeUtf8 jwtSecret
          , cookieSecure = cookieSecure'
          , mailer = mailer
          }

  let shutdownAction = Pool.destroyAllResources beamPool
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

  putText [i|Listening on port #{Warp.getPort settings}...|]
  Warp.runSettings settings $ logStdoutDev $ Site.Server.app appState
