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
import Network.Wai (Request, rawPathInfo, requestMethod)
import Network.Wai.Handler.Warp (Settings)
import Network.Wai.Handler.Warp qualified as Warp
import Protolude
import System.Posix (Handler (..), installHandler, sigTERM)
import System.IO

import BananaSplit.Persistence qualified as Persistence
import BananaSplit.Receipts (ReceiptsReaderConfig (..))
import Site.Auth (mkSessionKey)
import Site.Config (createConfig)
import Site.Mailer (mkMailer)
import Site.Server qualified
import Site.Types

runBackend :: IO ()
runBackend = do
  -- Containers commonly have no locale configured (LANG unset or "C"/"POSIX"),
  -- which makes GHC default stdout/stderr to an encoding that can't represent
  -- most Unicode text. Printing anything outside that range (e.g. a Greek
  -- receipt echoed into a log line) then throws mid-write instead of
  -- printing, which is what was showing up as blank/truncated log lines
  -- rather than the actual message.
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

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
  fetchedSettings <-
    liftIO $
      Conferer.fetchKey @Settings
        config
        "server"
        ( Warp.defaultSettings
            & Warp.setInstallShutdownHandler shutdownHandler
            & Warp.setPort 8000
        )
  -- Force this regardless of whatever Conferer.FromConfig.Warp derives for
  -- it: unhandled exceptions must always be logged, and that shouldn't be
  -- something that silently depends on config plumbing.
  let settings = Warp.setOnException logWarpException fetchedSettings

  putText [i|Listening on port #{Warp.getPort settings}...|]
  Warp.runSettings settings $ Site.Server.app appState

-- | Log unhandled exceptions Warp catches outside of Servant's own handler
-- dispatch (e.g. while streaming a response, or in a WAI middleware). Site.Server
-- already catches and logs everything that escapes a handler, but this is a
-- second net for whatever falls outside that.
logWarpException :: Maybe Request -> SomeException -> IO ()
logWarpException mRequest e
  | Warp.defaultShouldDisplayException e = do
      let context = maybe "" (\r -> " " <> show (requestMethod r) <> " " <> show (rawPathInfo r)) mRequest
      putText $ "[warp] unhandled exception" <> context <> ": " <> show e
  | otherwise = pure ()
