{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A tiny record-of-functions abstraction over "how do we deliver a login
-- token to a user". The concrete implementation is chosen from configuration
-- (Conferer) at startup, so dev can print to the console while prod sends a
-- real email — without any call site knowing which is in use.
module Site.Mailer (
  Mailer (..),
  mkMailer,
  consoleMailer,
) where

import Conferer qualified
import Data.Aeson (object, (.=))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Network.HTTP.Req
import Protolude

-- | The single capability the auth flow needs. Swap the implementation, keep
-- the interface: @sendLoginCode email code@ delivers a login confirmation code
-- to @email@ however this mailer sees fit.
newtype Mailer = Mailer
  { sendLoginCode :: Text -> Text -> IO ()
  }

-- | Pick a mailer based on @mailer.type@ (default: @console@). This is the one
-- place that maps configuration to an implementation.
mkMailer :: Conferer.Config -> IO Mailer
mkMailer config = do
  mailerType <- Conferer.fetchKey @Text config "mailer.type" "console"
  case mailerType of
    "console" -> pure consoleMailer
    "resend" -> do
      apiKey <- Conferer.fetchFromConfig "mailer.resend.apikey" config
      from <- Conferer.fetchFromConfig "mailer.resend.from" config
      pure $ resendMailer apiKey from
    other ->
      panic $ "unknown mailer.type: " <> other <> " (expected 'console' or 'resend')"

-- | Dev mailer: dumps the code to stdout so you can copy it into the app.
consoleMailer :: Mailer
consoleMailer =
  Mailer
    { sendLoginCode = \email code -> do
        putText "┌─ Banana Split login code ──────────────────────────"
        putText $ "│ email: " <> email
        putText $ "│ code:  " <> code
        putText "└────────────────────────────────────────────────────"
    }

-- | Prod mailer: sends the code via the Resend transactional email HTTP API
-- (no new dependency — reuses 'req', same as the receipts integration).
resendMailer :: Text -> Text -> Mailer
resendMailer apiKey from =
  Mailer
    { sendLoginCode = \email code -> do
        let body =
              object
                [ "from" .= from
                , "to" .= [email]
                , "subject" .= ("Tu código de acceso a Banana Split" :: Text)
                , "html" .= loginEmailHtml code
                ]
        runReq defaultHttpConfig $ do
          _ <-
            req
              POST
              (https "api.resend.com" /: "emails")
              (ReqBodyJson body)
              ignoreResponse
              (header "Authorization" ("Bearer " <> Text.encodeUtf8 apiKey))
          pure ()
    }

loginEmailHtml :: Text -> Text
loginEmailHtml code =
  Text.unlines
    [ "<p>Usá este código para iniciar sesión en Banana Split:</p>"
    , "<p><strong style=\"font-size:1.5em;letter-spacing:0.2em\">" <> code <> "</strong></p>"
    , "<p>Vence en 15 minutos.</p>"
    ]
