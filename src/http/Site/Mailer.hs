{-# LANGUAGE OverloadedStrings #-}

-- | A tiny record-of-functions abstraction over "how do we deliver a login
-- token to a user". The concrete implementation is chosen from configuration
-- (Conferer) at startup, so dev can print to the console while prod sends a
-- real email — without any call site knowing which is in use.
module Site.Mailer (
  Mailer (..),
  SmtpSettings (..),
  SmtpSecurity (..),
  mkMailer,
  consoleMailer,
  smtpMailer,
) where

import Conferer qualified
import Data.Text qualified as Text
import Network.Mail.Mime (Mail (..))
import Network.Mail.Mime qualified as Mime
import Network.Mail.SMTP (Address (..))
import Network.Mail.SMTP qualified as SMTP
import Protolude

import BananaSplit.Email (Email, unEmail)

newtype Mailer = Mailer
  { sendLoginCode :: Email -> Text -> IO ()
  }

data SmtpSettings = SmtpSettings
  { host :: Text
  , port :: Int
  , security :: SmtpSecurity
  , credentials :: Maybe (Text, Text)
  -- ^ user and password, or 'Nothing' for unauthenticated relays
  , from :: Address
  }

-- | How to secure the connection: implicit TLS (usually port 465),
-- STARTTLS upgrade (usually port 587) or no encryption at all (local
-- relays, mailpit and friends).
data SmtpSecurity = SmtpTls | SmtpStartTls | SmtpPlain

mkMailer :: Conferer.Config -> IO Mailer
mkMailer config = do
  mailerType <- Conferer.fetchFromConfig @Text "mailer.type" config
  case mailerType of
    "console" -> pure consoleMailer
    "smtp" -> do
      host <- Conferer.fetchFromConfig "mailer.smtp.host" config
      port <- Conferer.fetchFromConfig @Int "mailer.smtp.port" config
      securityText <- Conferer.fetchFromConfig @Text "mailer.smtp.security" config
      security <- case securityText of
        "tls" -> pure SmtpTls
        "starttls" -> pure SmtpStartTls
        "plain" -> pure SmtpPlain
        other ->
          panic $ "unknown mailer.smtp.security: " <> other <> " (expected 'tls', 'starttls' or 'plain')"
      user <- Conferer.fetchFromConfig @(Maybe Text) "mailer.smtp.user" config
      password <- Conferer.fetchFromConfig @(Maybe Text) "mailer.smtp.password" config
      credentials <- case (user, password) of
        (Just u, Just p) -> pure $ Just (u, p)
        (Nothing, Nothing) -> pure Nothing
        _ ->
          panic "mailer.smtp.user and mailer.smtp.password must be set together (or not at all)"
      fromAddress <- Conferer.fetchFromConfig "mailer.smtp.from" config
      fromName <- Conferer.fetchFromConfig @(Maybe Text) "mailer.smtp.fromname" config
      pure $
        smtpMailer
          SmtpSettings
            { host = host
            , port = port
            , security = security
            , credentials = credentials
            , from = Address fromName fromAddress
            }
    other ->
      panic $ "unknown mailer.type: " <> other <> " (expected 'console' or 'smtp')"

-- | Dev mailer: dumps the code to stdout so you can copy it into the app.
consoleMailer :: Mailer
consoleMailer =
  Mailer
    { sendLoginCode = \email code -> do
        putText $
          unlines
            [ "┌─ Banana Split login code ──────────────────────────"
            , "│ email: " <> unEmail email
            , "│ code:  " <> code
            , "└────────────────────────────────────────────────────"
            ]
    }

-- | Prod mailer: sends the code over plain SMTP, so it works with any
-- provider (or a self-hosted relay) instead of being tied to one
-- transactional email API.
smtpMailer :: SmtpSettings -> Mailer
smtpMailer settings =
  Mailer
    { sendLoginCode = \email code ->
        deliver settings $
          SMTP.simpleMail
            settings.from
            [Address Nothing (unEmail email)]
            []
            []
            "Tu código de acceso a Banana Split"
            [Mime.htmlPart $ toS $ loginEmailHtml code]
    }

deliver :: SmtpSettings -> Mail -> IO ()
deliver settings =
  let host = Text.unpack settings.host
      port = fromIntegral settings.port
  in case (settings.security, settings.credentials) of
       (SmtpTls, Just (user, password)) ->
         SMTP.sendMailWithLoginTLS' host port (Text.unpack user) (Text.unpack password)
       (SmtpTls, Nothing) ->
         SMTP.sendMailTLS' host port
       (SmtpStartTls, Just (user, password)) ->
         SMTP.sendMailWithLoginSTARTTLS' host port (Text.unpack user) (Text.unpack password)
       (SmtpStartTls, Nothing) ->
         SMTP.sendMailSTARTTLS' host port
       (SmtpPlain, Just (user, password)) ->
         SMTP.sendMailWithLogin' host port (Text.unpack user) (Text.unpack password)
       (SmtpPlain, Nothing) ->
         SMTP.sendMail' host port

loginEmailHtml :: Text -> Text
loginEmailHtml code =
  Text.unlines
    [ "<p>Usá este código para iniciar sesión en Banana Split:</p>"
    , "<p><strong style=\"font-size:1.5em;letter-spacing:0.2em\">" <> code <> "</strong></p>"
    , "<p>Vence en 5 minutos.</p>"
    ]
