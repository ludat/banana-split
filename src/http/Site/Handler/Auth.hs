{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Handler.Auth (
  handleLogin,
  handleVerify,
  handleLogout,
  handleMe,
) where

import Servant

import BananaSplit
import BananaSplit.Persistence (fetchUserById, findOrCreateUser)
import Preludat
import Site.Api (LoginChallenge (..), LoginParams (..), VerifyParams (..))
import Site.Auth (clearSessionCookie, generateLoginCode, issueLoginChallenge, issueToken, renderSessionCookie, verifyLoginChallenge)
import Site.Handler.Utils (runBeam, throwJsonError)
import Site.Mailer (Mailer (..))
import Site.Types

-- | Signing a session token can only fail because of a misconfiguration (e.g.
-- an @auth.jwtsecret@ shorter than the 32 bytes HS256 requires), never because
-- of client input. So it's a real exception, not a handled 4xx/5xx.
newtype SessionTokenError = SessionTokenError Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Step 1 of login: email the caller a short 6-digit code and hand back a
-- signed challenge that commits to it. We do not create the account here and
-- never reveal whether the email exists — the account is created only once the
-- code is verified.
handleLogin :: LoginParams -> AppHandler LoginChallenge
handleLogin params = do
  let email = normalizeEmail params.email
  key <- asks (.jwk)
  pepper <- asks (.authPepper)
  mailer <- asks (.mailer)
  code <- liftIO generateLoginCode
  eChallenge <- liftIO $ issueLoginChallenge key pepper email code
  case eChallenge of
    Left err ->
      liftIO $ throwIO $ SessionTokenError $ "could not sign login challenge: " <> show err
    Right challenge -> do
      liftIO $ mailer.sendLoginCode email code
      pure $ LoginChallenge challenge

-- | Step 2 of login: exchange a challenge + its emailed code for a session.
-- This is where find-or-create happens (so unverified emails never create
-- accounts), along with the user's global participante — both in one
-- transaction, since a half-created account has no participante and wouldn't
-- appear in settlements.
handleVerify ::
  VerifyParams
  -> AppHandler (Headers '[Header "Set-Cookie" Text] User)
handleVerify params = do
  key <- asks (.jwk)
  pepper <- asks (.authPepper)
  secure <- asks (.cookieSecure)
  mEmail <- liftIO $ verifyLoginChallenge key pepper params.challenge params.code
  email <- maybe (throwJsonError err401 "invalid or expired code") pure mEmail
  user <- runBeam $ do
    u <- findOrCreateUser email
    pure u
  eToken <- liftIO $ issueToken key user
  case eToken of
    Left err ->
      liftIO $ throwIO $ SessionTokenError $ "could not sign session token: " <> show err
    Right token ->
      pure $ addHeader (renderSessionCookie secure token) user

-- | Logout just expires the cookie client-side; JWTs are stateless.
handleLogout :: AppHandler (Headers '[Header "Set-Cookie" Text] Text)
handleLogout = do
  secure <- asks (.cookieSecure)
  pure $ addHeader (clearSessionCookie secure) "ok"

-- | Current user. Reads the DB so the UI stays fresh even if the token's
-- claims have gone stale (e.g. after a future rename).
handleMe :: User -> AppHandler User
handleMe sessionUser = do
  fresh <- runBeam $ fetchUserById sessionUser.id
  case fresh of
    Nothing -> throwJsonError err401 "user not found"
    Just user -> pure user
