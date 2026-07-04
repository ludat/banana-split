{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Handler.Auth (
  handleSignup,
  handleSignin,
  handleVerify,
  handleLogout,
  handleMe,
) where

import Data.Text qualified as Text
import Servant

import BananaSplit
import BananaSplit.Persistence (createUser, fetchUserByEmail, fetchUserById)
import Preludat
import Site.Api (LoginChallenge (..), SigninParams (..), SignupParams (..), VerifyParams (..))
import Site.Auth (ChallengeFlow (..), clearSessionCookie, generateLoginCode, issueLoginChallenge, issueToken, renderSessionCookie, verifyLoginChallenge)
import Site.Handler.Utils (runBeam, throwJsonError)
import Site.Mailer (Mailer (..))
import Site.Types

-- | Signing a session token can only fail because of a misconfiguration (e.g.
-- an @auth.jwtsecret@ shorter than the 32 bytes HS256 requires), never because
-- of client input. So it's a real exception, not a handled 4xx/5xx.
newtype SessionTokenError = SessionTokenError Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Step 1 of signup: a new account. We reject an email that already exists up
-- front (before emailing anything) so the user gets immediate, useful feedback
-- instead of only finding out after entering a code. The account itself is not
-- created until the code is verified.
handleSignup :: SignupParams -> AppHandler LoginChallenge
handleSignup params = do
  let email = normalizeEmail params.email
  let nombre = Text.strip params.nombre
  when (Text.null nombre) $
    throwJsonError err400 "Ingresá tu nombre"
  existing <- runBeam $ fetchUserByEmail email
  for_ existing $ \_ ->
    throwJsonError err409 "Ya existe una cuenta con ese email. Iniciá sesión."
  issueChallenge (SignupFlow nombre) email

-- | Step 1 of signin: an existing account. We reject an unknown email up front
-- (symmetric with signup) so we never email a code for an account that can't
-- be logged into.
handleSignin :: SigninParams -> AppHandler LoginChallenge
handleSignin params = do
  let email = normalizeEmail params.email
  existing <- runBeam $ fetchUserByEmail email
  when (isNothing existing) $
    throwJsonError err404 "No encontramos una cuenta con ese email. Registrate."
  issueChallenge SigninFlow email

-- | Shared tail of both step-1 handlers: mint a code, sign a challenge that
-- carries the flow, and email the code.
issueChallenge :: ChallengeFlow -> Text -> AppHandler LoginChallenge
issueChallenge flow email = do
  key <- asks (.jwk)
  pepper <- asks (.authPepper)
  mailer <- asks (.mailer)
  code <- liftIO generateLoginCode
  eChallenge <- liftIO $ issueLoginChallenge key pepper flow email code
  case eChallenge of
    Left err ->
      liftIO $ throwIO $ SessionTokenError $ "could not sign login challenge: " <> show err
    Right challenge -> do
      liftIO $ mailer.sendLoginCode email code
      pure $ LoginChallenge challenge

-- | Step 2 of both flows: exchange a challenge + its emailed code for a
-- session. The flow rides inside the challenge, so this is where a signup
-- actually creates the account and a signin actually looks one up — re-checking
-- existence to close the gap since step 1.
handleVerify ::
  VerifyParams
  -> AppHandler (Headers '[Header "Set-Cookie" Text] User)
handleVerify params = do
  key <- asks (.jwk)
  pepper <- asks (.authPepper)
  secure <- asks (.cookieSecure)
  mResult <- liftIO $ verifyLoginChallenge key pepper params.challenge params.code
  (flow, email) <- maybe (throwJsonError err401 "Código inválido o vencido") pure mResult
  user <- case flow of
    SigninFlow -> do
      existing <- runBeam $ fetchUserByEmail email
      maybe (throwJsonError err404 "No encontramos una cuenta con ese email") pure existing
    SignupFlow nombre -> do
      existing <- runBeam $ fetchUserByEmail email
      case existing of
        Just _ -> throwJsonError err409 "Ya existe una cuenta con ese email. Iniciá sesión."
        Nothing -> runBeam $ createUser email nombre
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
