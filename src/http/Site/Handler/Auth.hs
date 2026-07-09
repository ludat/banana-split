{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Handler.Auth (
  handleRequestCode,
  handleVerify,
  handleRegister,
  handleLogout,
  handleMe,
  handleUpdateMe,
) where

import Data.Text qualified as Text
import Servant

import BananaSplit
import BananaSplit.Persistence (LoginEvent (..), clearAttempts, countRecentAttempts, createUser, fetchUserByEmail, fetchUserById, recordAttempt, updateUser)
import Preludat
import Site.Api (LoginChallenge (..), RegisterParams (..), RequestCodeParams (..), UpdateMeParams (..), VerifyParams (..), VerifyResult (..))
import Site.Auth (ChallengePayload (..), checkChallengeCode, clearSessionCookie, generateLoginCode, issueLoginChallenge, issueRegistrationToken, issueToken, openChallenge, renderSessionCookie, verifyRegistrationToken)
import Site.Handler.Utils (runBeam, throwJsonError)
import Site.Mailer (Mailer (..))
import Site.Types

newtype SessionTokenError = SessionTokenError Text
  deriving stock (Show)
  deriving anyclass (Exception)

-- | How many login events (codes emailed + wrong codes submitted, counted
-- together) an email may pile up within the window before both @request-code@
-- and @verify@ start rejecting with 429 — one budget covering both code
-- brute-forcing and email-bombing (see 'countRecentAttempts' for the window).
maxLoginAttempts :: Int
maxLoginAttempts = 10

-- | Step 1: email a login code. Deliberately does /not/ look at the database, so
-- it can't reveal whether an account exists — that is disclosed only at verify
-- time, and only to the proven owner of the address.
handleRequestCode :: RequestCodeParams -> AppHandler LoginChallenge
handleRequestCode params = do
  let email = normalizeEmail params.email
  key <- asks (.jwk)
  pepper <- asks (.authPepper)
  mailer <- asks (.mailer)
  -- Throttle login events for this address, so the endpoint can't be used to
  -- email-bomb a victim or run up SMTP costs.
  attempts <- runBeam $ countRecentAttempts email
  when (attempts >= maxLoginAttempts)
    $ throwJsonError err429 "Pediste demasiados códigos. Esperá unos minutos y volvé a intentar."
  code <- liftIO generateLoginCode
  challenge <-
    liftIO
      $ issueLoginChallenge key pepper email code
      `orElse` (throwIO . SessionTokenError . ("Could not sign login challenge: " <>) . show)
  runBeam $ recordAttempt email CodeSent
  liftIO $ mailer.sendLoginCode email code
  pure $ LoginChallenge challenge

-- | Step 2: prove ownership of the challenge's email with the code, then branch
-- on whether an account exists — log in, or hand back a registration token for a
-- new one. The rate-limit check sits between signature recovery and the
-- constant-time code check, so the 6-digit code can't be brute-forced.
handleVerify :: VerifyParams -> AppHandler (Headers '[Header "Set-Cookie" Text] VerifyResult)
handleVerify params = do
  key <- asks (.jwk)
  pepper <- asks (.authPepper)
  secure <- asks (.cookieSecure)
  payload <-
    liftIO (openChallenge key params.challenge)
      `orElseMay` throwJsonError err401 "Código inválido o vencido"
  let email = payload.email
  attempts <- runBeam $ countRecentAttempts email
  when (attempts >= maxLoginAttempts)
    $ throwJsonError err429 "Demasiados intentos. Esperá unos minutos y volvé a intentar."
  unless (checkChallengeCode pepper payload params.code) $ do
    runBeam $ recordAttempt email VerifyFailure
    throwJsonError err401 "Código inválido o vencido"
  -- Ownership proven: drop this email's attempts so a legit fumble doesn't count.
  runBeam $ clearAttempts email
  existing <- runBeam $ fetchUserByEmail email
  case existing of
    Just user -> do
      token <-
        liftIO
          $ issueToken key user
          `orElse` (throwIO . SessionTokenError . ("Could not sign session token: " <>) . show)
      pure $ addHeader (renderSessionCookie secure token) (VerifyLoggedIn user)
    Nothing -> do
      regToken <-
        liftIO
          $ issueRegistrationToken key email
          `orElse` (throwIO . SessionTokenError . ("Could not sign registration token: " <>) . show)
      pure $ noHeader (VerifyNeedsRegistration regToken)

-- | Step 3 (new accounts only): exchange the registration token + a chosen
-- display name for a created account and a session. Identity comes from the
-- (verified) token, not the request body.
handleRegister :: RegisterParams -> AppHandler (Headers '[Header "Set-Cookie" Text] User)
handleRegister params = do
  key <- asks (.jwk)
  secure <- asks (.cookieSecure)
  let nombre = Text.strip params.nombre
  when (Text.null nombre)
    $ throwJsonError err400 "Ingresá tu nombre"
  email <-
    liftIO (verifyRegistrationToken key params.registrationToken)
      `orElseMay` throwJsonError err401 "Tu registro venció. Volvé a empezar."
  existing <- runBeam $ fetchUserByEmail email
  when (isJust existing)
    $ throwJsonError err409 "Ya existe una cuenta con ese email. Iniciá sesión."
  user <- runBeam $ createUser email nombre
  token <-
    liftIO
      $ issueToken key user
      `orElse` (throwIO . SessionTokenError . ("Could not sign session token: " <>) . show)
  pure $ addHeader (renderSessionCookie secure token) user

handleLogout :: AppHandler (Headers '[Header "Set-Cookie" Text] Text)
handleLogout = do
  secure <- asks (.cookieSecure)
  pure $ addHeader (clearSessionCookie secure) "ok"

handleMe :: User -> AppHandler User
handleMe sessionUser = do
  runBeam (fetchUserById sessionUser.id)
    `orElseMay` throwJsonError err401 "user not found"

handleUpdateMe :: User -> UpdateMeParams -> AppHandler User
handleUpdateMe sessionUser params = do
  let nombre = Text.strip params.nombre
  when (Text.null nombre)
    $ throwJsonError err400 "Ingresá tu nombre"
  runBeam $ updateUser sessionUser.id nombre
