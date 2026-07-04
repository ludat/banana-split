{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Auth (
  mkSessionKey,
  issueToken,
  verifyToken,
  generateLoginCode,
  ChallengeFlow (..),
  issueLoginChallenge,
  verifyLoginChallenge,
  renderSessionCookie,
  clearSessionCookie,
  authHandler,
  AuthContext,
) where

import Control.Lens (at, (&), (?~), (^.))
import Control.Monad (guard)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (SHA256, hashWith)
import Crypto.JWT
import Data.Aeson (Value (String), toJSON)
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime, secondsToDiffTime)
import Network.HTTP.Types.Header (hCookie)
import Network.Wai (Request, requestHeaders)
import Servant (AuthProtect, err401, errBody, errHeaders)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import System.Random (randomRIO)
import Web.Cookie (
  SetCookie,
  defaultSetCookie,
  parseCookies,
  renderSetCookie,
  sameSiteLax,
  setCookieHttpOnly,
  setCookieMaxAge,
  setCookieName,
  setCookiePath,
  setCookieSameSite,
  setCookieSecure,
  setCookieValue,
 )

import BananaSplit
import Preludat
import Site.Types (App (..))

-- | The servant Context entries needed to serve @AuthProtect "session"@ routes.
type AuthContext = '[AuthHandler Request User]

type instance AuthServerData (AuthProtect "session") = User

-- | Build the symmetric signing key from the configured secret.
mkSessionKey :: Text -> JWK
mkSessionKey secret = fromOctets (encodeUtf8 secret)

sessionDurationSeconds :: NominalDiffTime
sessionDurationSeconds = 90 * 24 * 60 * 60

sessionCookieName :: ByteString
sessionCookieName = "session"

-- | How long a login challenge stays valid after the code is emailed.
loginChallengeDurationSeconds :: NominalDiffTime
loginChallengeDurationSeconds = 15 * 60

-- | Marks a JWT as a login challenge, so a session token can never be replayed
-- as a challenge (or vice versa).
loginPurpose :: Value
loginPurpose = String "login-challenge"

-- | Which flow a challenge belongs to. Signup carries the display name the
-- caller typed so it survives the (stateless) round-trip to the verify step;
-- signin needs nothing beyond the email already in the challenge.
data ChallengeFlow
  = SignupFlow Text
  | SigninFlow
  deriving (Show, Eq)

-- | A fresh 6-digit login code — short enough to read out over the phone or
-- type on a device that can't open a link.
generateLoginCode :: IO Text
generateLoginCode = do
  n <- randomRIO (0, 999999 :: Int)
  pure $ Text.justifyRight 6 '0' (show n)

-- | One-way commitment to a code, bound to the email and peppered with the
-- server secret. Because the pepper never leaves the server, someone who reads
-- the challenge JWT still cannot brute-force the 6-digit code offline — the
-- only avenue is submitting guesses, which the short expiry bounds.
codeCommitment :: ByteString -> Text -> Text -> Text
codeCommitment pepper email code =
  decodeUtf8
    $ convertToBase Base16
    $ hashWith SHA256
    $ pepper
    <> ":"
    <> encodeUtf8 email
    <> ":"
    <> encodeUtf8 code

-- | Sign a short-lived challenge for @email@ that commits to @code@ without
-- containing it. The client keeps the returned challenge and later presents it
-- together with the code (which arrived out-of-band by email) to log in. The
-- flow (and, for signup, the display name) rides inside the signed challenge so
-- it can't be tampered with between the two steps.
issueLoginChallenge :: JWK -> ByteString -> ChallengeFlow -> Text -> Text -> IO (Either JWTError Text)
issueLoginChallenge key pepper flow email code = do
  now <- getCurrentTime
  let withFlow =
        case flow of
          SigninFlow -> addClaim "flow" (String "signin")
          SignupFlow nombre ->
            addClaim "flow" (String "signup") . addClaim "nombre" (String nombre)
  let claims =
        emptyClaimsSet
          & claimIat ?~ NumericDate now
          & claimExp ?~ NumericDate (addUTCTime loginChallengeDurationSeconds now)
          & addClaim "purpose" loginPurpose
          & addClaim "email" (String email)
          & addClaim "code" (String (codeCommitment pepper email code))
          & withFlow
  signed <-
    runJOSE
      $ signClaims key (newJWSHeader (RequiredProtection, HS256)) claims
  pure $ fmap (decodeUtf8 . BSL.toStrict . encodeCompact) (signed :: Either JWTError SignedJWT)

-- | Check a challenge + submitted code and recover the flow + email if they
-- match. Returns 'Nothing' on a bad signature, expiry, wrong purpose, unknown
-- flow, or wrong code.
verifyLoginChallenge :: JWK -> ByteString -> Text -> Text -> IO (Maybe (ChallengeFlow, Text))
verifyLoginChallenge key pepper challenge code = do
  result <-
    runJOSE $ do
      jwt <- decodeCompact (BSL.fromStrict (encodeUtf8 challenge))
      verifyClaims (defaultJWTValidationSettings (const True)) key (jwt :: SignedJWT)
  pure $ case result :: Either JWTError ClaimsSet of
    Left _ -> Nothing
    Right claims -> do
      guard (claims ^. unregisteredClaims . at "purpose" == Just loginPurpose)
      email <- stringClaim claims "email"
      expected <- stringClaim claims "code"
      -- Constant-time so a wrong code leaks nothing through timing.
      guard (constEq (encodeUtf8 expected) (encodeUtf8 (codeCommitment pepper email code)))
      flow <- case stringClaim claims "flow" of
        Just "signin" -> Just SigninFlow
        Just "signup" -> SignupFlow <$> stringClaim claims "nombre"
        _ -> Nothing
      pure (flow, email)

stringClaim :: ClaimsSet -> Text -> Maybe Text
stringClaim claims name =
  case claims ^. unregisteredClaims . at name of
    Just (String t) -> Just t
    _ -> Nothing

-- | Sign a session token carrying the user's identity. Written so a
-- verification step could be inserted later without changing the shape.
issueToken :: JWK -> User -> IO (Either JWTError Text)
issueToken key user = do
  now <- getCurrentTime
  let claims =
        emptyClaimsSet
          & claimIat ?~ NumericDate now
          & claimExp ?~ NumericDate (addUTCTime sessionDurationSeconds now)
          & addClaim "uid" (toJSON (show user.id :: Text))
          & addClaim "email" (String user.email)
          & addClaim "nombre" (String user.nombre)
  signed <-
    runJOSE
      $ signClaims key (newJWSHeader (RequiredProtection, HS256)) claims
  pure $ fmap (decodeUtf8 . BSL.toStrict . encodeCompact) (signed :: Either JWTError SignedJWT)

-- | Verify a token's signature and expiry and rebuild the 'User' from its
-- claims. No database access — identity comes entirely from the token.
verifyToken :: JWK -> ByteString -> IO (Maybe User)
verifyToken key raw = do
  result <-
    runJOSE $ do
      jwt <- decodeCompact (BSL.fromStrict raw)
      verifyClaims (defaultJWTValidationSettings (const True)) key (jwt :: SignedJWT)
  pure $ case result :: Either JWTError ClaimsSet of
    Left _ -> Nothing
    Right claims -> claimsToUser claims

claimsToUser :: ClaimsSet -> Maybe User
claimsToUser claims = do
  uidText <- textClaim "uid"
  email <- textClaim "email"
  nombre <- textClaim "nombre"
  uid <- readMaybe (toS uidText :: [Char])
  pure User{id = uid, email = email, nombre = nombre}
  where
    textClaim name =
      case claims ^. unregisteredClaims . at name of
        Just (String t) -> Just t
        _ -> Nothing

-- | @Set-Cookie@ value that stores the session token.
renderSessionCookie :: Bool -> Text -> Text
renderSessionCookie secure token =
  renderCookie
    $ (baseCookie secure)
      { setCookieValue = encodeUtf8 token
      , setCookieMaxAge = Just (secondsToDiffTime (round sessionDurationSeconds))
      }

-- | @Set-Cookie@ value that immediately expires the session (logout).
clearSessionCookie :: Bool -> Text
clearSessionCookie secure =
  renderCookie
    $ (baseCookie secure)
      { setCookieValue = ""
      , setCookieMaxAge = Just 0
      }

baseCookie :: Bool -> SetCookie
baseCookie secure =
  defaultSetCookie
    { setCookieName = sessionCookieName
    , setCookiePath = Just "/"
    , setCookieHttpOnly = True
    , setCookieSameSite = Just sameSiteLax
    , setCookieSecure = secure
    }

renderCookie :: SetCookie -> Text
renderCookie = decodeUtf8 . BSL.toStrict . toLazyByteString . renderSetCookie

sessionTokenFromRequest :: Request -> Maybe ByteString
sessionTokenFromRequest req = do
  cookieHeader <- List.lookup hCookie (requestHeaders req)
  List.lookup sessionCookieName (parseCookies cookieHeader)

-- | Servant auth handler for @AuthProtect "session"@: pull the session cookie,
-- verify it, and hand the 'User' to the protected handler (401 otherwise).
authHandler :: App -> AuthHandler Request User
authHandler app = mkAuthHandler $ \req ->
  case sessionTokenFromRequest req of
    Nothing -> throwError unauthorized
    Just token -> do
      mUser <- liftIO $ verifyToken app.jwk token
      case mUser of
        Nothing -> throwError unauthorized
        Just user -> pure user
  where
    unauthorized =
      err401
        { errBody = "{\"error\":\"not authenticated\"}"
        , errHeaders = [("Content-Type", "application/json")]
        }
