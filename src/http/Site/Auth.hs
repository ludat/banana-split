{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Auth (
  AuthContext,
  ChallengePayload (..),
  Session (..),
  authHandler,
  checkChallengeCode,
  clearSessionCookie,
  generateLoginCode,
  issueLoginChallenge,
  issueRegistrationToken,
  issueToken,
  mkSessionKey,
  openChallenge,
  sessionAuthHandler,
  renderSessionCookie,
  sessionDurationSeconds,
  sessionRefreshThreshold,
  shouldRefreshSession,
  verifyLoginChallenge,
  verifyRegistrationToken,
  verifySession,
  verifyToken,
) where

import Control.Lens ((.~), (?~), (^.))
import Crypto.JWT
import Crypto.Number.Generate (generateMax)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object, String), withObject, (.:))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (
  NominalDiffTime,
  UTCTime,
  addUTCTime,
  diffUTCTime,
  getCurrentTime,
  secondsToDiffTime,
 )
import Network.HTTP.Types.Header (hCookie)
import Network.Wai (Request, requestHeaders)
import Servant (AuthProtect, ServerError, err401, errBody, errHeaders)
import Servant qualified
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
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

-- | The servant Context entries needed to serve @AuthProtect@ routes.
type AuthContext = '[AuthHandler Request User, AuthHandler Request Session]

-- Routes are tagged with the payload their handler wants — @AuthProtect User@
-- for identity only, @AuthProtect Session@ when the expiry matters too —
-- rather than with a name, so the type family is forced to be the identity.
type instance AuthServerData (AuthProtect User) = User

type instance AuthServerData (AuthProtect Session) = Session

-- | Build the symmetric signing key from the configured secret.
mkSessionKey :: Text -> JWK
mkSessionKey secret = fromOctets (encodeUtf8 secret)

sessionDurationSeconds :: NominalDiffTime
sessionDurationSeconds = 90 * 24 * 60 * 60

-- | Re-issue the session cookie once the token has less than this much life
-- left: any visit either leaves at least this much validity or renews in full.
sessionRefreshThreshold :: NominalDiffTime
sessionRefreshThreshold = sessionDurationSeconds / 2

-- | Whether a session expiring at @expiresAt@ is old enough (seen from @now@)
-- to deserve a fresh cookie.
shouldRefreshSession :: UTCTime -> UTCTime -> Bool
shouldRefreshSession now expiresAt = diffUTCTime expiresAt now < sessionRefreshThreshold

sessionCookieName :: ByteString
sessionCookieName = "session"

loginChallengeDurationSeconds :: NominalDiffTime
loginChallengeDurationSeconds = 5 * 60

data TokenPurpose
  = LoginChallenge
  | Registration
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

hs256ValidationSettings :: JWTValidationSettings
hs256ValidationSettings =
  defaultJWTValidationSettings (const True)
    & validationSettingsAlgorithms
    .~ Set.singleton HS256

generateLoginCode :: IO Text
generateLoginCode = do
  n <- generateMax 1_000_000
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

-- | The signed-JWT payload of a login challenge. Embedding 'ClaimsSet' as a
-- typed subtype (rather than stuffing extra fields into its deprecated
-- unregistered-claims map) is jose's supported way to carry custom claims:
-- 'signJWT'/'verifyJWT' serialise the whole record. The @purpose@ marker is
-- re-checked on decode, so a session token can never be replayed as a
-- challenge — its JSON shape simply won't parse as a 'ChallengeClaims'.
data ChallengeClaims = ChallengeClaims
  { stdClaims :: ClaimsSet
  , email :: Text
  , commitment :: Text
  -- ^ The peppered commitment to the code (see 'codeCommitment'), never the code.
  }

instance HasClaimsSet ChallengeClaims where
  claimsSet f c = fmap (\std -> c{stdClaims = std}) (f (c.stdClaims))

instance ToJSON ChallengeClaims where
  toJSON c =
    insertClaims
      [ ("purpose", toJSON LoginChallenge)
      , ("email", String c.email)
      , ("code", String c.commitment)
      ]
      (toJSON c.stdClaims)

instance FromJSON ChallengeClaims where
  parseJSON = withObject "ChallengeClaims" $ \o -> do
    purpose <- o .: "purpose"
    when (purpose /= LoginChallenge) (fail "not a login challenge")
    std <- parseJSON (Object o)
    email <- o .: "email"
    commitment <- o .: "code"
    pure
      ChallengeClaims
        { stdClaims = std
        , email = email
        , commitment = commitment
        }

-- | The signed-JWT payload of a registration token: proof that an email's
-- ownership was verified (via the login-code round-trip), exchangeable for
-- account creation. Separate @purpose@ from a challenge or a session, so none
-- can be replayed as another.
data RegistrationClaims = RegistrationClaims
  { regStdClaims :: ClaimsSet
  , regEmail :: Text
  }

instance HasClaimsSet RegistrationClaims where
  claimsSet f c = fmap (\std -> c{regStdClaims = std}) (f (c.regStdClaims))

instance ToJSON RegistrationClaims where
  toJSON c =
    insertClaims
      [ ("purpose", toJSON Registration)
      , ("email", String c.regEmail)
      ]
      (toJSON c.regStdClaims)

instance FromJSON RegistrationClaims where
  parseJSON = withObject "RegistrationClaims" $ \o -> do
    purpose <- o .: "purpose"
    when (purpose /= Registration) (fail "not a registration token")
    std <- parseJSON (Object o)
    email <- o .: "email"
    pure RegistrationClaims{regStdClaims = std, regEmail = email}

-- | The signed-JWT payload of a session token: the standard claims plus the
-- 'User' identity it authenticates.
data SessionClaims = SessionClaims
  { sessionStdClaims :: ClaimsSet
  , sessionUser :: User
  }

instance HasClaimsSet SessionClaims where
  claimsSet f s = fmap (\std -> s{sessionStdClaims = std}) (f (sessionStdClaims s))

instance ToJSON SessionClaims where
  toJSON s =
    insertClaims
      [ ("uid", toJSON s.sessionUser.id)
      , ("email", toJSON s.sessionUser.email)
      , ("nombre", toJSON s.sessionUser.nombre)
      ]
      (toJSON s.sessionStdClaims)

instance FromJSON SessionClaims where
  parseJSON = withObject "SessionClaims" $ \o -> do
    std <- parseJSON (Object o)
    uid <- o .: "uid"
    email <- o .: "email"
    nombre <- o .: "nombre"
    pure
      SessionClaims
        { sessionStdClaims = std
        , sessionUser = User{id = uid, email = email, nombre = nombre}
        }

insertClaims :: [(Key, Value)] -> Value -> Value
insertClaims extra (Object o) = Object (foldr (\(k, v) -> KeyMap.insert k v) o extra)
insertClaims _ other = other

issueLoginChallenge :: JWK -> ByteString -> Text -> Text -> IO (Either JWTError Text)
issueLoginChallenge key pepper email code = do
  now <- getCurrentTime
  let std =
        emptyClaimsSet
          & claimIat
          ?~ NumericDate now
            & claimExp
          ?~ NumericDate (addUTCTime loginChallengeDurationSeconds now)
  let payload =
        ChallengeClaims
          { stdClaims = std
          , email = email
          , commitment = codeCommitment pepper email code
          }
  signed <-
    runJOSE
      $ signJWT key (newJWSHeader (RequiredProtection, HS256)) payload
  pure $ fmap (decodeUtf8 . BSL.toStrict . encodeCompact) (signed :: Either JWTError SignedJWT)

data ChallengePayload = ChallengePayload
  { email :: Text
  , codeCommitmentExpected :: Text
  }
  deriving (Show, Eq)

openChallenge :: JWK -> Text -> IO (Maybe ChallengePayload)
openChallenge key challenge = do
  result <-
    runJOSE $ do
      jwt <- decodeCompact (BSL.fromStrict (encodeUtf8 challenge))
      verifyJWT hs256ValidationSettings key (jwt :: SignedJWT)
  pure $ case result :: Either JWTError ChallengeClaims of
    Left _ -> Nothing
    Right claims ->
      Just
        ChallengePayload
          { email = claims.email
          , codeCommitmentExpected = claims.commitment
          }

-- | Constant-time check that @code@ matches the challenge's commitment, so a
-- wrong code leaks nothing through timing.
checkChallengeCode :: ByteString -> ChallengePayload -> Text -> Bool
checkChallengeCode pepper payload code =
  constEq
    (encodeUtf8 payload.codeCommitmentExpected)
    (encodeUtf8 (codeCommitment pepper payload.email code))

-- | Check a challenge + submitted code and recover the email if they match.
-- Returns 'Nothing' on a bad signature, expiry, wrong purpose, or wrong code.
-- (Composition of 'openChallenge' and 'checkChallengeCode' for callers that
-- don't need the two phases separately.)
verifyLoginChallenge :: JWK -> ByteString -> Text -> Text -> IO (Maybe Text)
verifyLoginChallenge key pepper challenge code = do
  mPayload <- openChallenge key challenge
  pure $ do
    payload <- mPayload
    guard (checkChallengeCode pepper payload code)
    pure payload.email

-- | Sign a session token carrying the user's identity. Written so a
-- verification step could be inserted later without changing the shape.
issueToken :: JWK -> User -> IO (Either JWTError Text)
issueToken key user = do
  now <- getCurrentTime
  let std =
        emptyClaimsSet
          & claimIat
          ?~ NumericDate now
            & claimExp
          ?~ NumericDate (addUTCTime sessionDurationSeconds now)
  let payload = SessionClaims{sessionStdClaims = std, sessionUser = user}
  signed <-
    runJOSE
      $ signJWT key (newJWSHeader (RequiredProtection, HS256)) payload
  pure $ fmap (decodeUtf8 . BSL.toStrict . encodeCompact) (signed :: Either JWTError SignedJWT)

-- | Sign a short-lived token attesting that @email@'s ownership was just
-- verified, to be exchanged (with a display name) for a freshly created
-- account. Handed to the client only when a verified email has no account yet.
issueRegistrationToken :: JWK -> Text -> IO (Either JWTError Text)
issueRegistrationToken key email = do
  now <- getCurrentTime
  let std =
        emptyClaimsSet
          & claimIat
          ?~ NumericDate now
            & claimExp
          ?~ NumericDate (addUTCTime loginChallengeDurationSeconds now)
  let payload = RegistrationClaims{regStdClaims = std, regEmail = email}
  signed <-
    runJOSE
      $ signJWT key (newJWSHeader (RequiredProtection, HS256)) payload
  pure $ fmap (decodeUtf8 . BSL.toStrict . encodeCompact) (signed :: Either JWTError SignedJWT)

-- | Verify a registration token and recover the email whose ownership it
-- attests. 'Nothing' on a bad signature, expiry, or wrong purpose.
verifyRegistrationToken :: JWK -> Text -> IO (Maybe Text)
verifyRegistrationToken key token = do
  result <-
    runJOSE $ do
      jwt <- decodeCompact (BSL.fromStrict (encodeUtf8 token))
      verifyJWT hs256ValidationSettings key (jwt :: SignedJWT)
  pure $ case result :: Either JWTError RegistrationClaims of
    Left _ -> Nothing
    Right claims -> Just claims.regEmail

-- | What a verified session token attests: who the user is, and until when.
-- The expiry is what lets the refresh endpoint decide whether to re-issue.
data Session = Session
  { user :: User
  , expiresAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

-- | Verify a token's signature and expiry and rebuild the 'Session' from
-- its claims. No database access — identity comes entirely from the token.
-- A token without an @exp@ claim is rejected (we never issue one).
verifySession :: JWK -> ByteString -> IO (Maybe Session)
verifySession key raw = do
  result <-
    runJOSE $ do
      jwt <- decodeCompact (BSL.fromStrict raw)
      verifyJWT hs256ValidationSettings key (jwt :: SignedJWT)
  pure $ case result :: Either JWTError SessionClaims of
    Left _ -> Nothing
    Right claims -> do
      NumericDate expiresAt <- claims.sessionStdClaims ^. claimExp
      pure Session{user = claims.sessionUser, expiresAt = expiresAt}

-- | 'verifySession' for callers that only need the identity.
verifyToken :: JWK -> ByteString -> IO (Maybe User)
verifyToken key = fmap (fmap (.user)) . verifySession key

renderSessionCookie :: Bool -> Text -> Text
renderSessionCookie secure token =
  renderCookie
    $ (baseCookie secure)
      { setCookieValue = encodeUtf8 token
      , setCookieMaxAge = Just (secondsToDiffTime (round sessionDurationSeconds))
      }

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

-- | Servant auth handler for @AuthProtect User@: pull the session cookie,
-- verify it, and hand the 'User' to the protected handler (401 otherwise).
authHandler :: App -> AuthHandler Request User
authHandler app = mkAuthHandler $ authenticateWith (verifyToken app.jwk)

-- | Like 'authHandler' but for @AuthProtect Session@: same cookie, same
-- verification, but the handler also gets the token's expiry (so the refresh
-- endpoint can decide whether to re-issue).
sessionAuthHandler :: App -> AuthHandler Request Session
sessionAuthHandler app = mkAuthHandler $ authenticateWith (verifySession app.jwk)

-- | The cookie-extraction + 401 logic shared by both auth handlers.
authenticateWith :: (ByteString -> IO (Maybe a)) -> Request -> Servant.Handler a
authenticateWith verify req =
  case sessionTokenFromRequest req of
    Nothing -> throwError unauthorized
    Just token -> do
      mResult <- liftIO $ verify token
      case mResult of
        Nothing -> throwError unauthorized
        Just result -> pure result

unauthorized :: ServerError
unauthorized =
  err401
    { errBody = "{\"error\":\"not authenticated\"}"
    , errHeaders = [("Content-Type", "application/json")]
    }
