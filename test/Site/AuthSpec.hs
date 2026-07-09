{-# LANGUAGE OverloadedStrings #-}

module Site.AuthSpec (
  spec,
) where

import Control.Lens ((&), (?~))
import Crypto.JWT
import Data.Aeson (Value (String), toJSON)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as Text
import Data.Time (addUTCTime, getCurrentTime)
import Protolude
import Test.Hspec

import BananaSplit (User (..), nullUlid)
import Site.Auth (
  generateLoginCode,
  issueLoginChallenge,
  issueRegistrationToken,
  issueToken,
  mkSessionKey,
  verifyLoginChallenge,
  verifyRegistrationToken,
  verifyToken,
 )

-- | A dev-only signing secret. Kept over 64 bytes so the same key can also sign
-- an HS512 token in the algorithm-pinning test (HS512 requires a >=64-byte key),
-- letting that test isolate the algorithm as the sole reason for rejection.
testKey :: JWK
testKey = mkSessionKey "dev-secret-key-that-is-comfortably-over-sixty-four-bytes-in-length-abcdef"

otherKey :: JWK
otherKey = mkSessionKey "another-distinct-dev-secret-also-well-over-sixty-four-bytes-in-length-xyz"

testPepper :: ByteString
testPepper = "test-pepper"

testUser :: User
testUser = User{id = nullUlid, email = "user@example.com", nombre = "Alice"}

spec :: Spec
spec = do
  describe "generateLoginCode" $ do
    it "always produces a 6-digit numeric code" $ do
      codes <- replicateM 200 generateLoginCode
      let isCode c = Text.length c == 6 && Text.all (\d -> d >= '0' && d <= '9') c
      all isCode codes `shouldBe` True

    it "does not return a constant (crypto source actually varies)" $ do
      codes <- replicateM 200 generateLoginCode
      length (ordNub codes) `shouldSatisfy` (> 1)

  describe "login challenge round trip" $ do
    it "recovers the email from a challenge presented with the correct code" $ do
      Right challenge <-
        issueLoginChallenge testKey testPepper "user@example.com" "123456"
      result <- verifyLoginChallenge testKey testPepper challenge "123456"
      result `shouldBe` Just "user@example.com"

    it "rejects a wrong code" $ do
      Right challenge <-
        issueLoginChallenge testKey testPepper "user@example.com" "123456"
      result <- verifyLoginChallenge testKey testPepper challenge "000000"
      result `shouldBe` Nothing

    it "rejects a challenge signed with a different key" $ do
      Right challenge <-
        issueLoginChallenge otherKey testPepper "user@example.com" "123456"
      result <- verifyLoginChallenge testKey testPepper challenge "123456"
      result `shouldBe` Nothing

  describe "registration token round trip" $ do
    it "recovers the email from a freshly issued token" $ do
      Right token <- issueRegistrationToken testKey "user@example.com"
      result <- verifyRegistrationToken testKey token
      result `shouldBe` Just "user@example.com"

    it "rejects a registration token signed with a different key" $ do
      Right token <- issueRegistrationToken otherKey "user@example.com"
      result <- verifyRegistrationToken testKey token
      result `shouldBe` Nothing

    it "rejects a login challenge presented as a registration token" $ do
      Right challenge <-
        issueLoginChallenge testKey testPepper "user@example.com" "123456"
      result <- verifyRegistrationToken testKey challenge
      result `shouldBe` Nothing

  describe "session token round trip" $ do
    it "verifies a freshly issued token back into the user" $ do
      Right token <- issueToken testKey testUser
      result <- verifyToken testKey (encodeUtf8 token)
      result `shouldBe` Just testUser

    it "rejects a token signed with a different key" $ do
      Right token <- issueToken otherKey testUser
      result <- verifyToken testKey (encodeUtf8 token)
      result `shouldBe` Nothing

  describe "algorithm pinning" $ do
    -- The security-relevant assertion: a token with a *valid* HS512 signature
    -- (which the old permissive validation settings would have accepted) must
    -- now be rejected, because verification is pinned to HS256 only.
    it "rejects an otherwise-valid token signed with HS512" $ do
      Right token <- signTokenWith HS512 testKey testUser
      result <- verifyToken testKey (encodeUtf8 token)
      result `shouldBe` Nothing

    it "still accepts the HS256 tokens we actually issue" $ do
      Right token <- signTokenWith HS256 testKey testUser
      result <- verifyToken testKey (encodeUtf8 token)
      result `shouldBe` Just testUser

-- | Mirror of 'issueToken' but with a caller-chosen algorithm, so tests can
-- forge a validly-signed token under an algorithm we never issue.
signTokenWith :: Alg -> JWK -> User -> IO (Either JWTError Text)
signTokenWith alg key user = do
  now <- getCurrentTime
  let claims =
        emptyClaimsSet
          & claimIat ?~ NumericDate now
          & claimExp ?~ NumericDate (addUTCTime 3600 now)
          & addClaim "uid" (toJSON (show user.id :: Text))
          & addClaim "email" (String user.email)
          & addClaim "nombre" (String user.nombre)
  signed <- runJOSE $ signClaims key (newJWSHeader (RequiredProtection, alg)) claims
  pure $ fmap (decodeUtf8 . BSL.toStrict . encodeCompact) (signed :: Either JWTError SignedJWT)
