module BananaSplit.Persistence.LoginAttemptsSpec (
  spec,
) where

import Protolude
import Test.Hspec

import BananaSplit.Persistence (LoginEvent (..), clearAttempts, countRecentAttempts, recordAttempt)
import BananaSplit.Persistence.SpecHook (RunDb (..))

spec :: SpecWith RunDb
spec = describe "login attempt rate-limit log" $ do
  it "counts an email's recorded attempts" $ \(RunDb runDb) -> do
    runDb $ do
      recordAttempt "user@example.com" VerifyFailure
      recordAttempt "user@example.com" VerifyFailure
    n <- runDb $ countRecentAttempts "user@example.com"
    n `shouldBe` 2

  it "counts every event kind against the same budget" $ \(RunDb runDb) -> do
    runDb $ do
      recordAttempt "user@example.com" CodeSent
      recordAttempt "user@example.com" VerifyFailure
      recordAttempt "user@example.com" VerifyFailure
    n <- runDb $ countRecentAttempts "user@example.com"
    n `shouldBe` 3

  it "keeps counts separate per email" $ \(RunDb runDb) -> do
    runDb $ do
      recordAttempt "a@example.com" VerifyFailure
      recordAttempt "b@example.com" VerifyFailure
    na <- runDb $ countRecentAttempts "a@example.com"
    na `shouldBe` 1

  it "counts case-insensitively (emails are normalized)" $ \(RunDb runDb) -> do
    runDb $ recordAttempt "User@Example.com " VerifyFailure
    n <- runDb $ countRecentAttempts "user@example.com"
    n `shouldBe` 1

  it "clearAttempts wipes the email's attempts" $ \(RunDb runDb) -> do
    runDb $ do
      recordAttempt "user@example.com" VerifyFailure
      recordAttempt "user@example.com" CodeSent
      clearAttempts "user@example.com"
    n <- runDb $ countRecentAttempts "user@example.com"
    n `shouldBe` 0
