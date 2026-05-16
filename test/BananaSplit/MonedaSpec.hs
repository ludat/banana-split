module BananaSplit.MonedaSpec (spec) where

import Data.Map.Strict qualified as Map
import Protolude
import Test.Hspec

import BananaSplit.Moneda

spec :: Spec
spec = describe "PorMoneda" $ do
  describe "Semigroup" $ do
    it "preserves entries for different currencies independently" $
      enMoneda [1 :: Int] ARS <> enMoneda [2] USD
        `shouldBe` PorMoneda (Map.fromList [(ARS, [1]), (USD, [2])])

    it "combines values for the same currency with (<>)" $
      enMoneda [1 :: Int] ARS <> enMoneda [2] ARS
        `shouldBe` enMoneda [1, 2] ARS

  describe "Monoid" $ do
    it "mempty is a left identity" $
      mempty <> enMoneda [1 :: Int] ARS `shouldBe` enMoneda [1] ARS

    it "mempty is a right identity" $
      enMoneda [1 :: Int] ARS <> mempty `shouldBe` enMoneda [1] ARS
