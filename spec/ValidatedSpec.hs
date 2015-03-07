module ValidatedSpec (spec) where

import           Test.Hspec
import           Data.Validated

spec :: Spec
spec = do
  describe "fmap" $ do
    it "should answer valid for valid" $ do
      fmap (+1) (Valid 1) `shouldBe` (Valid 2)

    it "should answer invalid for invalid" $ do
      fmap (+1) (Invalid ["foo", "bar"]) `shouldBe` (Invalid ["foo", "bar"])

    it "should answer doubtful for doubtful" $ do
      fmap (+1) (Doubtful 2 ["foo"]) `shouldBe` (Doubtful 3 ["foo"])

