module ValidatedSpec (spec) where

import           Test.Hspec
import           Data.Validated

spec :: Spec
spec = do
  describe "Validated" $ do
    it "smoke test" $ do
      1 `shouldBe` 1
