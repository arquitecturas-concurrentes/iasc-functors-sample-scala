module ValidatedAccSpec (spec) where

import           Test.Hspec
import           Data.Validated.Acc
import           Control.Applicative

spec :: Spec
spec = do
  describe "fmap" $ do
    it "should answer valid for valid" $ do
      fmap (+1) (Valid 1) `shouldBe` (Valid 2)

    it "should answer invalid for invalid" $ do
      fmap (+1) (Invalid ["foo", "bar"]) `shouldBe` (Invalid ["foo", "bar"])

    it "should answer doubtful for doubtful" $ do
      fmap (+1) (Doubtful 2 ["foo"]) `shouldBe` (Doubtful 3 ["foo"])

  describe "pure" $ do
    it "should answer valid" $ do
      pure 1 `shouldBe` (Valid 1)

  describe "<*>" $ do
    it "should keep messages on doubtful" $ do
      (Doubtful (+1) ["bar"]) <*> (Doubtful 1 ["foo"])  `shouldBe` (Doubtful 2 ["foo", "bar"])

    it "should keep messages on invalid" $ do
      (Invalid ["bar"]) <*> (Doubtful 1 ["foo"])  `shouldBe` (Invalid ["foo", "bar"] :: Validated Int)

    it "should keep messages on doubtful" $ do
      (Valid (+1)) <*> (Valid 1) `shouldBe` (Valid 2)

    it "should integrate with liftA2" $ do
       (liftA2 elem) (Valid 'a') (Valid "hello world") `shouldBe` (Valid False)
