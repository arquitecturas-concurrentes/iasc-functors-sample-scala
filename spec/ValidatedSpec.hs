module ValidatedSpec (spec) where

import           Test.Hspec
import           Data.Validated
import           Control.Applicative
import           Control.Monad


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

    it "should join messages on invalid and doubtful" $ do
      (Invalid ["bar"]) <*> (Doubtful 1 ["foo"])  `shouldBe` (Invalid ["foo", "bar"] :: Validated Int)

    it "should apply function with valids" $ do
      (Valid (+1)) <*> (Valid 1) `shouldBe` (Valid 2)

    it "should join messages on doubtful and invalid" $ do
      (Doubtful (+1) ["bar"]) <*> (Invalid ["foo"]) `shouldBe` (Invalid ["foo", "bar"])

    it "should integrate with liftA2" $ do
       (liftA2 elem) (Valid 'a') (Valid "hello world") `shouldBe` (Valid False)


  describe ">>=" $ do
    it "should integrate with do" $ do
      (do
        v1 <- Valid 1
        v2 <- Doubtful 2 ["foo", "bar"]
        v3 <- Doubtful 3 ["baz"]
        return $ v1 + v2 + v3) `shouldBe` (Doubtful 6 ["foo", "bar", "baz"])

  describe "ap" $ do
    it "should behave like <*> on doubtful and invalid" $ do
      let v1 = Doubtful (+1) ["foo"]
      let v2 = Invalid ["bar"]
      ap v1 v2 `shouldBe` v1 <*> v2

    it "should behave like <*> on invalid and doubtful" $ do
      let v1 = Invalid ["foo"] :: Validated (Int -> Int)
      let v2 = Doubtful 1 ["bar"]
      ap v1 v2 `shouldBe` v1 <*> v2
