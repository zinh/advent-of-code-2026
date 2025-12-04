module LibSpec (spec) where

import Test.Hspec
import Lib (rotate, parseLine, Rotation(..), Direction(..))

spec :: Spec
spec = do
  describe "rotate" $ do
    it "rotates left by subtracting the step from position" $ do
      rotate 50 (Rotation L 68) `shouldBe` 82
      rotate 50 (Rotation L 168) `shouldBe` 82

    it "rotates right by adding the step to position" $ do
      rotate 52 (Rotation R 48) `shouldBe` 0

    it "handles zero position" $ do
      rotate 0 (Rotation R 5) `shouldBe` 5
      rotate 0 (Rotation L 5) `shouldBe` 95

    it "handles zero step" $ do
      rotate 10 (Rotation L 0) `shouldBe` 10
      rotate 10 (Rotation R 0) `shouldBe` 10

  describe "parseLine" $ do
    it "parse String to left Rotation" $ do
      parseLine "L68" `shouldBe` Rotation L 68

    it "parse String to right Rotation" $ do
      parseLine "R68" `shouldBe` Rotation R 68
