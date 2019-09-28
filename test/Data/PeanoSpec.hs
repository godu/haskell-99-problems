module Data.PeanoSpec where

import Control.Exception (evaluate)
import Data.Peano (Peano(..))
import Test.Hspec (Spec, errorCall, hspec, it, shouldBe, shouldThrow)

spec :: Spec
spec = do
  it "Eq" $ do
    (Zero == Zero) `shouldBe` True
    (Zero == Succ Zero) `shouldBe` False
    (Succ Zero == Zero) `shouldBe` False
    (Succ Zero == Succ Zero) `shouldBe` True
    (Succ Zero == Succ (Succ Zero)) `shouldBe` False
    (Succ (Succ Zero) == Succ Zero) `shouldBe` False
  it "Enum" $ do
    toEnum 0 `shouldBe` Zero
    toEnum 1 `shouldBe` Succ Zero
    fromEnum Zero `shouldBe` 0
    fromEnum (Succ Zero) `shouldBe` 1
    succ Zero `shouldBe` Succ Zero
    succ (Succ Zero) `shouldBe` Succ (Succ Zero)
    pred (Succ Zero) `shouldBe` Zero
    evaluate (pred Zero) `shouldThrow` errorCall "toEnum -1"
  it "Ord" $ do
    Zero <= Zero `shouldBe` True
    (Zero <= Succ Zero) `shouldBe` True
    (Succ Zero <= Zero) `shouldBe` False
    (Succ Zero <= Succ Zero) `shouldBe` True
  it "Read" $ do
    read "0" `shouldBe` Zero
    read "1" `shouldBe` Succ Zero
    evaluate (read "a" :: Peano) `shouldThrow`
      errorCall "Prelude.read: no parse"
