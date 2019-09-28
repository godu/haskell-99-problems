module Data.PeanoSpec where

import Control.Exception (evaluate)
import Data.Ix (Ix(..), inRange, index, range)
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
    evaluate (toEnum (-1) :: Peano) `shouldThrow` errorCall "toEnum -1"
    fromEnum Zero `shouldBe` 0
    fromEnum (Succ Zero) `shouldBe` 1
    succ Zero `shouldBe` Succ Zero
    succ (Succ Zero) `shouldBe` Succ (Succ Zero)
    pred (Succ Zero) `shouldBe` Zero
    evaluate (pred Zero) `shouldThrow` errorCall "pred Zero"
  it "Show" $ do
    show Zero `shouldBe` "0"
    show (Succ Zero) `shouldBe` "1"
  it "Ord" $ do
    Zero <= Zero `shouldBe` True
    (Zero <= Succ Zero) `shouldBe` True
    (Succ Zero <= Zero) `shouldBe` False
    (Succ Zero <= Succ Zero) `shouldBe` True
  it "Read" $ do
    read "0" `shouldBe` Zero
    read "1" `shouldBe` Succ Zero
    evaluate (read "-1" :: Peano) `shouldThrow` errorCall "toEnum -1"
    evaluate (read "a" :: Peano) `shouldThrow`
      errorCall "Prelude.read: no parse"
  it "Num" $ do
    Succ (Succ Zero) + Succ Zero `shouldBe` Succ (Succ (Succ Zero))
    Succ Zero + Succ (Succ Zero) `shouldBe` Succ (Succ (Succ Zero))
    Succ (Succ Zero) - Succ Zero `shouldBe` Succ Zero
    evaluate (Zero - Succ Zero) `shouldThrow` errorCall "minus < 0"
    Succ Zero * Zero `shouldBe` Zero
    Zero * Succ Zero `shouldBe` Zero
    Succ (Succ Zero) *
      Succ (Succ Zero) `shouldBe` Succ (Succ (Succ (Succ Zero)))
    abs (Succ Zero) `shouldBe` Succ Zero
    signum Zero `shouldBe` Zero
    signum (Succ (Succ Zero)) `shouldBe` Succ Zero
    (2 :: Peano) `shouldBe` Succ (Succ Zero)
    (0 :: Peano) `shouldBe` Zero
    evaluate (fromInteger (-2) :: Peano) `shouldThrow`
      errorCall "fromInteger < 0"
  it "Ix" $ do
    range (Zero, Succ (Succ Zero)) `shouldBe`
      [Zero, Succ Zero, Succ (Succ Zero)]
    range (Zero, Zero) `shouldBe` [Zero]
    range (Succ Zero, Zero) `shouldBe` []
    index (Zero, Succ (Succ Zero)) (Succ Zero) `shouldBe` 1
    inRange (Zero, Succ (Succ Zero)) (Succ Zero) `shouldBe` True
    inRange (Zero, Succ Zero) (Succ (Succ Zero)) `shouldBe` False
  it "Real" $ do
    toRational Zero `shouldBe` 0
    toRational (Succ Zero) `shouldBe` 1
  it "Integral" $ do
    toInteger Zero `shouldBe` 0
    toInteger (Succ Zero) `shouldBe` 1
    quotRem (Succ Zero) (Succ (Succ Zero)) `shouldBe` (Zero, Succ Zero)
    quotRem (Succ (Succ Zero)) (Succ Zero) `shouldBe` (Succ (Succ Zero), Zero)
    evaluate (quotRem Zero Zero) `shouldThrow` errorCall "0/0"
