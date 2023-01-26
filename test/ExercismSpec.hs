module ExercismSpec where

import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )

isLeapYear :: Integer -> Bool
isLeapYear year =
  isDividedBy4
    && ( not isDividedBy100
           /= isDividedBy400
       )
  where
    isDividedBy a b = (a `mod` b) == 0
    isDividedBy4 = year `isDividedBy` 4
    isDividedBy100 = year `isDividedBy` 100
    isDividedBy400 = year `isDividedBy` 400

spec :: Spec
spec = do
  it "Leap" $ do
    isLeapYear 1800 `shouldBe` False
    isLeapYear 1900 `shouldBe` False
    isLeapYear 2000 `shouldBe` True
    isLeapYear 2100 `shouldBe` False
    isLeapYear 2200 `shouldBe` False
    isLeapYear 2300 `shouldBe` False
    isLeapYear 2400 `shouldBe` True
    isLeapYear 2500 `shouldBe` False
