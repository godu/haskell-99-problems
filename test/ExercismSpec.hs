{-# LANGUAGE NumericUnderscores #-}

module ExercismSpec where

import qualified Data.Char as Char (toLower)
import qualified Data.Set as Set (Set, delete, fromList, null)
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

isLeapYearSpec = it "isLeapYear" $ do
  isLeapYear 1_800 `shouldBe` False
  isLeapYear 1_900 `shouldBe` False
  isLeapYear 2_000 `shouldBe` True
  isLeapYear 2_100 `shouldBe` False
  isLeapYear 2_200 `shouldBe` False
  isLeapYear 2_300 `shouldBe` False
  isLeapYear 2_400 `shouldBe` True
  isLeapYear 2_500 `shouldBe` False

data Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

ageOn :: Planet -> Float -> Float
ageOn Earth = (/ oneYear)
  where
    oneYear = 365.25 * 24 * 60 * 60
ageOn Mercury = ageOn Earth . (/ 0.240_846_7)
ageOn Venus = ageOn Earth . (/ 0.615_197_26)
ageOn Mars = ageOn Earth . (/ 1.880_815_8)
ageOn Jupiter = ageOn Earth . (/ 11.862_615)
ageOn Saturn = ageOn Earth . (/ 29.447_498)
ageOn Uranus = ageOn Earth . (/ 84.016_846)
ageOn Neptune = ageOn Earth . (/ 164.791_32)

ageOnSpec = it "ageOn" $ do
  ageOn Earth 1_000_000_000 `shouldBe` 31.688_087
  ageOn Mercury 213_4835_688 `shouldBe` 280.879_36

isPangram :: String -> Bool
isPangram text = go text $ Set.fromList "abcdefghijklmnopqrstuvwxyz"
  where
    go :: String -> Set.Set Char -> Bool
    go _ ss | Set.null ss = True
    go [] _ = False
    go (x : xs) ss = go xs $ Set.delete (Char.toLower x) ss

isPangramSpec = it "isPangram" $ do
  isPangram "" `shouldBe` False
  isPangram "The quick brown fox jumps over the lazy dog" `shouldBe` True
  isPangram "a quick movement of the enemy will jeopardize five gunboats" `shouldBe` False
  isPangram "five boxing wizards jump quickly at it" `shouldBe` False
  isPangram "the_quick_brown_fox_jumps_over_the_lazy_dog" `shouldBe` True
  isPangram "\"Five quacking Zephyrs jolt my wax bed.\"" `shouldBe` True

spec :: Spec
spec = do
  isLeapYearSpec
  ageOnSpec
  isPangramSpec
