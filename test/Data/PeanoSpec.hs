module Data.PeanoSpec where

import Data.Peano (Peano(..), fromInt)
import Data.Semiring (Semiring, (*), (+), one, zero)
import Prelude
  ( Bool(..)
  , Eq
  , Int(..)
  , Ordering(..)
  , ($)
  , (&&)
  , (.)
  , (/=)
  , (<)
  , (<$>)
  , (<=)
  , (<>)
  , (==)
  , (>)
  , (||)
  , abs
  , compare
  , const
  , id
  , otherwise
  , read
  , show
  )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (choose)

instance Arbitrary Peano where
  arbitrary = fromInt <$> choose (0, 20)

eqTransitive a b c
  | a == b =
    if b == c
      then a == c
      else a /= c
  | b == c = a /= c
  | otherwise = True

eqSymmetric a b =
  if a == b
    then b == a
    else b /= a

eqReflexive a = a == a

ordAntisymmetric a b = ((a <= b) && (b <= a)) == (a == b)

ordTransitive a b c =
  case (compare a b, compare b c) of
    (LT, LT) -> a < c
    (LT, EQ) -> a < c
    (LT, GT) -> True
    (EQ, LT) -> a < c
    (EQ, EQ) -> a == c
    (EQ, GT) -> a > c
    (GT, LT) -> True
    (GT, EQ) -> a > c
    (GT, GT) -> a > c

ordTotal a b = (a <= b) || (b <= a)

semiringCommutativePlus a b = a + b == b + a

semiringLeftIdentityPlus a = zero + a == a

semiringRightIdentityPlus a = a + zero == a

semiringAssociativeTimes a b c = (a * b) * c == a * (b * c)

semiringLeftIdentityTimes a = one * a == a

semiringRightIdentityTimes a = a * one == a

semiringLeftMultiplicationDistributes a b c = a * (b + c) == (a * b) + (a * c)

semiringRightMultiplicationDistributes a b c = (a + b) * c == (a * c) + (b * c)

semiringLeftAnnihilation a = zero * a == zero

semiringRightAnnihilation a = a * zero == zero

spec :: Spec
spec = do
  describe "Eq" $ do
    it "Transitive" $ property (eqTransitive :: Peano -> Peano -> Peano -> Bool)
    it "Symmetric" $ property (eqSymmetric :: Peano -> Peano -> Bool)
    it "Reflexive" $ property (eqReflexive :: Peano -> Bool)
  describe "Ord" $ do
    it "Antisymmetry" $ property (ordAntisymmetric :: Peano -> Peano -> Bool)
    it "Transitivity" $
      property (ordTransitive :: Peano -> Peano -> Peano -> Bool)
    it "Totality" $ property (ordTotal :: Peano -> Peano -> Bool)
  describe "Semiring" $ do
    it "Additive Commutativity" $
      property (semiringCommutativePlus :: Peano -> Peano -> Bool)
    it "Additive Left Identity" $
      property (semiringLeftIdentityPlus :: Peano -> Bool)
    it "Additive Right Identity" $
      property (semiringRightIdentityPlus :: Peano -> Bool)
    it "Multiplicative Associativity" $
      property (semiringAssociativeTimes :: Peano -> Peano -> Peano -> Bool)
    it "Multiplicative Left Identity" $
      property (semiringLeftIdentityTimes :: Peano -> Bool)
    it "Multiplicative Right Identity" $
      property (semiringRightIdentityTimes :: Peano -> Bool)
    it "Multiplication Left Distributes Over Addition" $
      property
        (semiringLeftMultiplicationDistributes :: Peano -> Peano -> Peano -> Bool)
    it "Multiplication Right Distributes Over Addition" $
      property
        (semiringRightMultiplicationDistributes :: Peano -> Peano -> Peano -> Bool)
    it "Multiplicative Left Annihilation" $
      property (semiringLeftAnnihilation :: Peano -> Bool)
    it "Multiplicative Right Annihilation" $
      property (semiringRightAnnihilation :: Peano -> Bool)
