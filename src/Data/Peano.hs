module Data.Peano where

import Data.Semiring (Semiring(..), (*), (+), one, zero)
import Prelude
  ( Bool(..)
  , Bounded
  , Eq
  , Int
  , Monoid
  , Ord
  , Semigroup
  , Show
  , ($)
  , (-)
  , (.)
  , (<=)
  , (==)
  , (>)
  , show
  )

data Peano
  = Zero
  | Succ Peano

instance Eq Peano where
  Zero == Zero = True
  Zero == _ = False
  _ == Zero = False
  (Succ a) == (Succ b) = a == b

instance Semiring Peano where
  a `plus` Zero = a
  a `plus` Succ b = Succ $ a + b
  zero = Zero
  _ `times` Zero = Zero
  a `times` (Succ b) = a + (a * b)
  one = Succ Zero

toInt :: Peano -> Int
toInt Zero = 0
toInt (Succ a) = 1 + toInt a

fromInt :: Int -> Peano
fromInt 0 = Zero
fromInt a = Succ (fromInt (a - 1))

instance Show Peano where
  show = show . toInt

instance Ord Peano where
  Zero <= Zero = True
  Zero <= _ = True
  _ <= Zero = False
  (Succ a) <= (Succ b) = a <= b
