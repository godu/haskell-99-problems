module Data.Natural where

import           Data.Semiring                  ( Semiring(..)
                                                , (*)
                                                , (+)
                                                , one
                                                , zero
                                                )
import           Prelude                        ( Eq
                                                , Bool(..)
                                                , Bounded
                                                , Int
                                                , Monoid
                                                , Ord(..)
                                                , Semigroup
                                                , Show(..)
                                                , ($)
                                                , (.)
                                                , (-)
                                                , (==)
                                                )

data Natural
  = Zero
  | Succ Natural

instance Eq Natural where
  Zero     == Zero     = True
  Zero     == _        = False
  _        == Zero     = False
  (Succ a) == (Succ b) = a == b

instance Semiring Natural where
  a `plus` Zero   = a
  a `plus` Succ b = Succ $ a + b
  zero = Zero
  _ `times` Zero     = Zero
  a `times` (Succ b) = a + (a * b)
  one = Succ Zero

toInt :: Natural -> Int
toInt Zero     = 0
toInt (Succ a) = 1 + toInt a

fromInt :: Int -> Natural
fromInt 0 = Zero
fromInt a = Succ (fromInt (a - 1))

instance Show Natural where
  show = show . toInt

instance Ord Natural where
  Zero     <= Zero     = True
  Zero     <= _        = True
  _        <= Zero     = False
  (Succ a) <= (Succ b) = a <= b
