module Data.Integer where

import qualified Data.Natural as N
  ( Natural (..),
    fromInt,
    toInt,
  )
import Data.Semiring
  ( Ring (..),
    Semiring (..),
    negate,
    one,
    zero,
    (*),
    (+),
  )
import Prelude
  ( Bool (..),
    Bounded,
    Eq,
    Int,
    Monoid,
    Ord,
    Semigroup,
    Show,
    otherwise,
    show,
    ($),
    (-),
    (.),
    (<=),
    (==),
    (>),
  )

data Integer
  = Integer N.Natural N.Natural

instance Eq Integer where
  (Integer a b) == (Integer c d) = (a + d) == (b + c)

instance Semiring Integer where
  (Integer a b) `plus` (Integer c d) = Integer (a + c) (b + d)
  zero = Integer zero zero
  (Integer a b) `times` (Integer c d) = Integer (a * c + b * d) (a * d + b * c)
  one = Integer one zero

instance Ring Integer where
  negate (Integer a b) = Integer b a

toInt :: Integer -> Int
toInt (Integer a b) = N.toInt a - N.toInt b

fromInt :: Int -> Integer
fromInt x
  | 0 <= x = Integer (N.fromInt x) zero
  | otherwise = Integer zero (N.fromInt (- x))

instance Show Integer where
  show = show . toInt

instance Ord Integer where
  (Integer a b) <= (Integer c d) = (a + d) <= (b + c)
