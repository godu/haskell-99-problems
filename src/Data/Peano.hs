module Data.Peano where

import Data.Ix (Ix(..))

data Peano
  = Zero
  | Succ Peano

instance Eq Peano where
  (==) Zero Zero = True
  (==) Zero _ = False
  (==) _ Zero = False
  (==) (Succ a) (Succ b) = a == b

instance Enum Peano where
  fromEnum Zero = 0
  fromEnum (Succ x) = 1 + fromEnum x
  toEnum 0 = Zero
  toEnum x
    | x > 0 = Succ (toEnum (x - 1))
    | otherwise = error ("toEnum " <> show x)
  succ = Succ
  pred Zero = error "pred Zero"
  pred (Succ a) = a

instance Show Peano where
  show = show . fromEnum

instance Ord Peano where
  (<=) Zero Zero = True
  (<=) Zero _ = True
  (<=) _ Zero = False
  (<=) (Succ a) (Succ b) = a <= b

instance Read Peano where
  readsPrec d = map (\(a, b) -> (toEnum a, b)) . readsPrec d

instance Num Peano where
  a + Zero = a
  a + Succ b = succ (a + b)
  Zero - Zero = Zero
  Zero - _ = error "minus < 0"
  a - Zero = a
  Succ a - Succ b = a - b
  Zero * _ = Zero
  _ * Zero = Zero
  a * Succ Zero = a
  a * Succ b = a + a * b
  abs = id
  signum Zero = Zero
  signum _ = Succ Zero
  fromInteger a
    | a == 0 = Zero
    | a > 0 = Succ (fromInteger (a - 1))
    | a < 0 = error "fromInteger < 0"

infinity :: Peano
infinity = Succ infinity

instance Bounded Peano where
  minBound = Zero
  maxBound = infinity

instance Ix Peano where
  range (a, b) = enumFromTo a b
  index (a, _) b = fromEnum (b - a)
  inRange (a, b) c = a <= c && b >= c

instance Real Peano where
  toRational = toRational . toInteger

instance Integral Peano where
  toInteger Zero = 0
  toInteger (Succ a) = toInteger a + 1
  quotRem Zero Zero = error "0/0"
  quotRem a b =
    case compare a b of
      LT -> (Zero, a)
      _ ->
        let (q, r) = quotRem (a - b) b
         in (Succ q, r)

instance Semigroup Peano where
  (<>) = (+)

instance Monoid Peano where
  mempty = Zero
