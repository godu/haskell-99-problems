module MiniPatternsHandbook.MakeIllegalStatesUnrepresentable where

import Data.List.NonEmpty (NonEmpty (..))

group :: Eq a => [a] -> [NonEmpty a]
group = go
  where
    go [] = []
    go (x : xs) = (x :| ys) : go zs
      where
        (ys, zs) = span (== x) xs

data UpToThree a
  = One a
  | Two a a
  | Three a a a

sumUpToThree :: Num a => UpToThree a -> a
sumUpToThree (One x) = x
sumUpToThree (Two x y) = x + y
sumUpToThree (Three x y z) = x + y + z
