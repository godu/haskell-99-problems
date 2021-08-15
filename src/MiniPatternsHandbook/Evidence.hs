module MiniPatternsHandbook.Evidence where

import Control.Applicative (liftA2)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

add :: (a -> Maybe Int) -> (a -> Maybe Int) -> a -> Maybe Int
add f g x = liftA2 (+) (f x) (g x)

getNearestValues ::
  -- | Map from positions to values
  IntMap Double ->
  -- | Current position
  Int ->
  Double
getNearestValues vals pos =
  case (prev, next) of
    (Just p, Just n) -> p + n
    (Just p, Nothing) -> p
    (Nothing, Just n) -> n
    (Nothing, Nothing) -> 0.0
  where
    prev = IntMap.lookup (pos - 1) vals
    next = IntMap.lookup (pos + 1) vals
