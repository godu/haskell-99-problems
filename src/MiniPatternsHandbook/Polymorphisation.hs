module MiniPatternsHandbook.Polymorphisation where

import Data.Foldable (Foldable (fold))
import Data.Maybe (catMaybes)

maybeConcat :: [Maybe [Int]] -> [Int]
maybeConcat = error ""

maybeConcatList :: [Maybe [a]] -> [a]
maybeConcatList = concat . catMaybes

maybeConcatFunctor :: Monoid m => [Maybe m] -> m
maybeConcatFunctor = mconcat . catMaybes

foldMaybes :: (Foldable a, Monoid m) => a (Maybe m) -> m
foldMaybes = fold . fold

containsInt :: Int -> [[Int]] -> [[Int]]
containsInt = error ""

containsElem :: Eq a => a -> [[a]] -> [[a]]
containsElem a = filter (elem a)

containsElem_ :: (Foldable f, Eq a) => a -> [f a] -> [f a]
containsElem_ = filter . elem
