{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MiniPatternsHandbook.RecursiveGo where

sum :: forall a. Num a => [a] -> a
sum = go 0
  where
    go :: a -> [a] -> a
    go !i [] = i
    go !i (x : xs) = go (i + x) xs

ordNub :: forall a. Ord a => [a] -> [a]
ordNub = go []
  where
    go :: [a] -> [a] -> [a]
    go !xs [] = xs
    go !xs (y : ys)
      | y `elem` ys = go xs ys
      | otherwise = go (xs <> [y]) ys
