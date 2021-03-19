-- Lecture 6 - Recursive Functions
-- https://www.youtube.com/watch?v=WawJ8LArl54

module Graham.Lecture6 where

import Prelude
  ( Bool (False, True),
    Eq,
    Int,
    Ord,
    otherwise,
    (++),
    (-),
    (<=),
    (==),
  )

{-# ANN module "HLint: ignore Use foldr" #-}

and :: [Bool] -> Bool
and [] = True
and (True : xs) = and xs
and (False : xs) = False

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

(!!) :: [a] -> Int -> a
(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys)
  | x == y = True
  | otherwise = elem x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys
