module NinetyNineProblems where

import Data.List (group)
import Data.Tuple (swap)
import System.Random (RandomGen, randomR)

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) i = elementAt xs (i - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a
  = Elem a
  | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

compress :: Eq a => [a] -> [a]
compress (x:(y:xs))
  | x == y = compress (y : xs)
  | otherwise = x : compress (y : xs)
compress x = x

pack :: Eq a => [a] -> [[a]]
pack (x:xs) =
  let (h, l) = span (== x) xs
   in (x : h) : pack l
pack [] = []

encode :: Eq a => [a] -> [(Int, a)]
encode = map count . group
  where
    count xs = (myLength xs, head xs)

data ListItem a
  = Single a
  | Multiple Int a
  deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeItem . encode
  where
    encodeItem (1, x) = Single x
    encodeItem (c, x) = Multiple c x

decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified (Single x:xs) = x : decodeModified xs
decodeModified (Multiple c x:xs) = replicate c x ++ decodeModified xs
decodeModified _ = []

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = encodeModified

dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

repli :: [a] -> Int -> [a]
repli xs c = concatMap (replicate c) xs

dropEvery :: [a] -> Int -> [a]
dropEvery x c = dropEvery' x c 1
  where
    dropEvery' (x:xs) c i
      | c == i = dropEvery' xs c 1
      | otherwise = x : dropEvery' xs c (i + 1)
    dropEvery' [] _ _ = []

split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split (x:xs) i =
  let (l, r) = split xs (i - 1)
   in (x : l, r)

slice :: [a] -> Int -> Int -> [a]
slice _ 1 0 = []
slice (x:xs) 1 j = x : slice xs 1 (j - 1)
slice (x:xs) i j = slice xs (i - 1) (j - 1)
slice _ _ _ = []

rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate xs i
  | i < 0 = rotate (last xs : init xs) (i + 1)
  | otherwise = rotate (tail xs ++ [head xs]) (i - 1)

removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt i (x:xs) =
  let (l, r) = removeAt (i - 1) xs
   in (l, x : r)

insertAt :: a -> [a] -> Int -> [a]
insertAt a xs 1 = a : xs
insertAt a (x:xs) i = x : insertAt a xs (i - 1)

range :: Int -> Int -> [Int]
range l r
  | l == r = [l]
  | otherwise = l : range (l + 1) r

leftMap :: (a -> c) -> (a, b) -> (c, b)
leftMap f = swap . fmap f . swap

rndSelect :: RandomGen g => [a] -> Int -> g -> ([a], g)
rndSelect _ 0 g = ([], g)
rndSelect [] _ g = ([], g)
rndSelect xs i g = leftMap (p :) $ rndSelect xs' (i - 1) g'
  where
    (k, g') = randomR (0, length xs - 1) g
    (p, xs') = removeAt (k + 1) xs

diffSelect :: RandomGen g => Int -> Int -> g -> ([Int], g)
diffSelect 0 _ g = ([], g)
diffSelect i m g = leftMap (k :) $ diffSelect (i - 1) m g'
  where
    (k, g') = randomR (1, m) g

rndPermu :: RandomGen g => [a] -> g -> ([a], g)
rndPermu xs = rndSelect xs (length xs)
