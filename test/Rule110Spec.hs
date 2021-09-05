{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Rule110Spec where

import Control.Comonad (Comonad (duplicate, extend, extract))
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Prelude (Bool (False, True), Functor (fmap), IO, Int, Num (abs), const, cycle, otherwise, print, (!!), ($), (+), (-), (.), (==))

spec :: Spec
spec = do
  fibonacciSpec
  naiveSpec

-- https://blog.cofree.coffee/2020-10-17-bounded-space-automata/

data Store s a = Store (s -> a) s

initialStore :: Store Int Bool
initialStore = Store query 0
  where
    query :: Int -> Bool
    query i = cycle [True, False] !! abs i

pos :: Store s a -> s
pos (Store _ s) = s

peek :: s -> Store s a -> a
peek s (Store query _) = query s

peeks :: (s -> s) -> Store s a -> a
peeks f (Store query s) = query (f s)

seek :: s -> Store s a -> Store s a
seek s (Store query _) = Store query s

seeks :: (s -> s) -> Store s a -> Store s a
seeks f (Store query s) = Store query (f s)

peekMany :: [s] -> Store s a -> [a]
peekMany xs store = fmap (`peek` store) xs

peekFunctor :: Functor f => f s -> Store s a -> f a
peekFunctor fs store = fmap (`peek` store) fs

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment f store = fmap (`peek` store) (f (pos store))

updateStoreState :: (a -> b) -> Store s a -> Store s b
updateStoreState f (Store query s) = Store (f . query) s

instance Functor (Store s) where
  fmap :: (a -> b) -> Store s a -> Store s b
  fmap f (Store query s) = Store (f . query) s

instance Comonad (Store s) where
  extract :: Store s a -> a
  extract (Store query s) = query s

  extend :: (Store s a -> b) -> Store s a -> Store s b
  extend f (Store query s) = Store (f . Store query) s

  duplicate :: Store s a -> Store s (Store s a)
  duplicate (Store query a) = Store (Store query) a

fibStore :: Store Int Int
fibStore = Store query 0
  where
    query :: Int -> Int
    query 0 = 0
    query 1 = 1
    query n = query (n - 1) + query (n - 2)

window :: Store Int a -> [a]
window = experiment (\s -> [s .. s + 10])

windowedStore :: Store Int [Int]
windowedStore = extend window fibStore

fibonacciSpec :: Spec
fibonacciSpec = do
  it "fibonacci" $ do
    peek 4 windowedStore `shouldBe` [3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]

type Index = Int

type Size = Int

initializeStore :: [Bool] -> Store Index Bool
initializeStore xs = Store query 0
  where
    query :: Index -> Bool
    query i = xs !! i

neighbors :: Size -> Store Index Bool -> [Bool]
neighbors size = experiment (lookupIndices size)

lookupIndices :: Size -> Index -> [Index]
lookupIndices size i
  | i == 0 = [size - 1, 0, 1]
  | i == (size - 1) = [i - 1, i, 0]
  | otherwise = [i - 1, i, i + 1]

newState :: Size -> Store Index Bool -> Bool
newState size store =
  case neighbors size store of
    [False, False, False] -> False
    [True, False, False] -> False
    [True, True, True] -> False
    _ -> True

nextGen :: Size -> Store Index Bool -> Store Index Bool
nextGen size = extend (newState size)

viewStore :: Size -> Store Index Bool -> [Bool]
viewStore size = experiment (const [0 .. (size - 1)])

runSimulation :: Size -> Store Index Bool -> IO ()
runSimulation size store = do
  print $ viewStore size store
  runSimulation size $ nextGen size store

naiveSpec :: Spec
naiveSpec = do
  it "naive" $ do
    let initialStore = initializeStore [False, False, False, False, False, False, False, False, False, False]
        actualStore = nextGen 10 initialStore
    viewStore 10 actualStore `shouldBe` viewStore 10 initialStore
