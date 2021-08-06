{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Data.Ix (inRange)
import qualified Data.List
import Data.Set (Set, filter, fromList, member, toList)
import GHC.Float (int2Double)
import Linear (V3 (V3))
import Vis
  ( Flavour (Solid),
    Options (optBackgroundColor, optWindowName, optWindowSize),
    VisObject (Sphere, Trans, VisObjects),
    blue,
    defaultOpts,
    simulate,
    white,
  )
import Prelude hiding (filter)

newtype World = World (Set (V3 Int))

data State = Active | Inactive deriving (Eq)

rules :: (Int, Int, Int, Int)
rules = (4, 5, 5, 5)

main :: IO ()
main = simulate options frameRate initialWorld drawWorld updateWorld
  where
    options =
      ( defaultOpts
          { optWindowName = "Perlin landscape",
            optBackgroundColor = Just white,
            optWindowSize = Just (800, 600)
          }
      )
    frameRate = 1 / 2
    initialWorld =
      World $
        fromList
          [ V3 1 0 0,
            V3 1 1 0,
            V3 0 2 0,
            V3 (-1) 2 0,
            V3 (-2) 0 0,
            V3 (-2) 1 0,
            V3 0 0 1,
            V3 0 1 1,
            V3 (-1) 0 1,
            V3 (-1) 1 1
          ]
    drawWorld (World cells) = VisObjects $ createSphere <$> toList cells
      where
        createSphere position =
          Trans (int2Double <$> position) $
            Sphere 0.5 Solid blue

    updateWorld _ (World cells) = World $ generation cells

range :: Enum a => V3 a -> V3 a -> [V3 a]
range (V3 x y z) (V3 x' y' z') =
  (\[x, y, z] -> V3 x y z)
    <$> sequence
      [ [x .. x'],
        [y .. y'],
        [z .. z']
      ]

neighbors :: (Enum a, Num a, Ord a) => V3 a -> [V3 a]
neighbors p@(V3 x y z) =
  Data.List.filter
    (/= p)
    $ (\[x', y', z'] -> V3 (x + x') (y + y') (z + z'))
      <$> sequence
        [ [(-1) .. 1],
          [(-1) .. 1],
          [(-1) .. 1]
        ]

generation :: (Ord a, Num a, Enum a) => Set (V3 a) -> Set (V3 a)
generation cells = filter willBeAlive nextCells
  where
    nextCells = fromList $ foldMap neighbors cells
    willBeAlive cell =
      if cell `member` cells
        then inRange (a, b) activeNeighbors
        else inRange (c, d) activeNeighbors
      where
        (a, b, c, d) = rules
        activeNeighbors = length $ Data.List.filter (`member` cells) (neighbors cell)
