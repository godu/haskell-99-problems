{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import GHC.Float (double2Float, float2Int)
import Linear (V3 (V3))
import Numeric.Noise.Perlin (noiseValue, perlin)
import Vis
  ( Options (optBackgroundColor, optWindowName, optWindowSize),
    VisObject (Quad, Trans, VisObjects),
    defaultOpts,
    greyN,
    simulate,
    white,
  )
import Prelude hiding (filter)

data World = World

(width, height) = (100, 100)

amplitide = 10

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
    frameRate = 1
    initialWorld = updateWorld 0 []
    drawWorld quads =
      VisObjects $
        ( \(a, b, c, d, color) ->
            Trans (V3 (- (width / 2)) (- (height / 2)) 0) $
              Quad a b c d color
        )
          <$> quads
    updateWorld time _ =
      (\[x, y] -> quad x y)
        <$> sequence [[(- width) .. width], [(- height) .. height]]
      where
        perlinNoise = perlin (float2Int time) 5 0.05 0.5
        point x y = V3 x y $ (* amplitide) $ noiseValue perlinNoise (x, y, 0)
        quad x y =
          ( point (x - 0.5) (y - 0.5),
            point (x + 0.5) (y - 0.5),
            point (x + 0.5) (y + 0.5),
            point (x - 0.5) (y + 0.5),
            greyN $ double2Float $ (/ 2) $ (+ 1) $ noiseValue perlinNoise (x, y, 0)
          )
