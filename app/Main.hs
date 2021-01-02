module Main where

import qualified Data.Algebra as Algebra
import Data.SBV
import Solver

mkBoard
  [ a3,
    a4,
    a6,
    a7,
    a8,
    a9,
    b2,
    b3,
    b7,
    b8,
    b9,
    c1,
    c4,
    c5,
    c6,
    c7,
    c9,
    d2,
    d3,
    d4,
    d6,
    d7,
    d8,
    e2,
    e3,
    e5,
    e7,
    e8,
    f2,
    f3,
    f4,
    f6,
    f7,
    f8,
    g1,
    g3,
    g4,
    g5,
    g6,
    g9,
    h1,
    h2,
    h3,
    h7,
    h8,
    i1,
    i2,
    i3,
    i4,
    i6,
    i7
    ] =
    [ [5, 3, a3, a4, 7, a6, a7, a8, a9],
      [6, b2, b3, 1, 9, 5, b7, b8, b9],
      [c1, 9, 8, c4, c5, c6, c7, 6, c9],
      [8, d2, d3, d4, 6, d6, d7, d8, 3],
      [4, e2, e3, 8, e5, 3, e7, e8, 1],
      [7, f2, f3, f4, 2, f6, f7, f8, 6],
      [g1, 6, g3, g4, g5, g6, 2, 8, g9],
      [h1, h2, h3, 4, 1, 9, h7, h8, 5],
      [i1, i2, i3, i4, 8, i6, i7, 7, 9]
    ]
mkBoard _ = error "puzzle needs exactly 81 elements!"

puzzle = (51, mkBoard)

main :: IO ()
main = do
  Algebra.main
  actual <- sat (sudoku puzzle)
  print actual
