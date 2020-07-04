{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Solver where

import           Prelude
import           Data.SBV
import           Data.Generics

-- https://rise4fun.com/z3/tutorial

-- Propositional Logic

propositionalLogic :: Goal
propositionalLogic = do
  p <- sBool "p"
  q <- sBool "q"
  r <- sBool "r"

  let conjecture = ((p .=> q) .&& (q .=> r)) .=> (p .=> r)
  constrain $ sNot conjecture

-- Satisfiability and Validity

satisfiabilityAndValidity :: Goal
satisfiabilityAndValidity = do
  a <- sBool "a"
  b <- sBool "b"

  let demorgan = (a .&& b) .== (sNot ((sNot a) .|| (sNot b)))
  constrain $ sNot demorgan

-- Uninterpreted functions and constants

uninterpretedFunctionsAndConstants :: Goal
uninterpretedFunctionsAndConstants = do
  let f :: SInteger -> SInteger
      f = uninterpret "f"
  a <- sInteger "a"
  b <- sInteger "b"

  constrain $ (a .> 20)
  constrain $ (b .> a)
  constrain $ (f 10) .== 1


newtype A = A () deriving (Eq, Ord, Data, Read, Show, SymVal, HasKind)

uninterpretedSorts :: Goal
uninterpretedSorts = do
  x <- free "x"
  y <- free "y"
  let f :: SBV A -> SBV A
      f = uninterpret "f"
  constrain $ (f (f x)) .== x
  constrain $ (f x) .== y
  constrain $ x ./= y

-- Arithmetic

arithmeticReal :: Goal
arithmeticReal = do
  a <- sInteger "a"
  b <- sInteger "b"
  c <- sInteger "c"
  d <- sReal "d"
  e <- sReal "e"

  constrain $ a .> (b + 2)
  constrain $ a .== ((2 * c) + 10)
  constrain $ (c + b) .<= 1000
  constrain $ d .>= e

arithmeticToReal :: Goal
arithmeticToReal = do
  a <- sInteger "a"
  b <- sInteger "b"
  c <- sInteger "c"
  d <- sReal "d"
  e <- sReal "e"

  constrain $ e .> ((sFromIntegral (a + b)) + 2.0)
  constrain $ d .== ((sFromIntegral c) + 0.5)
  constrain $ a .> b

-- Nonlinear arithmetic

nonlinearArithmeticSimple :: Goal
nonlinearArithmeticSimple = do
  a <- sInteger "a"

  constrain $ (a * a) .> 3

nonlinearArithmeticUnsatisfiable :: Goal
nonlinearArithmeticUnsatisfiable = do
  x <- sReal "x"
  y <- sReal "y"
  z <- sReal "z"

  constrain $ (x * x) .== (x + 2.0)
  constrain $ (x * y) .== x
  constrain $ ((y - 1.0) * z) .== 1.0

nonlinearArithmeticSatisfiable :: Goal
nonlinearArithmeticSatisfiable = do
  b <- sReal "b"
  c <- sReal "c"

  constrain $ (b * b * b) + (b * c) .== 3.0

nonlinearArithmeticUnknown :: Goal
nonlinearArithmeticUnknown = do
  nonlinearArithmeticSimple
  nonlinearArithmeticSatisfiable

-- Division

division :: Goal
division = do
  a  <- sInteger "a"
  r1 <- sInteger "r1"
  r2 <- sInteger "r2"
  r3 <- sInteger "r3"
  r4 <- sInteger "r4"
  r5 <- sInteger "r5"
  r6 <- sInteger "r6"

  constrain $ a .== 10
  constrain $ r1 .== (a `sDiv` 4)
  constrain $ r2 .== (a `sMod` 4)
  constrain $ r3 .== (a `sRem` 4)
  constrain $ r4 .== (a `sDiv` (-4))
  constrain $ r5 .== (a `sMod` (-4))
  constrain $ r6 .== (a `sRem` (-4))

  b <- sReal "b"
  c <- sReal "c"

  constrain $ b .>= (c / 3.0)
  constrain $ c .>= 20.0

divisionByZero :: Goal
divisionByZero = do
  a <- sReal "a"
  constrain $ (a / 0) .== 10.0

-- | Taken from <http://people.brunel.ac.uk/~mastjjb/jeb/or/morelp.html>
--
-- A company makes two products (X and Y) using two machines (A and B).
--
--   - Each unit of X that is produced requires 50 minutes processing time on machine
--     A and 30 minutes processing time on machine B.
--
--   - Each unit of Y that is produced requires 24 minutes processing time on machine
--     A and 33 minutes processing time on machine B.
--
--   - At the start of the current week there are 30 units of X and 90 units of Y in stock.
--     Available processing time on machine A is forecast to be 40 hours and on machine B is
--     forecast to be 35 hours.
--
--   - The demand for X in the current week is forecast to be 75 units and for Y is forecast
--     to be 95 units.
--
--   - Company policy is to maximise the combined sum of the units of X and the units of Y
--     in stock at the end of the week.
--
-- How much of each product should we make in the current week?
--
-- We have:
--
-- >>> optimize Lexicographic production
-- Optimal model:
--   X     = 45 :: Integer
--   Y     =  6 :: Integer
--   stock =  1 :: Integer
--
-- That is, we should produce 45 X's and 6 Y's, with the final maximum stock of just 1 expected!

production :: Goal
production = do
  x <- sInteger "X" -- Units of X produced
  y <- sInteger "Y" -- Units of Y produced

  -- Amount of time on machine A and B
  let timeA = 50 * x + 24 * y
      timeB = 30 * x + 33 * y

  constrain $ timeA .<= 40 * 60
  constrain $ timeB .<= 35 * 60

  -- Amount of product we'll end up with
  let finalX = x + 30
      finalY = y + 90

  -- Make sure the demands are met:
  constrain $ finalX .>= 75
  constrain $ finalY .>= 95

  -- Policy: Maximize the final stock
  maximize "stock" $ (finalX - 75) + (finalY - 95)

toResult :: OptimizeResult -> Maybe SMTResult
toResult (LexicographicResult result) = Just result
toResult _                            = Nothing

-- | Taken from <http://people.brunel.ac.uk/~mastjjb/jeb/or/morelp.html>
--
--    *  maximize 5x1 + 6x2
--       - subject to
--
--          1. x1 + x2 <= 10
--          2. x1 - x2 >= 3
--          3. 5x1 + 4x2 <= 35
--          4. x1 >= 0
--          5. x2 >= 0
--
-- >>> optimize Lexicographic problem
-- Optimal model:
--   x1   =  47 % 9 :: Real
--   x2   =  20 % 9 :: Real
--   goal = 355 % 9 :: Real

linearOpt :: Goal
linearOpt = do
  [x1, x2] <- mapM sReal ["x1", "x2"]

  constrain $ x1 + x2 .<= 10
  constrain $ x1 - x2 .>= 3
  constrain $ 5 * x1 + 4 * x2 .<= 35
  constrain $ x1 .>= 0
  constrain $ x2 .>= 0

  maximize "goal" $ 5 * x1 + 6 * x2

xkcd :: Goal
xkcd = do
  [a, b, c, d, e, f] <- sWord16s ["a", "b", "c", "d", "e", "f"]

  constrain $ sAnd $ (.<= 10) <$> [a, b, c, d, e, f]

  constrain
    $   1505
    .== ((a * 215) + (b * 275) + (c * 335) + (d * 355) + (e * 420) + (f * 580))


euler :: Goal
euler = do
  [a, b, c] <- sIntegers ["a", "b", "c"]
  constrain $ a * a + b * b .== c * c
  constrain $ 0 .< a .&& a .< b .&& b .< c
  constrain $ a + b + c .== 1000

type Board = [[SWord8]]
type Puzzle = (Int, [SWord8] -> Board)
sudoku :: Puzzle -> Goal
sudoku (i, mkBoard) = do
  board <- mkBoard <$> mkExistVars i
  constrain $ sAnd $ inRange <$> concat board
  constrain
    $   sAnd
    $   distinct
    <$> (rows board)
    <>  (columns board)
    <>  (regions board)
 where
  rows board = board
  columns [[a1, a2, a3, a4, a5, a6, a7, a8, a9], [b1, b2, b3, b4, b5, b6, b7, b8, b9], [c1, c2, c3, c4, c5, c6, c7, c8, c9], [d1, d2, d3, d4, d5, d6, d7, d8, d9], [e1, e2, e3, e4, e5, e6, e7, e8, e9], [f1, f2, f3, f4, f5, f6, f7, f8, f9], [g1, g2, g3, g4, g5, g6, g7, g8, g9], [h1, h2, h3, h4, h5, h6, h7, h8, h9], [i1, i2, i3, i4, i5, i6, i7, i8, i9]]
    = [ [a1, b1, c1, d1, e1, f1, g1, h1, i1]
      , [a2, b2, c2, d2, e2, f2, g2, h2, i2]
      , [a3, b3, c3, d3, e3, f3, g3, h3, i3]
      , [a4, b4, c4, d4, e4, f4, g4, h4, i4]
      , [a5, b5, c5, d5, e5, f5, g5, h5, i5]
      , [a6, b6, c6, d6, e6, f6, g6, h6, i6]
      , [a7, b7, c7, d7, e7, f7, g7, h7, i7]
      , [a8, b8, c8, d8, e8, f8, g8, h8, i8]
      , [a9, b9, c9, d9, e9, f9, g9, h9, i9]
      ]
  regions [[a1, a2, a3, a4, a5, a6, a7, a8, a9], [b1, b2, b3, b4, b5, b6, b7, b8, b9], [c1, c2, c3, c4, c5, c6, c7, c8, c9], [d1, d2, d3, d4, d5, d6, d7, d8, d9], [e1, e2, e3, e4, e5, e6, e7, e8, e9], [f1, f2, f3, f4, f5, f6, f7, f8, f9], [g1, g2, g3, g4, g5, g6, g7, g8, g9], [h1, h2, h3, h4, h5, h6, h7, h8, h9], [i1, i2, i3, i4, i5, i6, i7, i8, i9]]
    = [ [a1, a2, a3, b1, b2, b3, c1, c2, c3]
      , [a4, a5, a6, b4, b5, b6, c4, c5, c6]
      , [a7, a8, a9, b7, b8, b9, c7, c8, c9]
      , [d1, d2, d3, e1, e2, e3, f1, f2, f3]
      , [d4, d5, d6, e4, e5, e6, f4, f5, f6]
      , [d7, d8, d9, e7, e8, e9, f7, f8, f9]
      , [g1, g2, g3, h1, h2, h3, i1, i2, i3]
      , [g4, g5, g6, h4, h5, h6, i4, i5, i6]
      , [g7, g8, g9, h7, h8, h9, i7, i8, i9]
      ]
  inRange x = 1 .<= x .&& x .<= 9
