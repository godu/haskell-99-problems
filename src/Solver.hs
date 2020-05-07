module Solver where

import           Prelude
import           Data.SBV

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

runProduction :: Goal -> IO (Maybe (Integer, Integer))
runProduction goal = extract <$> optimize Lexicographic goal
  where
    extract (LexicographicResult result) =
        case
                ( getModelValue "X" result :: Maybe Integer
                , getModelValue "Y" result :: Maybe Integer
                )
            of
                (Just x, Just y) -> Just (x, y)
                _                -> Nothing
    extract _ = Nothing

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

runLinearOpt :: Goal -> IO (Maybe (AlgReal, AlgReal))
runLinearOpt goal = extract <$> optimize Lexicographic goal
  where
    extract (LexicographicResult result) =
        case
                ( getModelValue "x1" result :: Maybe AlgReal
                , getModelValue "x2" result :: Maybe AlgReal
                )
            of
                (Just x, Just y) -> Just (x, y)
                _                -> Nothing
    extract _ = Nothing
