module SolverSpec where

import           Prelude
import           Test.Hspec
import           Solver
import           Data.SBV

z3Debug :: SMTConfig
z3Debug = z3 { verbose = True }

spec :: Spec
spec = do
    it "Propositional Logic" $ do
        actual <- satWith z3Debug propositionalLogic
        (show actual) `shouldBe` "Unsatisfiable"

    it "Satisfiability and Validity" $ do
        actual <- satWith z3Debug satisfiabilityAndValidity
        (show actual) `shouldBe` "Unsatisfiable"

    it "Uninterpreted functions and constants" $ do
        actual <- satWith z3Debug uninterpretedFunctionsAndConstants
        (show actual)
            `shouldBe` "Satisfiable. Model:\n\
                                 \  a = 21 :: Integer\n\
                                 \  b = 22 :: Integer\n\
                                 \\n\
                                 \  f :: Integer -> Integer\n\
                                 \  f _ = 1"

    it "Production" $ do
        actual <- optimize Lexicographic production
        (show actual)
            `shouldBe` "Optimal model:\n\
                       \  X     = 45 :: Integer\n\
                       \  Y     =  6 :: Integer\n\
                       \  stock =  1 :: Integer"

    it "LinearOpt" $ do
        actual <- optimize Lexicographic linearOpt
        (show actual)
            `shouldBe` "Optimal model:\n\
                       \  x1   =  47 % 9 :: Real\n\
                       \  x2   =  20 % 9 :: Real\n\
                       \  goal = 355 % 9 :: Real"
