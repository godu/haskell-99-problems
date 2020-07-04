module SolverSpec where

import           Prelude
import           Test.Hspec
import           Solver
import           Data.SBV

z3Debug :: SMTConfig
z3Debug = z3 { verbose = True }

spec :: Spec
spec = do
    describe "Z3 - Guide" $ do
        it "Propositional Logic" $ do
            actual <- sat propositionalLogic
            (show actual) `shouldBe` "Unsatisfiable"

        it "Satisfiability and Validity" $ do
            actual <- sat satisfiabilityAndValidity
            (show actual) `shouldBe` "Unsatisfiable"

        it "Uninterpreted functions and constants" $ do
            actual <- sat uninterpretedFunctionsAndConstants
            (show actual)
                `shouldBe` "Satisfiable. Model:\n\
                           \  a = 21 :: Integer\n\
                           \  b = 22 :: Integer\n\
                           \\n\
                           \  f :: Integer -> Integer\n\
                           \  f _ = 1"

        it "Uninterpreted sorts" $ do
            actual <- sat uninterpretedSorts
            (show actual)
                `shouldBe` "Satisfiable. Model:\n\
                          \  x = A!val!1 :: A\n\
                          \  y = A!val!0 :: A\n\
                          \\n\
                          \  f :: A -> A\n\
                          \  f A!val!0 = A!val!1\n\
                          \  f _       = A!val!0"

        describe "Arithmetic" $ do
            it "Real" $ do
                actual <- sat arithmeticReal
                (show actual)
                    `shouldBe` "Satisfiable. Model:\n\
                               \  a =  10 :: Integer\n\
                               \  b =   0 :: Integer\n\
                               \  c =   0 :: Integer\n\
                               \  d = 0.0 :: Real\n\
                               \  e = 0.0 :: Real"
            it "ToReal" $ do
                actual <- sat arithmeticToReal
                (show actual)
                    `shouldBe` "Satisfiable. Model:\n\
                               \  a =   1 :: Integer\n\
                               \  b =   0 :: Integer\n\
                               \  c =   0 :: Integer\n\
                               \  d = 0.5 :: Real\n\
                               \  e = 4.0 :: Real"
        describe "Nonlinear arithmetic" $ do
            it "Simple" $ do
                actual <- sat nonlinearArithmeticSimple
                (show actual)
                    `shouldBe` "Satisfiable. Model:\n\
                               \  a = -3 :: Integer"
            it "Unknown" $ do
                actual <- sat nonlinearArithmeticUnknown
                (show actual)
                    `shouldBe` "Unknown.\n\
                               \  Reason: smt tactic failed to show goal to be sat/unsat (incomplete (theory arithmetic))"

            it "Unsatisfiable" $ do
                actual <- sat nonlinearArithmeticUnsatisfiable
                (show actual) `shouldBe` "Unsatisfiable"

            it "Satisfiable" $ do
                actual <- sat nonlinearArithmeticSatisfiable
                (show actual)
                    `shouldBe` "Satisfiable. Model:\n\
                               \  b =     0.125 :: Real\n\
                               \  c = 23.984375 :: Real"


    it "xkcd 287" $ do
        actual <- allSat xkcd
        (show actual)
            `shouldBe` "Solution #1:\n\
                       \  a = 1 :: Word16\n\
                       \  b = 0 :: Word16\n\
                       \  c = 0 :: Word16\n\
                       \  d = 2 :: Word16\n\
                       \  e = 0 :: Word16\n\
                       \  f = 1 :: Word16\n\
                       \Solution #2:\n\
                       \  a = 7 :: Word16\n\
                       \  b = 0 :: Word16\n\
                       \  c = 0 :: Word16\n\
                       \  d = 0 :: Word16\n\
                       \  e = 0 :: Word16\n\
                       \  f = 0 :: Word16\n\
                       \Found 2 different solutions."

    it "euler 9" $ do
        actual <- sat euler
        (show actual)
            `shouldBe` "Satisfiable. Model:\n\
                       \  a = 200 :: Integer\n\
                       \  b = 375 :: Integer\n\
                       \  c = 425 :: Integer"

    it "sudoku" $ do
        let mkBoard [a3, a4, a6, a7, a8, a9, b2, b3, b7, b8, b9, c1, c4, c5, c6, c7, c9, d2, d3, d4, d6, d7, d8, e2, e3, e5, e7, e8, f2, f3, f4, f6, f7, f8, g1, g3, g4, g5, g6, g9, h1, h2, h3, h7, h8, i1, i2, i3, i4, i6, i7]
                = [ [5, 3, a3, a4, 7, a6, a7, a8, a9]
                  , [6, b2, b3, 1, 9, 5, b7, b8, b9]
                  , [c1, 9, 8, c4, c5, c6, c7, 6, c9]
                  , [8, d2, d3, d4, 6, d6, d7, d8, 3]
                  , [4, e2, e3, 8, e5, 3, e7, e8, 1]
                  , [7, f2, f3, f4, 2, f6, f7, f8, 6]
                  , [g1, 6, g3, g4, g5, g6, 2, 8, g9]
                  , [h1, h2, h3, 4, 1, 9, h7, h8, 5]
                  , [i1, i2, i3, i4, 8, i6, i7, 7, 9]
                  ]
            mkBoard _ = error "puzzle needs exactly 81 elements!"
            puzzle = (51, mkBoard)
        actual <- sat (sudoku puzzle)
        (show actual)
            `shouldBe` "Satisfiable. Model:\n\
                       \  s0  = 4 :: Word8\n\
                       \  s1  = 6 :: Word8\n\
                       \  s2  = 8 :: Word8\n\
                       \  s3  = 9 :: Word8\n\
                       \  s4  = 1 :: Word8\n\
                       \  s5  = 2 :: Word8\n\
                       \  s6  = 7 :: Word8\n\
                       \  s7  = 2 :: Word8\n\
                       \  s8  = 3 :: Word8\n\
                       \  s9  = 4 :: Word8\n\
                       \  s10 = 8 :: Word8\n\
                       \  s11 = 1 :: Word8\n\
                       \  s12 = 3 :: Word8\n\
                       \  s13 = 4 :: Word8\n\
                       \  s14 = 2 :: Word8\n\
                       \  s15 = 5 :: Word8\n\
                       \  s16 = 7 :: Word8\n\
                       \  s17 = 5 :: Word8\n\
                       \  s18 = 9 :: Word8\n\
                       \  s19 = 7 :: Word8\n\
                       \  s20 = 1 :: Word8\n\
                       \  s21 = 4 :: Word8\n\
                       \  s22 = 2 :: Word8\n\
                       \  s23 = 2 :: Word8\n\
                       \  s24 = 6 :: Word8\n\
                       \  s25 = 5 :: Word8\n\
                       \  s26 = 7 :: Word8\n\
                       \  s27 = 9 :: Word8\n\
                       \  s28 = 1 :: Word8\n\
                       \  s29 = 3 :: Word8\n\
                       \  s30 = 9 :: Word8\n\
                       \  s31 = 4 :: Word8\n\
                       \  s32 = 8 :: Word8\n\
                       \  s33 = 5 :: Word8\n\
                       \  s34 = 9 :: Word8\n\
                       \  s35 = 1 :: Word8\n\
                       \  s36 = 5 :: Word8\n\
                       \  s37 = 3 :: Word8\n\
                       \  s38 = 7 :: Word8\n\
                       \  s39 = 4 :: Word8\n\
                       \  s40 = 2 :: Word8\n\
                       \  s41 = 8 :: Word8\n\
                       \  s42 = 7 :: Word8\n\
                       \  s43 = 6 :: Word8\n\
                       \  s44 = 3 :: Word8\n\
                       \  s45 = 3 :: Word8\n\
                       \  s46 = 4 :: Word8\n\
                       \  s47 = 5 :: Word8\n\
                       \  s48 = 2 :: Word8\n\
                       \  s49 = 6 :: Word8\n\
                       \  s50 = 1 :: Word8"


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
