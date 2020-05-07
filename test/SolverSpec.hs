module SolverSpec where

import           Prelude
import           Test.Hspec
import           Solver
import           Data.SBV

spec :: Spec
spec = do
    it "Production" $ do
        actual <- runProduction production
        actual `shouldBe` (Just (45, 6))

    it "LinearOpt" $ do
        actual <- runLinearOpt linearOpt
        actual `shouldBe` (Just (fromRational $ 47 % 9, fromRational $ 20 % 9))
