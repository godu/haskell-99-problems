module GeneralizedAlgebraicDataTypeSpec where

import           GeneralizedAlgebraicDataType
import           Test.Hspec

spec :: Spec
spec = do
  describe "show" $ do
    it "I" $ show (I 1) `shouldBe` "1 : Int"
    it "B" $ show (B True) `shouldBe` "True : Bool"
    it "Add" $ show (Add (I 1) (I 2)) `shouldBe` "(1 : Int) + (2 : Int) : Int"
    it "Mul" $ show (Mul (I 2) (I 3)) `shouldBe` "(2 : Int) * (3 : Int) : Int"
    it "Eq"
      $ show (Eq (Add (I 2) (I 4)) (Mul (I 2) (I 3)))
      `shouldBe` "((2 : Int) + (4 : Int) : Int) == ((2 : Int) * (3 : Int) : Int) : Bool"
  describe "evaluate" $ do
    it "I" $ evaluate (I 1) `shouldBe` 1
    it "B" $ evaluate (B True) `shouldBe` True
    it "Add" $ evaluate (Add (I 1) (I 2)) `shouldBe` 3
    it "Mul" $ evaluate (Mul (I 2) (I 3)) `shouldBe` 6
    it "Eq" $ evaluate (Eq (Add (I 2) (I 4)) (Mul (I 2) (I 3))) `shouldBe` True
