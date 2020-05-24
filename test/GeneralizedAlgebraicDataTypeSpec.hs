module GeneralizedAlgebraicDataTypeSpec where

import           Prelude
import           Test.Hspec
import           GeneralizedAlgebraicDataType

spec :: Spec
spec = do
  describe "GeneralizedAlgebraicDatatype" $ do
    it "show" $ do
      show (I 1) `shouldBe` "1 : Int"
    it "evaluate" $ do
      evaluate (I 1) `shouldBe` 1
      evaluate (B False) `shouldBe` False
      evaluate (Add (I 1) (I 2)) `shouldBe` 3
      evaluate (Mul (I 2) (I 3)) `shouldBe` 6
      evaluate (Eq (I 1) (I 2)) `shouldBe` False
