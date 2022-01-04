module RecursionSpec where

import Test.Hspec (Spec, it, shouldBe)
import Prelude hiding (maximum, sum)

maximum :: [Int] -> Int
maximum [] = minBound
maximum (x : xs) =
  if (x > y) then x else y
  where
    y = maximum xs

spec :: Spec
spec = do
  it "maximum" $ do
    maximum [0, 3, 1, 4, 2] `shouldBe` 4
