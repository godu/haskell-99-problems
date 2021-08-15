module Graham.Lecture6Spec where

import Graham.Lecture6
  ( and,
    concat,
    elem,
    merge,
    replicate,
    (!!),
  )
import Test.Hspec
  ( Spec,
    it,
    shouldBe,
  )
import Prelude (Bool (False, True), Int, ($))

{-# ANN module "HLint: ignore Use &&" #-}

spec :: Spec
spec = do
  it "and" $ do
    and [] `shouldBe` True
    and [True, True] `shouldBe` True
    and [True, False, True] `shouldBe` False

  it "concat" $ do
    concat ([] :: [[Int]]) `shouldBe` []
    concat [[1], [], [2, 3]] `shouldBe` [1, 2, 3]

  it "replicate" $ do
    replicate 0 True `shouldBe` []
    replicate 2 True `shouldBe` [True, True]

  it "(!!)" $ do
    [1, 2, 3] !! 1 `shouldBe` 2

  it "elem" $ do
    elem 0 [1, 2, 3] `shouldBe` False
    elem 2 [1, 2, 3] `shouldBe` True

  it "merge" $ do
    merge [2, 5, 6] [1, 3, 4] `shouldBe` [1, 2, 3, 4, 5, 6]
