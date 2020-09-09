{-# LANGUAGE GADTs #-}

module GeneralizedAlgebraicDataType where

data Expression a where
  I :: Int -> Expression Int
  B :: Bool -> Expression Bool
  Add :: Expression Int -> Expression Int -> Expression Int
  Mul :: Expression Int -> Expression Int -> Expression Int
  Eq :: (Eq a, Show a) => Expression a -> Expression a -> Expression Bool

instance (Show a) => Show (Expression a) where
  show (I a) = show a <> " : Int"
  show (B a) = show a <> " : Bool"
  show (Add a b) = "(" <> show a <> ") + (" <> show b <> ") : Int"
  show (Mul a b) = "(" <> show a <> ") * (" <> show b <> ") : Int"
  show (Eq a b) = "(" <> show a <> ") == (" <> show b <> ") : Bool"

evaluate :: Expression a -> a
evaluate (I a) = a
evaluate (B a) = a
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Mul a b) = evaluate a * evaluate b
evaluate (Eq a b) = evaluate a == evaluate b
