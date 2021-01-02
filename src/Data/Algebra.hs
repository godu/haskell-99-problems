-- https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/
{-# LANGUAGE DeriveFunctor #-}

module Data.Algebra where

data ExprF a
  = Const Int
  | Add a a
  | Mul a a
  deriving (Functor)

newtype Fix f = In (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (In x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

alg :: ExprF Int -> Int
alg (Const i) = i
alg (x `Add` y) = x + y
alg (x `Mul` y) = x * y

eval :: Fix ExprF -> Int
eval = cata alg

testExpr =
  In $
    In
      ( In (Const 2)
          `Add` In (Const 3)
      )
      `Mul` In (Const 4)

main = print $ eval testExpr
