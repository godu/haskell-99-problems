-- https://doisinkidney.com/posts/2020-10-17-ski.html#ref-wildenhainTuringCompletenessMS2017

module LambdaCalculus.BAMT where

b x y z = x $ y z

a x y = y

-- m x = x x

t x y = y x


-- Const
s = b (t a) (b b t)
