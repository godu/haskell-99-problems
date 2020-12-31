-- https://doisinkidney.com/posts/2020-10-17-ski.html#ref-wildenhainTuringCompletenessMS2017

module LambdaCalculus.BCKW where

-- Const
k x y = x

-- Duplicate
w x y = x y y

-- Flip
c x y z = x z y

-- Composition
b x y z = x (y z)


-- Identity
id = c k c

t = b c c

s = b (b (b w) c) (b b)

bb = b b
