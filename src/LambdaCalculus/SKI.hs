-- https://doisinkidney.com/posts/2020-10-17-ski.html#ref-wildenhainTuringCompletenessMS2017

module LambdaCalculus.SKI where

s x y z = x z (y z)

k x y = x

i = s k k
