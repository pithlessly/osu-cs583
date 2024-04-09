module Lecture where

class BiFunctor f where
    bmap :: (a1 -> a2) -> (b1 -> b2) -> f a1 b1 -> f a2 b2

instance BiFunctor (,) where
    bmap f g (x, y) = (f x, g y)
