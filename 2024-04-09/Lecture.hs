module Lecture where

class BiFunctor f where
    bmap :: (a1 -> a2) -> (b1 -> b2) -> f a1 b1 -> f a2 b2

instance BiFunctor (,) where
    bmap f g (x, y) = (f x, g y)

mapL  :: BiFunctor f => (a -> c) -> f a b -> f c b
mapR  :: BiFunctor f => (b -> c) -> f a b -> f a c
mapLR :: BiFunctor f => (a -> b) -> f a a -> f b b

mapL  f = bmap f id
mapR  f = bmap id f
mapLR f = bmap f f

newtype BiCompose f g a b = BiCompose (f (g a b))

instance (Functor f, BiFunctor g) => BiFunctor (BiCompose f g) where
    bmap f g (BiCompose x) = BiCompose (fmap (bmap f g) x)

data Graph a b = G [a] [(Int, Int, b)]

instance BiFunctor Graph where
    bmap f g (G nodes edges) = G (map f nodes) (map (\(n1, n2, edge) -> (n1, n2, g edge)) edges)

-- mapL: map nodes
-- mapR: map values
