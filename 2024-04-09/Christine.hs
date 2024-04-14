import Data.Maybe (fromMaybe)

-- =========== --
-- Exercise #3 --
-- =========== --

data BinTree a = Branch a (BinTree a) (BinTree a)
               | Empty

foldT :: b -> (a -> b -> b -> b) -> BinTree a -> b
foldT e b Empty = e
foldT e b (Branch x l r) = b x (foldT e b l) (foldT e b r)

sumT, maxT :: BinTree Int -> Int
sumT = foldT 0 (\n x y -> n + x + y)
maxT = fromMaybe (error "empty tree") .
       foldT Nothing (\a x y -> maximum [Just a, x, y])

levels :: BinTree a -> [[a]]
levels = takeWhile (not . null) .
         foldT (repeat []) (\a l r -> [a] : zipWith (++) l r)

-- =========== --
-- Exercise #4 --
-- =========== --

data RTree a = Node a [RTree a]

foldR :: (a -> [b] -> b) -> RTree a -> b
foldR f (Node x ns) = f x (map (foldR f) ns)

mapR :: (a -> b) -> RTree a -> RTree b
mapR f = foldR (\a bs -> Node (f a) bs)

dfs :: RTree a -> [a]
dfs = foldR (\x xss -> x : concat xss)
