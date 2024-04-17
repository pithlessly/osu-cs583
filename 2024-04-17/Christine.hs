data BinTree a = Branch a (BinTree a) (BinTree a)
               | Empty

data RTree a = Node a [RTree a]

-- =========== --
-- Exercise #5 --
-- =========== --

class Tree t where
  root     :: t a -> a
  subtrees :: t a -> [t a]
  isEmpty  :: t a -> Bool

instance Tree BinTree where
  root (Branch x _ _) = x
  root Empty          = error "root of empty tree"
  subtrees (Branch _ l r) = [l, r]
  subtrees Empty          = []
  isEmpty (Branch {}) = False
  isEmpty Empty       = True

instance Tree RTree where
  root (Node x _) = x
  subtrees (Node _ ts) = ts
  isEmpty _ = False

instance Tree [] where
  root (x:_) = x
  root _     = error "root of empty list"
  subtrees (_:xs) = [xs]
  subtrees _      = []
  isEmpty = null

-- =========== --
-- Exercise #6 --
-- =========== --

foldT :: Tree t => (a -> [b] -> b) -> b -> t a -> b
foldT f e t
  | isEmpty t = e
  | otherwise = f (root t) (map (foldT f e) (subtrees t))

dfs :: Tree t => t a -> [a]
dfs t = foldT (\x xss -> \d -> x : foldr id d xss) id t []

sumT :: (Num a, Tree t) => t a -> a
sumT = foldT (\x xs -> x + sum xs) 0
