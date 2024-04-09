import Data.List (sort)

-- =========== --
-- Exercise #1 --
-- =========== --

-- remove consecutive equal elements
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : uniq (dropWhile (== x) xs)

-- given target, find all x in a list such that (x + y == target) for some y in the list.
-- note that this returns the elements in a different order from the given example
-- and doesn't include duplicates.
canSumTo :: Int -> [Int] -> [Int]
canSumTo target = (\xs -> walk xs (reverse xs)) . uniq . sort
  where
    walk (low : lows) (high : highs) =
      case compare (low + high) target of
        EQ -> low : walk lows         highs
        GT ->       walk (low : lows) highs
        LT ->       walk lows         (high : highs)
    walk _ _ = []

-- difference of consecutive elements
diff :: [Int] -> [Int]
diff (x1 : x2 : xs) = (x2 - x1) : diff (x2 : xs)
diff _ = []

-- =========== --
-- Exercise #2 --
-- =========== --

type Row a = [a]
type Table a = [Row a]

type Pic = Table Bool

mapT :: (a -> b) -> Table a -> Table b
mapT = map . map

mergeTables :: (a -> b -> c) -> Table a -> Table b -> Table c
mergeTables = zipWith . zipWith

union :: Pic -> Pic -> Pic
union = mergeTables (||)

shiftR :: Pic -> Pic
shiftR = map rowR
  where rowR r = False : init r
