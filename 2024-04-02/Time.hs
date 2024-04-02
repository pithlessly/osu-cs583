module Time where

import Data.Function (on)

data Time = Seconds Int
          | Minutes Int

secs :: Time -> Int
secs (Seconds s) = s
secs (Minutes m) = m * 60

instance Eq Time where
  (==) = (==) `on` secs

data IntSet = ESet | Ins Int IntSet

toList :: IntSet -> [Int]
toList ESet = []
toList (Ins i s) = insert i (toList s) where
  insert i [] = [i]
  insert i (n : ns)
    | i < n  = i : n : ns
    | i > n  = n : insert i ns
    | i == n = n : ns

-- potential instance:
-- (==) = (==) `on` toList

member :: Int -> IntSet -> Bool
member n ESet = False
member n (Ins m s) = (n == m) || member n s

subset :: IntSet -> IntSet -> Bool
subset ESet _ = True
subset (Ins n s) s' = member n s' && subset s s'

instance Eq IntSet where
  s1 == s2 = subset s1 s2 && subset s2 s1
