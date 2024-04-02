module Time where

import Data.Function (on)

data Time = Seconds Int
          | Minutes Int

secs :: Time -> Int
secs (Seconds s) = s
secs (Minutes m) = m * 60

instance Eq Time where
  (==) = (==) `on` secs
