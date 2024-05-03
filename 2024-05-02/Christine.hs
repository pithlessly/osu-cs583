{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

-- =========== --
-- Exercise #8 --
-- =========== --

import Control.Monad (liftM, ap, replicateM)

data State s a = State (s -> (a,s))

instance Functor     (State s) where fmap = liftM
instance Applicative (State s) where pure = return; (<*>) = ap
instance Monad       (State s) where
  return x = State (\s->(x,s))
  State c >>= f = State (\s->let (x,s') = c s
                                 State d = f x
                              in d s')

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

type Counter = Int
type Counting a = State Counter a

reset = modify (const 0)
incr  = add 1
get   = State (\c -> (c, c))
add n = modify (n +)

type Stack a = State [a]

new       = modify (\_ -> [])
push x    = modify (x :)
pop       = modify tail
top       = State (\s -> (head s, s))
topop     = State (\(x:s) -> (x, s))
binOp (!) = modify (\(x:y:s) -> (y ! x) : s)

-- =========== --
-- Exercise #9 --
-- =========== --

newtype Parser a = P { papply :: String -> [(a, String)] }

instance Functor     Parser where fmap = liftM
instance Applicative Parser where pure = return; (<*>) = ap
instance Monad       Parser where
  return x = P (\s -> [(x, s)])
  p >>= k  = P (\s -> do (a, s') <- papply p s; papply (k a) s')

(<<<) = papply
char :: Char -> Parser Char
char c = P (\s -> case s of c':s' | c' == c -> [(c',s')]
                            _               -> [])

rep :: Int -> Parser a -> Parser [a]
rep = replicateM

orElse :: Parser a -> Parser a -> Parser a
p `orElse` q = P (\s ->
  case papply p s of [] -> papply q s
                     v -> v)
