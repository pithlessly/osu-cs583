module ProjectIdeas where

type Var = String

data Formula
  = Var Var
  | Not Formula
  | Or Formula Formula
  | And Formula Formula
  deriving (Show)

satisfiable :: Formula -> Maybe [(Var, Bool)]
satisfiable = undefined

satExamples :: [(Formula, Maybe [(Var, Bool)])]
satExamples =
  [( a,
     Just [("a", True)] ),

   ( Not a,
     Just [("a", False)] ),

   ( a && Not a,
     Nothing ),

   ( Not a && b,
     Just [("a", False), ("b", True)] ),

   ( (a || Not b) &&
     (c || Not b) &&
     (Not a || Not c) &&
     (b || a),
     Just [("a", True), ("b", False), ("c", True)] )
  ]
  where (&&) = And; (||) = Or; (a, b, c) = (Var "a", Var "b", Var "c")

-- type In = Formula
-- type Out = Maybe [(Var, Bool)]
-- classProject = satisfiable

data Rrb a = Rrb {- abstract -}

empty :: Rrb a
empty = Rrb

singleton :: a -> Rrb a
singleton _ = Rrb

fromList :: [a] -> Rrb a
fromList _ = Rrb

toList :: Rrb a -> [a]
toList _ = []

cons, snoc :: a -> Rrb a -> Rrb a
cons _ _ = Rrb
snoc _ _ = Rrb

appendV :: Rrb a -> Rrb a -> Rrb a
appendV _ _ = Rrb

lengthV :: Rrb a -> Int
lengthV _ = 0

index :: Int -> Rrb a -> a
index _ _ = error "out of bounds"

traverseV :: Applicative f => (a -> f b) -> Rrb a -> f (Rrb b)
traverseV _ _ = pure Rrb

tests :: [IO Bool]
tests =
  [ test "empty" ==> (
      toList empty == ([] :: [Int]))

  , test "singleton" int ==> \n ->
      toList (singleton n) == [n]

  , test "fromList" list ==> \xs ->
      toList (fromList xs) == xs

  , test "cons" int rrb ==> \x xs ->
      toList (cons x xs) == x : toList xs

  , test "snoc" int rrb ==> \x xs ->
      toList (snoc x xs) == toList xs ++ [x]

  , test "append" rrb rrb ==> \xs ys ->
      toList (appendV xs ys) == toList xs ++ toList ys

  , test "length" rrb ==> \xs ->
      lengthV xs == length (toList xs)

  , test "index" int rrb ==> \i xs ->
      let xs' = toList xs in
      not (0 <= i && i < length xs') ||
        (index i xs == xs' !! i)

  , test "traverse" rrb ==> \xs ->
      let f n = (show n, n + 1) in
      (toList <$> traverseV f xs) == traverse f (toList xs)
  ]
  where
    int  = testArgument "an integer"         (\x -> x :: Int)
    list = testArgument "a list of integers" (\x -> x :: [Int])
    rrb  = testArgument "a vector"           (\x -> fromList x :: Rrb Int)

-- ================================= --
-- property testing fluent interface --
-- ================================= --

type Sat a = (a -> a) -> a
type Chain a b = (String, IO (a -> b))

test :: String -> (Chain a a -> r) -> r
test name = \k -> k (name, pure applyNoArgs)
  where applyNoArgs = id

testArgument :: Read s => String -> (s -> t) -> Chain a (t -> b) -> (Chain a b -> r) -> r
testArgument ty conv (name, mkApplicator) = \k -> k (name, do
  applyArgs <- mkApplicator
  putStr $ "Please enter " ++ ty ++ ": "
  v <- conv . read <$> getLine
  pure (($ v) . applyArgs))

(==>) :: Sat (Chain a Bool) -> a -> IO Bool
chain ==> f = do
  let (name, mkApplicator) = chain id
  putStrLn $ let s = "Test " ++ show name in replicate (length s) '=' ++ "\n" ++ s
  applyArgs <- mkApplicator
  let success = applyArgs f
  putStrLn $ "Test " ++ show name ++ if success then " passed" else " failed"
  pure success
