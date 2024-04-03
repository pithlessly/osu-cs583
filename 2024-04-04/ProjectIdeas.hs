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
data RrbAPI a = RrbAPI {
    _empty :: Rrb a,
    _singleton :: a -> Rrb a,
    _fromList :: [a] -> Rrb a,
    _cons :: a -> Rrb a -> Rrb a,
    _snoc :: Rrb a -> a -> Rrb a,
    _concat :: Rrb a -> Rrb a -> Rrb a,
    _index :: Int -> Rrb a -> a,
    _length :: Rrb a -> Int,
    _traverse :: forall f b. Applicative f => (a -> f b) -> Rrb a -> f (Rrb b)
  }

-- type In = ()
-- type Out = forall a. RrbAPI a
-- classProject () = RrbAPI { {- TODO -} }
