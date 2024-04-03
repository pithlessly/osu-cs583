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
