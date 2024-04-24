import Control.Monad (ap)

-- =========== --
-- Exercise #7 --
-- =========== --

-- requirement: choice list is not a singleton
data Decision a = Certain a | Choice [a] deriving (Show)

toChoices :: Decision a -> [a]
toChoices (Certain x) = [x]
toChoices (Choice xs) = xs

instance Functor Decision where
  fmap f (Certain x) = Certain (f x)
  fmap f (Choice xs) = Choice (map f xs)

instance Applicative Decision where
  pure = Certain
  (<*>) = ap

join :: Decision (Decision a) -> Decision a
join (Certain x) = x
join (Choice cs) = let certains = [c | Certain c <- cs]
                       choices  = [c | Choice cs' <- cs, c <- cs']
                   in
                   case certains of
                     []  -> Choice choices
                     [b] -> Certain b
                     bs  -> Choice bs

instance Monad Decision where
  return = pure
  x >>= f = join (fmap f x)

-- Verification of the monad laws
-- ==============================
--
-- 1. join . pure = id :: Decision a -> Decision a
--
--        join (pure x)
--     == join (Certain x)  -- definition of pure
--     == x                 -- definition of join
--
-- 2. join . fmap pure = id :: Decision a -> Decision a
--
--        join (fmap pure (Certain x))
--     == join (Certain (pure x))       -- definition of fmap
--     == pure x                        -- definition of join
--     == Certain x                     -- definition of pure
--
--        join (fmap pure (Choice xs))
--     == join (Choice (map pure xs))       -- definition of fmap
--     == join (Choice (map Choice xs))     -- definition of pure
--     == let certains = xs; choices  = []  -- definition of join
--        in case certains of
--             []  -> Choice choices
--             [b] -> Certain b
--             bs  -> Choice bs
--     == case xs of                        -- substitute
--          [] -> Choice []
--          b  -> Certain b
--          bs -> Choice bs
--     == case xs of                        -- note that xs is not a singleton
--          [] -> Choice []
--          bs -> Choice bs
--     == Choice xs                         -- case analysis
--
-- 3. join . join = join . fmap join :: Decision (Decision (Decision a)) -> Decision a
--
--        join (fmap join (Certain (Certain d)))
--     == join (Certain (join (Certain d)))      -- definition of fmap
--     == d                                      -- definition of join
--
--        join (join (Certain (Certain d)))
--     == join (Certain d)                       -- definition of join
--     == d                                      -- definition of join
--
--        join (fmap join (Certain (Choice ds)))
--     == join (Certain (join (Choice ds)))      -- definition of fmap
--     == join (Choice ds)                       -- definition of join
--
--        join (join (Certain (Choice ds)))
--     == join (Choice ds)                       -- definition of join
--
--        join (join (Choice dss))
--        -- This one might not hold.
--        -- The equational reasoning is more complex.
