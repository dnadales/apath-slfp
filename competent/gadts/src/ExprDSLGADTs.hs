{-# LANGUAGE GADTs #-}
-- | Simple embedded DSL using GADT's.
--
-- Example adapted from:
--
--  https://en.wikibooks.org/wiki/Haskell/GADT

module ExprDSLGADTs where


data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq :: (Eq a) => Expr a -> Expr a -> Expr Bool

eRight = (I 5 `Add` I 1) `Eq` I 7

-- | And here we see that the use of GADT's prevent us from writting these
-- expressions.
--
-- > eWrong = (B True) `Add` I 5 -- Won't type check since 'B True' does not
-- >                             -- have type 'Expr Int'!
--

-- | How to define an evaluator? Let see...
eval :: Expr a -> a
eval (I i) = i
eval (B b) = b
eval (Add e0 e1) = eval e0 + eval e1
eval (Eq e0 e1) = eval e0 == eval e1

-- | Note that @Expr@ is quite unusual: it is parametrized but it is not a container!
--
--   - @Expr Int@ is an expresion that evaluates to an @Int@, not a container
--     for an integer.
--
--   - @Expr b@ could be inhabited for some @b@: for instance @Expr String@.
--
--   - It makes no sense (and we cannot) define a function with type @(a ->b)
--   -> Expr a -> Expr b@ (How would you transform a @Expr Int@ into a @Expr
--   Bool@?).
--

mapExpr :: (a -> b) -> Expr a -> Expr b
mapExpr f e = undefined -- How would you define this?

-- | Do we need to define the type of a function operating on Phantom Types
-- explicitly?
--
-- Note that there are two possible types to this function.
--f :: Expr a -> String
whatIs :: Expr Int -> String
whatIs (I _) = "an integer expression"
whatIs (Add _ _) = "an adition operation"
