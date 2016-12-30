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
