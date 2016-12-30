{-# LANGUAGE GADTs #-}
-- | Simple embedded DSL using GADT's.

module ExprDSLGADTs where


data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr a -> Expr a -> Expr a

eRight = (I 5 `Add` I 1) `Eq` I 7

-- | And here we see that the use of GADT's prevent us from writting these
-- expressions.
--
-- > eWrong = (B True) `Add` I 5 -- Won't type check since 'B True' does not
-- >                             -- have type 'Expr Int'!
--
