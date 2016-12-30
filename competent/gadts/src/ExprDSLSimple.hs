-- | Simple embedded DSL for simple Boolean expresions.
--
-- Example taken from:
--
--  https://en.wikibooks.org/wiki/Haskell/GADT

module ExprDSLSimple where

data Expr = I Int
          | B Bool
          | Add Expr Expr
          | Eq Expr Expr

eval :: Expr -> Maybe (Either Int Bool)
eval (I i) = Just (Left i)
eval (B b) = Just (Right b)
eval (Add e0 e1) = do
  ev0 <- eval e0
  ev1 <- eval e1
  case ev0 of
    Left i0 ->
      case ev1 of
        Left i1 -> return (Left (i0 + i1))
        Right _ -> Nothing
    Right _ -> Nothing
eval (Eq e0 e1) = do
  ev0 <- eval e0
  ev1 <- eval e1
  case ev0 of
    Left i0 ->
      case ev1 of
        Left i1 -> return (Right (i0 == i1))
        Right _ -> Nothing
    Right b0 ->
      case ev1 of
        Left _ -> Nothing
        Right b1 -> return (Right (b0 == b1))

-- * Some example expressions
eRight = (I 5 `Add` I 1) `Eq` I 7
eWrong = B True `Add` I 5
