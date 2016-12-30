-- | Adding phantom types to the simple DSL.
--
-- Example taken from:
--
--   https://en.wikibooks.org/wiki/Haskell/GADT

module ExprDSLPhantom where

data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a)
            | Eq (Expr a) (Expr a)

-- | What if we trued to define the 'eval' expression with this type?
--
-- > eval :: Expr a -> Either Int Bool
-- > eval (I i) = Left i
-- > eval (B b) = Right b
--
-- How do can I make use of phantom types here?
--
-- > eval (Add e0 e1) =  ...
--
-- How do we use 'a' to keep track of the type of the expression?
add :: Expr Int -> Expr Int -> Expr Int
add = Add

i :: Int -> Expr Int
i = I

b :: Bool -> Expr Bool
b = B

-- | Nice thing of these definitions is that the following expression:
--
-- > e1 = (b True) `add` i 5
--
-- Does not type-checks, because add expects two 'Expr Int'.

-- | We cannot just do this:
--
-- > eq :: Expr a -> Expr a -> Expr Bool
-- > eq = Eq
--



-- | Even if we don't use equality we run into the same limitations:
--
-- > eval :: Expr a -> a
-- > eval (I i) = i
--
-- Here the compiler will complain that could not match type 'a' with 'Int'.

-- | What we need is a way to restrict the return types of the constructors
-- themselves (For instance having 'Add' with type 'Expr Int -> Expr Int ->
-- Expr Int' instead of 'Expr a -> Expr a -> Expr a'), and that's exactly what
-- generalized data types do.
