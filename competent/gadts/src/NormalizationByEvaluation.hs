{-# LANGUAGE GADTs #-}
-- | Exercises based the Section "Normalization by Evaluation" of the paper
-- "Fun with Phantom Types".

module NormalizationByEvaluation where

import           Control.Monad.State.Lazy
import           Data.Char
-- z :: a
-- y :: a -> b
-- x :: a -> b -> c
s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = (x z) (y z)

k :: a -> b -> a
k x _ = x

i :: a -> a
i x = x

e = (s ((s (k s)) ((s (k k )) i ))) ((s ((s (k s)) ((s (k k )) i ))) (k i ))

-- | Some examples to understand what @reify@ is doing. It seems that it
-- transform a function into a some sort of normal-form (which can be seen as a
-- simplified version of the function):
--
-- The result of @reify (b :→ b) (s k k)@ is @Fun (λa → a)@, which seems to
-- indicate that @s k k@ can be rewrited as the indentity function.
--
-- > s k k :: a -> a
-- > > s k k 10
-- > 10
--
-- The result of reify @(b :→ (b :→ b)) (s (k k) i)@ is @Fun (λa → Fun (λb →
-- a))@ which seems to indicate that @s (k k) i@ is equivalent to a function
-- that returns a function what always output its @a@.
--
-- > s (k k) i :: a -> b -> a
-- > > s (k k) i 10 "hello"
-- > 10
--
-- Finally, the result of @reify ((b :→ b) :→ (b :→ b)) e@ is @Fun (λa → Fun
-- (λb → App a (App a b)))@ which seems to indicate that @e@ can be normalized
-- to a function that takes a function and a value and applies the function
-- first to the value, and then applies it again to the result.
--
-- > e :: (b -> b) -> b -> b
-- > > e (+ 1) 1
-- > 3
-- > > e (++ " bar") "foo"
-- > "foo bar bar"

data Type t where
  RBase :: Type Base
  (:->) :: Type a -> Type b -> Type (a -> b)

-- infixr :->
-- :-> :: Type a -> Type b -> Type (a -> b)
-- :-> = RFun

data Term t where
  App :: (Term (a -> b)) -> Term a -> Term b
  Fun :: (Term a -> Term b) -> Term (a -> b)
  -- | Added for the sake of exercise 12.
  Var :: String -> Term t

-- | Base type of the simply typed lambda-calculus.
--
-- @Base@ is the fixed point of Term.
newtype Base = In {out :: Term Base}

reify :: Type t -> t -> Term t
reify RBase v = out v
--
-- > ra |-> rb :: Type (a -> b)
-- > ra :: Type a
-- > rb :: Type b
-- > v :: (a -> b)
-- > reify (ra |-> rb) v :: Term (a -> b)
--
-- To apply Fun, we need values of type @Term a@ and @Term b@, where do we get
-- them from?
--
-- > Fun (\ta -> tb)
--
-- What is @tb@ here?
-- > reflect ra ta :: a
-- > v a :: b
-- > reify rb b :: Term b
--
-- And this is what we want!
--
reify (ra :-> rb) v =
  Fun f
  where f ta = reify rb b
          where b = v a
                a = reflect ra ta

reflect :: Type t -> Term t -> t
reflect RBase t = In t
reflect (ra :-> rb) t = \x -> reflect rb (App t (reify ra x))
-- > ra |-> rb :: Type (a -> b)
-- > ra :: Type a
-- > rb :: Type b
-- > t :: Term (a -> b)
-- > App t :: Term a -> Term b
--
-- So let's work with what we have:
--
-- > x :: a
-- > reify ra x :: Term a
-- > App t (reify ra x) :: Term b
-- > reflect rb (App t (reify ra x)) :: b

-- Some auxiliary definitions useful for using @reify@ in the REPL.
b :: Type Base
b = RBase

instance Show (Term t) where
  show t = evalState (gshow t) ("a", 'a')

gshow :: Term t -> State (String, Char) String
gshow (Var name) =
  return name
gshow (Fun tatb) = do
  (name, _) <- get
  modify nextVar
  body <- gshow (tatb (Var name))
  return $ "Fun (\\" ++ name ++ " -> " ++ body ++ ")"
gshow (App tab ta) = do
  f <- gshow tab
  x <- gshow ta
  return $ "App " ++ f ++ " (" ++ x ++ ")"

-- | Determines the next character in the sequence from 'a' to 'z'.
next :: Char -> Char
next c = chr $ ord 'a' + (ord c -  ord 'a' + 1) `mod` 26

-- | Determines the next variable name.
nextVar :: (String, Char) -> (String, Char)
nextVar (xs, 'z') = (xs ++ ['a'], 'a')
nextVar (xs, c) = (take (length xs - 1) xs ++ [c'], c')
  where c' = next c
