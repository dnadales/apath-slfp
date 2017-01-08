{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Could we define generic traversals without using rank-n-polymorphism?
--
-- Based on the Section "Generic traversals and queries" of the paper "Fun with
-- Phantom Types".

module GenericTraversalsWithoutPoly where

newtype Name = Name String deriving (Eq, Show)
newtype Age = Age Int deriving (Num, Show)
data Person = Person { getName :: Name
                     , getAge  :: Age
                     } deriving Show

data Type t where
  RInt :: Type Int
  RChar :: Type Char
  RList :: Type a -> Type [a]
  RPair :: Type a -> Type b -> Type (a, b)
  RPerson :: Type Person

rString :: Type String
rString = RList RChar

type Traversal t = Type t -> t -> t

tick :: Name -> Traversal t
tick name RPerson p =
  if (getName p == name)
  then p {getAge = getAge p + 1}
  else p
tick _ _ p = p

(°) :: Traversal t -> Traversal t -> Traversal t
(f ° g) rt = f rt . g rt

ps :: [Person]
ps = [ Person (Name "Alice") (Age 25)
     , Person (Name "Bob") (Age 10)
     ]

p0' = (tick (Name "Alice") ° tick (Name "Alice")) RPerson (ps !! 0)

-- | Define a function that applies the a traversal @f@ to the immediate
-- components:
imap :: Traversal t -> Traversal t
imap _ RInt i = i
imap _ RChar c = c
imap _ (RList _) [] = []
-- | And here is where you need the higher rank!
--
-- In this case we have:
--
-- > imap :: (Type t -> t -> t) -> (Type t -> t -> t)
-- > f :: Type t -> t -> t
-- > RList rt :: Type [a]
-- > rt :: Type a
--
-- This implies that @t ~ [a]@, since @RList rt@ is the first argument of the
-- function @imap f@, @RList rt :: Type [a] ~ Type t@.
--
-- This implies that f :: Type [a] -> [a] -> [a], so this means that we cannot
-- apply @f rt @ to @x@, since @x :: a@.
--
imap f (RList rt) (x:xs) = -- f rt x : f (RList rt) xs
  -- Note that this will work however:
  f (RList rt) xs
