{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
-- | Exercises based on the Section "Generic traversals and queries" of the
-- paper "Fun with Phantom Types".
module GenericTraversals where

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


-- | Do we need higher rank polymorphism at all?
type Traversal = forall t . Type t -> t -> t

-- | Increment the age of a person.
tick :: Name -> Traversal
tick name RPerson p =
  if (getName p == name)
  then p {getAge = getAge p + 1}
  else p
tick _ _ p = p

-- | The combinator `everywhere` implements the generic part of traversal.

-- | The identity traversal:
copy :: Traversal
copy _ = id

-- | Composition of traversals:
(°) :: Traversal -> Traversal -> Traversal
(f ° g) rt = f rt . g rt

-- | Composing traversals is similar to function composition. The only
-- difference is that you don't have to pass the type around.
p0' = (tick (Name "Alice") ° tick (Name "Alice")) RPerson (ps !! 0)

-- | Define a function that applies the a traversal @f@ to the immediate
-- components:
imap :: Traversal -> Traversal
imap _ RInt i = i
imap _ RChar c = c
imap _ (RList _) [] = []
imap f (RList rt) (x:xs) = f rt x : f (RList rt) xs
imap f (RPair rt0 rt1) (x, y) = (f rt0 x, f rt1 y)
imap f RPerson (Person nname aage) = Person newName newAge
  where Name name = nname
        Age age = aage
        newName = Name $ f rString name
        newAge = Age $ f RInt age

everywhere :: Traversal -> Traversal
everywhere f = f ° imap (everywhere f)
-- | What if we would have defined everywhere as:
-- > everywhere f = imap (everywhere f)
-- Well f does not get applied

-- | Now, check out this varianat of everywhere:
everywhere' :: Traversal -> Traversal
everywhere' f = imap (everywhere' f) ° f

ps :: [Person]
ps = [ Person (Name "Alice") (Age 25)
     , Person (Name "Bob") (Age 10)
     ]

-- @everywhere@ should be defined in such a way that:
ps' = everywhere (tick (Name "Bob")) (RList RPerson) ps

-- what if we apply @imap@?
ps'' = imap (tick (Name "Bob")) (RList RPerson) ps
ps''' = imap (tick (Name "Alice")) (RList RPerson) ps

pt = (ps !! 0, ps !! 1)
pt' = everywhere (tick (Name "Alice")) (RPair RPerson RPerson) pt



