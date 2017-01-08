{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
-- | Exercises based the Section "Dynamic Types" of the paper "Fun with Phantom
-- Types".

module DynamicValues where

-- | Remember our Type t.
data Type t where
  RInt :: Type Int
  RChar :: Type Char
  RList :: Type a -> Type [a]
  RPair :: Type a -> Type b -> Type (a, b)
--  RDyn :: Type Dynamic -- | We introduce a new type.
  RDyn :: Type (Dynamic t)


-- | Here is how we can define "existential types" apparently: a dynamic value
-- is a pair consisting of a type representation of @Type t@ and a value of
-- type t *for some type @t@*.
--data Dynamic = forall t. Dyn (Type t) t
--
-- Why not just:
data Dynamic t = Dyn (Type t) t
