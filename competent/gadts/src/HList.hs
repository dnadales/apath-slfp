{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
-- | Heterogeneous lists in Haskell.

module HList where


-- | Heterogeneous lists with GADTs (and DataKinds, and TypeOperators).
--
--
data HList tys where -- `tys` denotes the types of all the elements in the list!
  HNil  :: HList '[]
  (:>) ::  h -> HList t  -> HList (h ': t)

infixr 5 :>

-- | Using HList we can we can write heterogeneous lists like this one.

xs0 :: HList ([String, Int])
xs0 = "foo" :> 1 :> HNil

xs1 = "bar" :> xs0 :> HNil

xs2 = True :> () :> [Just 7] :> "Hi" :> HNil

hhead :: HList (ty: tys) -> ty
hhead (x :> _) = x

hsize :: HList tys -> Int
hsize HNil = 0
hsize (_ :> xs) = 1 + hsize xs

-- | To make this work you need "PromotedDatatypes"
-- Also, you need to enable TypeOperators, DataKinds, PolyKinds
--
-- To get the trick working you use data-kinds.

-- | Also you can use partial-type signatures (type holes). To enable this use
-- the language extension 'PartialTypeSignature'.

-- | Now, how would you make a function 'get' for 'HList'? You can't even type
-- it? How can you know which type it will return? We need to define another
-- type.

data Elem list elt where
  EZ :: Elem (x ': xs) x
  ES :: Elem xs x -> Elem (y ': xs) x

-- | 'Elem xs x' allows to *select* a type in a list of types.

-- | The first constructor selects the first element in a list of types. The
-- second one selects the next element.

-- | To try this in the ghci REPL,
--
--      ghci> :set -XDataKinds
--      ghci> :t EZ :: Elem '[Bool, Int] Bool
--
-- Try also
--
--      ghci> :t EZ :: Elem '[Bool, Int] Int
--
-- and see what happens.
--
-- Also look at the types of the following expressions:
--
-- > ES EZ :: Elem (y : x : xs) x
-- > ES (ES EZ) :: Elem (y : y1 : x : xs) x
-- > ES (ES (ES EZ)) :: Elem (y : y1 : y2 : x : xs) x
--


-- | Now how could we use 'Elem' to write our get function?
get :: Elem tys ty -> HList tys -> ty
get EZ (x :> _) = x
get (ES tys) (_ :> xs) = get tys xs

example = get (ES EZ) ("hello" :> 2 :> HNil)
