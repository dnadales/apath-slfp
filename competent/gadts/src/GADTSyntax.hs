{-# LANGUAGE GADTs #-}
-- | Syntax of GADT's explained with examples.

module GADTSyntax where

-- | How do we write the following data types using GADT's?
--
-- Maybe:
--
-- > data Maybe a = Nothing | Just a
--
-- List:
--
-- > data List a = Nil | Cons a (List a)
--
-- Rose tree:
--
-- > data RoseTree a = RoseTree a [RoseTree a]
--
data GMaybe a where
  GNothing :: GMaybe a
  GJust :: a -> GMaybe a

data GList a where
  GNil :: GList a
  GCons :: a -> GList a -> GList a

data GRoseTree a where
  GRoseTree :: a -> [GRoseTree a] -> GRoseTree a

-- | When working with GADT's it is useful to think of constructors as
-- functions.

-- | So far in this file we haven't seen anything we cannot do without GADT's:
-- functions for constructor 'Foo a' have 'Foo a' has its return type. GADT's
-- let us control the type of 'Foo' we return (how 'a' gets instantiated).
--
-- Now consider:
--
data TrueGadtFoo a where
  MkTrueGadtFoo :: a -> TrueGadtFoo Int

-- This has no Haskell 98 equivalent.

-- | And remember that you must return the same datatype you're defining!
--
-- > data Foo where
-- >  MkFoo :: Bar Int-- This will not typecheck
--
