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
