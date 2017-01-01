-- | A first example on how type kinds allow us to re-use abstractions.

module HigherTrees where

data Tree f a = Leaf a
              | Tree (f (Tree f a))

data Pair a = MkPair a a

type BinTree a = Tree Pair a
type RoseTree a = Tree [] a

t0 :: BinTree String
t0 = Tree (MkPair (Leaf "foo") (Leaf "bar"))
