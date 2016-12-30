{-# LANGUAGE GADTs #-}
-- | Safe lists in Haskell.
--
-- Example taken from:
--
--  https://en.wikibooks.org/wiki/Haskell/GADT

module SafeList where

data Empty
data NonEmpty

data SafeList a b where
  SNil :: SafeList a Empty
  SCons :: a -> SafeList a b -> SafeList a NonEmpty

sHead :: SafeList a NonEmpty -> a
sHead (SCons x _) = x

xs :: SafeList String NonEmpty
xs = "foo" `SCons` ("bar" `SCons` SNil)

-- | Note that we can call 'sHead xs' but 'sHead SNil' won't even compile!

-- | We cannot type this function!
--
-- > silly True = SCons "BlaBla" SNil
-- > silly False = SNil
--
-- It seems we cannot produce both empty and non-empty lists!
--
-- Adding a type declaration like:
--
-- > silly :: Bool -> SafeList String b
--
-- Won't work, since we have to return a something of type 'b' for all 'b'!
