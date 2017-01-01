{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
-- | An illustration of a problem with kinds.

module WeedyKinds where

-- | Without having poly-kinds and kind signatures we cannot define type @T0@
-- below:
--
-- > data T f a = MkT (f a)
-- > type T0 = T Maybe Int
--
-- > data F f = MkF (f Int)
--
-- > type T1 = T F Maybe
--
-- Using @PolyKinds@ solves the problem above.

data T (f :: k -> *) a = MkT (f a)
type T0 = T Maybe Int

data F f = MkF (f Int)
type T1 = T F Maybe

e0 :: T0
e0 = MkT (Just 0)

e1 :: T1
e1 = MkT (MkF (Just 0))
