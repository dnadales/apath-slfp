-- | An illustration of a problem with kinds.

module WeedyKinds where

data T f a = MkT (f a)
type T0 = T Maybe Int

data F f = MkF (f Int)
-- | We cannot define this type
--
-- > type T1 = T F Maybe

