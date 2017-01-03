{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs     #-}
-- | Example taken from this answer:
--
--    http://stackoverflow.com/a/41433732/2289983

module SoSimple where

import           Data.Void

data SoSimple a where
  SoSimple :: SoSimple Int

eval :: SoSimple a -> a
-- eval :: SoSimple a -> Int
eval SoSimple = 3

data SomeSimple where
    SomeSimple :: SoSimple a -> SomeSimple

-- typechecks if eval :: SoSimple a -> Int,
--    but not if eval :: SoSimple a -> a
-- evalSome :: SomeSimple -> Int
-- evalSome (SomeSimple x) = eval x

-- typechecks if eval :: SoSimple a -> a,
--    but not if eval :: SoSimple a -> Int
evalNone :: SoSimple Void -> Void
evalNone = eval
