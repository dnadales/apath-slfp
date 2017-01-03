{-# LANGUAGE GADTs #-}
-- | Exercises based on Section "Generic Functions" of the paper "Fun with
-- Phantom Types".

module GenericFunctions where

-- | Basic idea: define a type whose elements represent types.
data Type t where
  RInt :: Type Int
  RChar :: Type Char
  RList :: Type a -> Type [a]
  RPair :: Type a -> Type b -> Type (a, b)

-- An element @rt@ of type @Type t@ is a representation of @t@.

-- | Exercise: define @tString@ below:
tString :: Type String
-- Solution:
tString = RList RChar

-- | Exercise: define the type for @(String, Int)@.
tStringInt :: Type (String, Int)
tStringInt = RPair tString RInt

-- | The compression function that uses @Type@:
data Bit = Zero | One

-- Exercise: using the functions @compressInt@ and @compressChar@ below define
-- the function @compress@.
compressInt :: Int -> [Bit]
compressInt = undefined

compressChar :: Char -> [Bit]
compressChar = undefined

compress :: Type t -> t -> [Bit]
compress RInt v = compressInt v
compress RChar c = compressChar c
compress (RList _) [] = Zero : []
compress (RList rt) (x: xs) = (One : (compress rt x)) ++ (compress (RList rt) xs)
compress (RPair rt0 rt1) (x, y) = (compress rt0 x) ++ (compress rt1 y)

-- | Exercise 3 from the paper: implement generic equality:
eq :: Type t -> t -> t -> Bool
eq RInt x y = x == y
eq (RList _) [] [] = True
eq (RList _) [] (_: _) = False
eq (RList _) (_:_) [] = False
eq (RList rt) (x:xs) (y:ys) = (eq rt x y) && eq (RList rt) xs ys
-- ... and so on...
