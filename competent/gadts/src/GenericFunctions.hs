{-# LANGUAGE GADTs #-}
-- | Exercises based on Section "Generic Functions" of the paper "Fun with
-- Phantom Types".

module GenericFunctions where
import           Data.Char

-- | Goal: define a function that compresses data of different types without
-- using type-classes.
--
-- We can't just define a function like:
--
-- > compress' :: a -> [Bit]
-- > compress' v = ???
--
-- Since we need to apply the function depending on the type of the value
-- passed as parameter.

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

instance Show Bit where
  show Zero = "0"
  show One = "1"

-- Exercise: using the functions @compressInt@ and @compressChar@ below define
-- the function @compress@.
compressInt :: Int -> [Bit]
compressInt = (take 32) . compressInt'

compressInt' :: Int -> [Bit]
compressInt' n = digit (n `mod` 2) : compressInt' (n `div` 2)
  where
    digit 0 = Zero
    digit 1 = One

compressChar :: Char -> [Bit]
compressChar = (take 7) . compressInt' . ord

compress :: Type t -> t -> [Bit]
compress RInt v = compressInt v
compress RChar c = compressChar c
compress (RList _) [] = Zero : []
compress (RList rt) (x: xs) = (One : (compress rt x)) ++ (compress (RList rt) xs)
compress (RPair rt0 rt1) (x, y) = (compress rt0 x) ++ (compress rt1 y)

-- | Exercise 3 from the paper: implement generic equality:
--
-- Examples:
--
-- > eq (RList RChar) "foo" "bar
-- > eq (RList RChar) "foo" "foo"

eq :: Type t -> t -> t -> Bool
eq RInt x y = x == y
eq RChar x y = x == y
eq (RList _) [] [] = True
eq (RList _) [] (_: _) = False
eq (RList _) (_:_) [] = False
eq (RList rt) (x:xs) (y:ys) = (eq rt x y) && eq (RList rt) xs ys
eq (RPair rt0 rt1) (x0, y0) (x1, y1)= eq rt0 x0 x1 && eq rt1 y0 y1

-- | What if we want to uncompress the data?
uncompress :: Type t -> [Bit] -> t
uncompress RInt xs = gUncompress xs 0 0
  where
    gUncompress :: [Bit] -> Int -> Int -> Int
    gUncompress [] _ _ = 0
    gUncompress [d] acc len = acc - (undigit d) * 2 ^ len
    gUncompress (d:ds) acc len = gUncompress ds (acc + (undigit d) * 2 ^ len) (len+1)
    undigit Zero = 0
    undigit One = 1
uncompress RChar xs = chr n
  where n = uncompress RInt (xs++[Zero]) -- We add an extra zero at the end
                                         -- since otherwise the function
                                         -- uncompress will take the complement
                                         -- at the 7th bit.
