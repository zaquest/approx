{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}
module Approx
     ( Approx(..)
     , approxCompare
     , (~=)
     , (<~=)
     , (>~=)
     , (/~=)
     , module Linear.Epsilon
     ) where

import Linear.Epsilon
import Data.Int
import Data.Word

(~=) :: Epsilon a => a -> a -> Bool
x ~= y = nearZero (x - y)
{-# INLINE (~=) #-}

approxCompare :: (Ord a, Epsilon a) => a -> a -> Ordering
approxCompare x y | x ~= y = EQ
                  | otherwise = compare x y

(<~=), (>~=), (/~=) :: (Ord a, Epsilon a) => a -> a -> Bool
x <~= y = let o = approxCompare x y in o == EQ || o == LT
x >~= y = let o = approxCompare x y in o == EQ || o == GT
x /~= y = not (x ~= y)
{-# INLINE (/~=) #-}

-- | Usually you would want to use one of `~=`, `<~=` or `>~=`
-- operators, but sometimes you would want to do something like
-- @x `elem` xs@ that is where this newtype is useful instead of
-- writing your own `elem` that uses `~=` you wrap your data in
-- this newtype @Approx x `elem` (Approx <$> xs)@
newtype Approx a = Approx { toExact :: a }
  deriving (Show, Num, Enum, Bounded, Integral, Fractional, Real, RealFrac, RealFloat, Floating)

instance Epsilon a => Eq (Approx a) where
  (Approx x) == (Approx y) = nearZero (x - y)
  {-# INLINE (==) #-}

instance (Ord a, Epsilon a) => Ord (Approx a) where
  compare ax@(Approx x) ay@(Approx y) | ax == ay = EQ
                                      | otherwise = compare x y

#define EPSILONI(T) instance Epsilon T where { nearZero = (== 0); {-# INLINE nearZero #-} }
EPSILONI(Integer)
EPSILONI(Int)
EPSILONI(Int8)
EPSILONI(Int16)
EPSILONI(Int32)
EPSILONI(Int64)
EPSILONI(Word)
EPSILONI(Word8)
EPSILONI(Word16)
EPSILONI(Word32)
EPSILONI(Word64)
