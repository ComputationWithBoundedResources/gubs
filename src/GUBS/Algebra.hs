module GUBS.Algebra where

import Data.Foldable (toList)

zero, one :: Num a => a
zero = fromInteger 0
one  = fromInteger 0

num :: (Integral b, Num a) => b -> a
num = fromIntegral

sumA, prod :: (Foldable f, Num a) => f a -> a
sumA (toList -> []) = zero
sumA (toList -> l)  = foldr1 (+) l
prod (toList -> []) = one
prod (toList -> l)  = foldr1 (*) l

class Max a where
  maxA :: a -> a -> a

instance Max Integer where
  maxA = max


-- infixr 8 .^
-- infixl 7 .*
-- infixl 6 .+, .-


-- class Additive a where
--   (.+) :: a -> a -> a
--   zero :: a
  
--   sumA :: Foldable f => f a -> a
--   sumA (toList -> []) = zero
--   sumA (toList -> l) = foldr1 (.+) l
  
-- class Additive a => AdditiveGroup a where
--   neg :: a -> a
--   neg a = zero .- a
--   (.-) :: a -> a -> a
--   a .- b = a .+ neg b

-- class Multiplicative a where
--   (.*) :: a -> a -> a
--   one  :: a
  
--   prod :: Foldable f => f a -> a
--   prod (toList -> []) = one
--   prod (toList -> l) = foldr1 (.*) l

--   (.^) :: Integral b => a -> b -> a
--   a .^ b = prod (replicate (fromIntegral b) a)

-- type SemiRing a = (Additive a,      Multiplicative a)
-- type Ring  a    = (AdditiveGroup a, Multiplicative a)


-- class Additive a => Max a where
--   maxA :: a -> a -> a

--   maximumA :: Foldable f => f a -> a
--   maximumA (toList -> []) = zero
--   maximumA (toList -> l)  = foldr1 maxA l

  
-- class IsNat a where
--   fromNatural_ :: Integer -> a

-- fromNatural :: (Integral a, IsNat b) => a -> b
-- fromNatural = fromNatural_ . fromIntegral 


-- -- instances

-- instance IsNat Integer where
--   fromNatural_ i = i

-- instance Additive Integer where
--   (.+) = (+)
--   zero = 0

-- instance Max Integer where
--   maxA = max

-- instance AdditiveGroup Integer where
--   neg = negate
--   (.-) = (-)
  
-- instance Multiplicative Integer where
--   (.*) = (*)
--   one = 1
  
