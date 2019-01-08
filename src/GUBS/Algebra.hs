{-# LANGUAGE DerivingStrategies #-}
module GUBS.Algebra where


import           Control.Monad                   (guard)
import           Data.Foldable                   (toList)
import           Data.Ratio                      (Ratio)
import qualified Data.Ratio                      as R (denominator, numerator, (%))
import           Text.ParserCombinators.ReadP    (char)
import           Text.ParserCombinators.ReadPrec
import qualified Text.PrettyPrint.ANSI.Leijen    as PP
import           Text.Read
import           Text.Read.Lex


zero, one :: Num a => a
zero = fromInteger 0
one  = fromInteger 1

num :: (Integral b, Num a) => b -> a
num = fromIntegral

sumA, prod :: (Foldable f, Num a) => f a -> a
sumA (toList -> []) = zero
sumA (toList -> l)  = foldr1 (+) l
prod (toList -> []) = one
prod (toList -> l)  = foldr1 (*) l

class Num a => Max a where
  maxA :: a -> a -> a

  maximumA ::(Foldable f) => f a -> a
  maximumA (toList -> []) = zero
  maximumA (toList -> l)  = foldr1 maxA l

instance Max Integer where
  maxA = max


-- Q

newtype Q = Q { ratio :: Rational }
  deriving newtype (Eq, Ord, Num, Fractional, Real, Show, Read)

instance Max Q where
  maxA = max

(%) :: Integer -> Integer -> Q
n % d = Q $ n R.% d

numerator, denominator :: Q -> Integer
numerator   = R.numerator   . ratio
denominator = R.denominator . ratio

toRat :: Real a => a -> Q
toRat = Q . toRational

showQ :: Q-> String
showQ q
  | n == 0    = "0"
  | d == 1    = show n
  | otherwise = show (ratio q)
  where
    n = numerator q
    d = denominator q

-- read assignment (ident number)
readAssignment :: String -> Q
readAssignment = either error id . readEither' readPrec'
  where readPrec' = parens (lexP *> readPrecQ)

readQ :: String -> Q
readQ = either error id . readEither' readPrecQ

readPrecQ  :: ReadPrec Q
readPrecQ = readDouble +++ readRational +++ parens (readRat) where
  number     = do
    l <- lexP
    case l of
      Number n -> return $ numberToRational n
      _        -> pfail

  -- readInteger  = fromInteger <$> (readPrec :: ReadPrec Integer)
  readRational = Q           <$> (readPrec :: ReadPrec Rational)
  readDouble   = toRat       <$> (readPrec :: ReadPrec Double)
  readRat      = do
    _ <- lift (char '/')
    n <-  number
    guard (R.denominator n == 1)
    d <- number
    guard (R.denominator d == 1)
    return $ R.numerator n % R.numerator d


readEither' :: Show a => ReadPrec a -> String -> Either String a
readEither' a s =
  case [ x | (x,"") <- readPrec_to_S a minPrec s ] of
      [x] -> Right x
      []  -> Left $ "Prelude.read: no parse: " ++  s
      xs  -> Left $ "Prelude.read: ambiguous parse:" ++ s ++ ":" ++ show xs
  where

instance PP.Pretty Q where
  pretty = PP.text . showQ

