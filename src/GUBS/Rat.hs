{-# LANGUAGE DerivingStrategies #-}
module GUBS.Rat where


import           Control.Monad                   (guard)
import           Data.Ratio                      (Ratio)
import qualified Data.Ratio                      as R (denominator, numerator, (%))
import           Text.ParserCombinators.ReadP    (skipSpaces)
import           Text.ParserCombinators.ReadPrec
import qualified Text.PrettyPrint.ANSI.Leijen    as PP
import           Text.Read

import           GUBS.Algebra


newtype Rat = Rat { ratio :: Rational }
  deriving newtype (Eq, Ord, Num, Fractional, Real, Show, Read)

(%) :: Integer -> Integer -> Rat
n % d = Rat $ n R.% d

numerator, denominator :: Rat -> Integer
numerator   = R.numerator   . ratio
denominator = R.denominator . ratio

toRat :: Real a => a -> Rat
toRat = Rat . toRational

show' :: Rat-> String
show' q
  | n == 0    = "0"
  | d == 1    = show n
  | otherwise = show (ratio q)
  where
    n = numerator q
    d = denominator q

read' :: String -> Rat
read' = either error id . readE

readE :: String -> Either String Rat
readE s =
  case [ x | (x,"") <- readPrec_to_S readPrec' minPrec s ] of
      [x] -> Right x
      []  -> Left "Prelude.read: no parse"
      _   -> Left "Prelude.read: ambiguous parse"
  where
  readPrec' = readRat <++ readInteger <++ readRatio <++ readDouble <++ pfail where
    readInteger = fromInteger <$> (readPrec :: ReadPrec Integer)
    readDouble  = toRat       <$> (readPrec :: ReadPrec Double)
    readRat     = do
      k <- get
      guard $ k == '/'
      whitespace
      n <- readPrec :: ReadPrec Integer
      whitespace
      d <- readPrec :: ReadPrec Integer
      return $ n % d
    whitespace = lift skipSpaces
    readRatio   = Rat <$> (readPrec :: ReadPrec (Ratio Integer))


instance PP.Pretty Rat where
  pretty = PP.text . show'

instance Additive Rat where
  (.+) = (+)
  zero = 0

instance AdditiveGroup Rat where
  neg = negate
  (.-) = (-)

instance Multiplicative Rat where
  (.*) = (*)
  one = 1

class FromRat a where
  fromRat :: Rat -> a

instance FromRat Rat where
  fromRat = id

