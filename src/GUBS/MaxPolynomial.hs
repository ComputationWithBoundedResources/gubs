module GUBS.MaxPolynomial where


import           Data.Foldable                (toList)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           GUBS.Algebra
import qualified GUBS.Polynomial              as P


data MaxPoly v c
  =  Var v
  | Const c
  | Plus (MaxPoly v c) (MaxPoly v c)
  | Mult (MaxPoly v c) (MaxPoly v c)
  | Max (MaxPoly v c) (MaxPoly v c)
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Num c => Num (MaxPoly v c) where
  (+)         = Plus
  (*)         = Mult
  negate      = Mult (negate one)
  fromInteger = Const . fromInteger
  abs         = error "MaxPolynomial.Num.abs: not defined."
  signum      = error "MaxPolynomial.Num.signum: not defined."

instance Num c => Max (MaxPoly v c) where
  maxA = Max


variable :: v -> MaxPoly v c
variable = Var

constant :: c -> MaxPoly v c
constant = Const


variablesDL :: MaxPoly v c -> [v] -> [v]
variablesDL (Var v) = (:) v
variablesDL (Const _)  = id
variablesDL (Plus p q) = variablesDL p . variablesDL q
variablesDL (Mult p q) = variablesDL p . variablesDL q
variablesDL (Max p q)  = variablesDL p . variablesDL q

variables :: MaxPoly v c -> [v]
variables p = variablesDL p []

coefficients :: MaxPoly v c -> [c]
coefficients = toList


-- operations

fromMaxPoly :: (Max a, Num a) => (v -> a) -> (c -> a) -> MaxPoly v c -> a
fromMaxPoly var _   (Var v)    = var v
fromMaxPoly _   con (Const c)  = con c
fromMaxPoly var con (Plus p q) = fromMaxPoly var con p + fromMaxPoly var con q
fromMaxPoly var con (Mult p q) = fromMaxPoly var con p * fromMaxPoly var con q
fromMaxPoly var con (Max p q)  = fromMaxPoly var con p `maxA` fromMaxPoly var con q

substitute :: Num c => (v -> MaxPoly v' c) -> MaxPoly v c -> MaxPoly v' c
substitute s = fromMaxPoly s Const


-- max elimination

splitMax :: (Ord v, Num c) =>  MaxPoly v c -> [P.Polynomial v c]
splitMax (Var v)    = [P.variable v]
splitMax (Const c)  = [P.coefficient c]
splitMax (Plus p q) = (+) <$> splitMax p <*> splitMax q
splitMax (Mult p q) = (*) <$> splitMax p <*> splitMax q
splitMax (Max p q)  = splitMax p ++ splitMax q


degree :: MaxPoly v c -> Int
degree (Var _)    = 1
degree (Const _)  = 0
degree (Plus p q) = degree p `max` degree q
degree (Max p q)  = degree p `max` degree q
degree (Mult p q) = degree p   +   degree q


-- pretty printing

instance (Ord v, Eq c, Num c, PP.Pretty v, PP.Pretty c) => PP.Pretty (MaxPoly v c) where
  pretty   = pp . splitMax where
    pp []  = PP.text "0"
    pp [t] = PP.pretty t
    pp ts  = PP.text "max" PP.<> PP.tupled (PP.pretty `map` ts)


-- instance (Eq c, Additive c) => Additive (MaxPoly v c) where
--   zero = Const zero
--   Const i .+ t2      | zero == i = t2
--   t1      .+ Const j | zero == j = t1
--   Const i .+ Const j             = Const (i .+ j)
--   t1 .+ t2                       = Plus t1 t2

-- instance (Eq c, Additive c) => Max (MaxPoly v c) where
--   Const i `maxA` t2      | zero == i = t2
--   t1      `maxA` Const j | zero == j = t1
--   t1      `maxA` t2                  = Max t1 t2

-- instance (Eq c, Additive c, Multiplicative c) => Multiplicative (MaxPoly v c) where
--   one = Const one
--   Const i .* t2      | zero == i = zero
--                      | one == i  = t2
--   t1      .* Const j | zero == j = zero
--                      | one == j  = t1
--   Const i .* Const j             = Const (i .* j)
--   t1 .* t2                       = Mult t1 t2
