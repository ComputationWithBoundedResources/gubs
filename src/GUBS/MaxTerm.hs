module GUBS.MaxTerm (
  Term (..)
  , fun
  , variable
  , constant
  , funs
  , funsDL
  , vars
  , varsDL
  , coefficients
  , args
  , argsDL
  , definedSymbol
  , substitute
  , interpret
  , interpretM
  ) where


import           Data.Foldable                (toList)
import           Data.Functor.Identity        (runIdentity)
import           Data.List                    (nub)
import           Data.String                  (IsString (..))
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           GUBS.Algebra
import           GUBS.Utils


data Term f v c
  = Var v
  | Const c
  | Fun f [Term f v c]
  | Mult (Term f v c) (Term f v c)
  | Plus (Term f v c) (Term f v c)
  | Max  (Term f v c) (Term f v c)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Num c => Num (Term f v c) where
  (+)         = Plus
  (*)         = Mult
  negate      = Mult (negate one)
  fromInteger = Const . fromInteger
  abs         = error "MaxTerm.Num.abs: not defined."
  signum      = error "MaxTerm.Num.signum: not defined."

instance Num c => Max (Term f v c) where
  maxA = Max

-- pattern ZERO :: (Eq c, Num c) => Term f v c
-- pattern ONE :: (Eq c, Num c) => Term f v c
-- pattern ZERO = Const 0
-- pattern ONE  = Const 1

fun :: f -> [Term f v c] -> Term f v c
fun = Fun

variable :: v -> Term f v c
variable = Var

constant :: c -> Term f v c
constant = Const

instance IsString (Term f String c) where
  fromString = Var

-- instance Additive (Term f v) where
--   zero = ZERO
--   ZERO    .+ t2      = t2
--   t1      .+ ZERO    = t1
--   Const i .+ Const j = Const (i + j)
--   t1      .+ t2      = Plus t1 t2

-- instance Max (Term f v) where
--   maxA ZERO      t2        = t2
--   maxA t1        ZERO      = t1
--   maxA (Const i) (Const j) = Const (max i j)
--   maxA t1        t2        = Max t1 t2

-- instance Multiplicative (Term f v) where
--   one = ONE
--   ZERO    .* _       = ZERO
--   _       .* ZERO    = ZERO
--   ONE     .* t2      = t2
--   t1      .* ONE     = t1
--   Const i .* Const j = Const (i * j)
--   t1      .* t2      = Mult t1 t2

-- instance Num c => Num (Term f v c) where
--   (+)         = Plus
--   (*)         = Mult
--   fromInteger = Const . fromIntger
--   negate c    = Mult (negate one)
--   signum _    = error "signums not defined on terms"
--   abs _       = error "abs not defined on terms"
-- ops

argsDL :: Term f v c -> [Term f v c] -> [Term f v c]
argsDL Var{}        = id
argsDL Const{}      = id
argsDL (Fun _ ts)   = (++) ts . foldr ((.) . argsDL) id ts
argsDL (Mult t1 t2) = (++) [t1,t2] . argsDL t1 . argsDL t2
argsDL (Plus t1 t2) = (++) [t1,t2] . argsDL t1 . argsDL t2
argsDL (Max t1 t2)  = (++) [t1,t2] . argsDL t1 . argsDL t2

args :: Term f v c -> [Term f v c]
args = flip argsDL []

varsDL :: Term f v c -> [v] -> [v]
varsDL (Var v)      = (v:)
varsDL Const {}     = id
varsDL (Fun _ ts)   = foldr ((.) . varsDL) id ts
varsDL (Mult t1 t2) = varsDL t1 . varsDL t2
varsDL (Plus t1 t2) = varsDL t1 . varsDL t2
varsDL (Max t1 t2)  = varsDL t1 . varsDL t2

vars :: Term f v c -> [v]
vars = flip varsDL []

funsDL :: Term f v c -> [(f,Int)] -> [(f,Int)]
funsDL Var {}       = id
funsDL Const {}     = id
funsDL (Fun f ts)   = ((f,length ts):) . foldr ((.) . funsDL) id ts
funsDL (Mult t1 t2) = funsDL t1 . funsDL t2
funsDL (Plus t1 t2) = funsDL t1 . funsDL t2
funsDL (Max t1 t2)  = funsDL t1 . funsDL t2

funs :: Eq f => Term f v c -> [(f,Int)]
funs = nub . flip funsDL []

definedSymbol :: Term f v c -> Maybe f
definedSymbol (Fun f _) = Just f
definedSymbol _ = Nothing

coefficients :: Term f v c -> [c]
coefficients = toList


-- interpret

interpretM :: (Max a, Num a,  Monad m) => (v -> m a) -> (f -> [a] -> m a) -> (c -> m a) ->  Term f v c -> m a
interpretM s i n = go where
  go (Var v)      = s v
  go (Const c)    = n c
  go (Fun f ts)   = i f =<< go `mapM` ts
  go (Plus t1 t2) = (+)  <$> go t1 <*> go t2
  go (Max t1 t2)  = maxA <$> go t1 <*> go t2
  go (Mult t1 t2) = (*)  <$> go t1 <*> go t2

interpret :: (Num a, Max a) => (v -> a) -> (f -> [a] -> a) -> (c -> a) -> Term f v c -> a
interpret s i n = runIdentity . interpretM (pure . s) (\ f as -> pure (i f as)) (pure . n)

substitute :: Num c => (v -> Term f v' c) -> Term f v c -> Term f v' c
substitute s = interpret s Fun Const


-- pretty printing

instance (PP.Pretty f, PP.Pretty v, PP.Pretty c) => PP.Pretty (Term f v c) where
  pretty = pp id where
    pp _   (Var v)      = PP.pretty v
    pp _   (Const i)    = PP.pretty i
    pp _   (Fun f ts)   = PP.pretty f PP.<> PP.tupled [PP.pretty ti | ti <- ts]
    pp par (Mult t1 t2) = ppBin par "*" (pp PP.parens t1) (pp PP.parens t2)
    pp par (Plus t1 t2) = ppBin par "+" (pp PP.parens t1) (pp PP.parens t2)
    pp _   (Max t1 t2)  = PP.text "max" PP.<> PP.tupled [PP.pretty t1, PP.pretty t2]

instance (PP.Pretty f, PP.Pretty v, PP.Pretty c) => PrettySexp (Term f v c) where
  prettySexp (Var v)      = ppCall "var" [PP.pretty v]
  prettySexp (Const i)    = PP.pretty i
  prettySexp (Fun f ts)   = ppSexp (PP.pretty f : [prettySexp ti | ti <- ts])
  prettySexp (Mult t1 t2) = ppCall "*" [prettySexp t1, prettySexp t2]
  prettySexp (Plus t1 t2) = ppCall "+" [prettySexp t1, prettySexp t2]
  prettySexp (Max t1 t2)  = ppCall "max" [prettySexp t1, prettySexp t2]


