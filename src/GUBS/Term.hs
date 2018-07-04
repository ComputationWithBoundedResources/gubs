module GUBS.Term (
  Term (..)
  , fun
  , variable
  , constant
  , funs
  , funsDL
  , vars
  , varsDL
  , args
  , argsDL
  , definedSymbol
  , substitute
  , interpret
  , interpretM
  ) where

import           Data.List (nub)
import           Data.String (IsString (..))
import           Data.Functor.Identity (runIdentity)
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           GUBS.Algebra
import           GUBS.Utils

data Term f v = Var v
              | Const Integer
              | Fun f [Term f v]
              | Mult (Term f v) (Term f v)
              | Plus (Term f v) (Term f v)
              | Max (Term f v) (Term f v)
              deriving (Eq, Ord, Show)

pattern ZERO :: Term f v
pattern ONE :: Term f v
pattern ZERO = Const 0
pattern ONE  = Const 1

fun :: f -> [Term f v] -> Term f v
fun = Fun

variable :: v -> Term f v
variable = Var

constant :: (IsNat n, Integral n) => n -> Term f v
constant = Const . fromNatural

instance IsString (Term f String) where
  fromString = Var
  
instance IsNat (Term f v) where
  fromNatural_ = Const

instance Additive (Term f v) where
  zero = ZERO
  ZERO    .+ t2      = t2
  t1      .+ ZERO    = t1
  Const i .+ Const j = Const (i + j)
  t1      .+ t2      = Plus t1 t2

instance Max (Term f v) where
  maxA ZERO      t2        = t2
  maxA t1        ZERO      = t1
  maxA (Const i) (Const j) = Const (max i j)
  maxA t1        t2        = Max t1 t2

instance Multiplicative (Term f v) where
  one = ONE
  ZERO    .* _       = ZERO
  _       .* ZERO    = ZERO
  ONE     .* t2      = t2
  t1      .* ONE     = t1
  Const i .* Const j = Const (i * j)
  t1      .* t2      = Mult t1 t2

instance Num (Term f v) where
  (+) = (.+)
  (*) = (.*)
  fromInteger = fromNatural
  signum _ = error "signums not defined on terms"
  abs _ = error "abs not defined on terms"
  negate _ = error "negate not defined on terms"    
-- ops

argsDL :: Term f v -> [Term f v] -> [Term f v]
argsDL Var{} = id
argsDL Const{} = id            
argsDL (Fun _ ts) = (++) ts . foldr ((.) . argsDL) id ts
argsDL (Mult t1 t2) = (++) [t1,t2] . argsDL t1 . argsDL t2 
argsDL (Plus t1 t2) = (++) [t1,t2] . argsDL t1 . argsDL t2 
argsDL (Max t1 t2) = (++) [t1,t2] . argsDL t1 . argsDL t2        
                       
args :: Term f v -> [Term f v]       
args = flip argsDL []

varsDL :: Term f v -> [v] -> [v]
varsDL (Var v) = (v:)
varsDL Const {} = id            
varsDL (Fun _ ts) = foldr ((.) . varsDL) id ts
varsDL (Mult t1 t2) = varsDL t1 . varsDL t2 
varsDL (Plus t1 t2) = varsDL t1 . varsDL t2 
varsDL (Max t1 t2) = varsDL t1 . varsDL t2        

vars :: Term f v -> [v]
vars = flip varsDL []
        
funsDL :: Term f v -> [(f,Int)] -> [(f,Int)]
funsDL Var {} = id
funsDL Const {} = id            
funsDL (Fun f ts) = ((f,length ts):) . foldr ((.) . funsDL) id ts
funsDL (Mult t1 t2) = funsDL t1 . funsDL t2 
funsDL (Plus t1 t2) = funsDL t1 . funsDL t2 
funsDL (Max t1 t2) = funsDL t1 . funsDL t2        

funs :: Eq f => Term f v -> [(f,Int)]
funs = nub . flip funsDL []

definedSymbol :: Term f v -> Maybe f
definedSymbol (Fun f _) = Just f
definedSymbol _ = Nothing              

-- pretty printing

instance (PP.Pretty f, PP.Pretty v) => PP.Pretty (Term f v) where
  pretty = pp id where
    pp _   (Var v) = PP.pretty v
    pp _   (Const i) = PP.integer i
    pp _   (Fun f ts) = PP.pretty f PP.<> PP.tupled [PP.pretty ti | ti <- ts]
    pp par (Mult t1 t2) = ppBin par "*" (pp PP.parens t1) (pp PP.parens t2)
    pp par (Plus t1 t2) = ppBin par "+" (pp PP.parens t1) (pp PP.parens t2)
    pp _   (Max t1 t2)  = PP.text "max" PP.<> PP.tupled [PP.pretty t1, PP.pretty t2]

instance (PP.Pretty f, PP.Pretty v) => PrettySexp (Term f v) where 
  prettySexp (Var v) = ppCall "var" [PP.pretty v]
  prettySexp (Const i) = PP.integer i
  prettySexp (Fun f ts) = ppSexp (PP.pretty f : [prettySexp ti | ti <- ts])
  prettySexp (Mult t1 t2) = ppCall "*" [prettySexp t1, prettySexp t2]
  prettySexp (Plus t1 t2) = ppCall "+" [prettySexp t1, prettySexp t2]
  prettySexp (Max t1 t2) = ppCall "max" [prettySexp t1, prettySexp t2]


interpretM :: (Max a, SemiRing a, IsNat a, Monad m) => (v -> m a) -> (f -> [a] -> m a) -> Term f v -> m a
interpretM s _ (Var v)      = s v
interpretM _ _ (Const c)    = pure (fromNatural c)
interpretM s i (Fun f ts)   = i f =<< interpretM s i `mapM` ts
interpretM s i (Plus t1 t2) = (.+) <$> interpretM s i t1 <*> interpretM s i t2
interpretM s i (Max t1 t2)  = maxA <$> interpretM s i t1 <*> interpretM s i t2
interpretM s i (Mult t1 t2) = (.*) <$> interpretM s i t1 <*> interpretM s i t2

interpret :: (Max a, SemiRing a, IsNat a) => (v -> a) -> (f -> [a] -> a) -> Term f v -> a
interpret s i = runIdentity . interpretM (pure . s) (\ f as -> pure (i f as))

substitute :: (v -> Term f v') -> Term f v -> Term f v'
substitute s = interpret s Fun 
