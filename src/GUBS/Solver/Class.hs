module GUBS.Solver.Class (
  SMTSolver (..)
  , stack
  , evalM
  , F.BoolLit (..)
  , F.Formula (..)
  , F.Atom (..)
  , SMTFormula
  , SMTExpression
  , assert
  , F.smtTop
  , F.smtBot
  , F.smtBool
  -- , F.smtNot
  , F.smtOr
  , F.smtAnd  
  , F.smtBigOr
  , F.smtBigAnd
  , F.smtAll
  , F.smtAny
  , smtEq
  , smtGeq
  ) where

import Control.Monad ((>=>))

import GUBS.Algebra hiding (neg)
import qualified GUBS.Expression as E
import qualified GUBS.Solver.Formula as F
import qualified GUBS.Polynomial as Poly

type SMTExpression s = E.Expression (NLiteral s)
type SMTFormula s = F.Formula (BLiteral s) (SMTExpression s)

class (Monad (SolverM s), Ord (NLiteral s)) => SMTSolver s where
  data SolverM s :: * -> *
  data NLiteral s :: *
  data BLiteral s :: *

  freshBool :: SolverM s (BLiteral s)
  freshNat  :: SolverM s (NLiteral s)
  
  push :: SolverM s ()
  pop  :: SolverM s ()

  assertFormula :: SMTFormula s -> SolverM s ()
  checkSat :: SolverM s Bool
  getValue :: NLiteral s -> SolverM s Integer
  

assert :: SMTSolver s => SMTFormula s -> SolverM s ()
assert = letElim >=> assertFormula where
  letElim F.Top = return F.Top
  letElim F.Bot = return F.Bot
  letElim l@F.Lit{} = return l
  letElim a@F.Atom{} = return a
  letElim (F.Or f1 f2) = F.Or <$> letElim f1 <*> letElim f2
  letElim (F.And f1 f2) = F.And <$> letElim f1 <*> letElim f2
  letElim (F.LetB e f) = do
    e' <- letElim e
    b <- freshBool
    assertFormula (F.Iff (F.literal b) e')
    letElim (f b)
  letElim (F.Iff{}) = error "Class.assert: unexpected Iff."

-- isZero :: SMTSolver s => Expression (Literal s) -> Formula (Exp s)
-- isZero e = smtBigAnd [ smtBool (c == 0)
--                        `smtOr` smtBigOr [ elit v `eqA` fromNatural 0 | (v,_) <- Poly.toPowers m]
--                      | (c,m) <- Poly.toMonos e]
  
simpEq,simpGeq :: SMTSolver s => SMTExpression s -> SMTExpression s -> SMTFormula s
simpEq e1 e2  = e1 `F.eqA` e2
simpGeq e1 e2 = e1 `F.geqA` e2
-- simpEq e1 e2 | Poly.isZero e1 = isZero e2
--              | Poly.isZero e2 = isZero e1
--              | otherwise      = toSolverExp e1 `Eq` toSolverExp e2
-- simpGeq e1 e2 | Poly.isZero e1 = isZero e2
--               | otherwise      = toSolverExp e1 `Geq` toSolverExp e2


smtEq,smtGeq :: SMTSolver s => SMTExpression s -> SMTExpression s -> SMTFormula s
-- smtEq = simpEq
-- smtGeq = simpGeq
smtEq = smtFactorIEQ simpEq
smtGeq = smtFactorIEQ simpGeq

smtFactorIEQ :: SMTSolver s => (SMTExpression s -> SMTExpression s -> SMTFormula s)
             -> SMTExpression s -> SMTExpression s -> SMTFormula s
smtFactorIEQ eq e1 e2 = 
  case Poly.factorise [e1,e2] of
    Just ((_,m), [e1',e2']) -> F.smtBigOr ([ Poly.variable v `F.eqA` fromNatural 0 | v <- Poly.monoVariables m] ++ [e1' `eq` e2'])
    _ -> e1 `eq` e2
    
stack :: SMTSolver s => SolverM s a -> SolverM s a
stack m = push *> m <* pop

evalM :: SMTSolver s => SMTExpression s -> SolverM s Integer
evalM = E.evalWithM getValue

