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
  ) where


import           Control.Monad       ((>=>))

import           GUBS.Algebra
import           GUBS.Polynomial     (Polynomial)
import qualified GUBS.Polynomial     as Poly
import qualified GUBS.Solver.Formula as F


type SMTExpression s = Polynomial (NLiteral s) Q
type SMTFormula s    = F.Formula  (BLiteral s) (SMTExpression s)

class (Monad (SolverM s), Ord (NLiteral s)) => SMTSolver s where
  data SolverM s :: * -> *
  data NLiteral s :: *
  data BLiteral s :: *

  freshBool :: SolverM s (BLiteral s)
  freshNat  :: SolverM s (NLiteral s)
  -- freshNat :: SolverM s (QLiteral s)
  -- freshNat = do
  --   v <- freshInt
  --   assert $ Atom $ Poly.variable v `Geq` Poly.coefficient zero
  --   return v
  -- freshInt  :: SolverM s (QLiteral s)
  -- freshReal :: SolverM s (QLiteral s)
  -- freshRat  :: SolverM s (QLiteral s)

  push :: SolverM s ()
  pop  :: SolverM s ()

  assertFormula :: SMTFormula s -> SolverM s ()
  checkSat :: SolverM s Bool
  getValue :: NLiteral s -> SolverM s Q


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

stack :: SMTSolver s => SolverM s a -> SolverM s a
stack m = push *> m <* pop

evalM :: SMTSolver s => SMTExpression s -> SolverM s Q
evalM = Poly.evalWithM getValue

