module GUBS.Natural.Solve (
  solveWith
  , solveWithLog
  , defaultProcessor
  , Answer (..)
  , interpretation
  , module N
  ) where


import           Data.List                     (nub)
import qualified Text.PrettyPrint.ANSI.Leijen  as PP

import           GUBS.Algebra

import           GUBS.Natural.ConstraintSystem as N
import           GUBS.Natural.Interpretation   (Interpretation)
import qualified GUBS.Natural.Interpretation   as I
import           GUBS.Natural.Solve.Encoding   as N
import           GUBS.Natural.Solve.SCC        as N
import           GUBS.Natural.Solve.Simplify   as N
import           GUBS.Natural.Strategy         as N

data Answer f v c = Open (ConstraintSystem f v c) (Interpretation f c) | Sat (Interpretation f c) deriving (Show)


defaultProcessor :: (Ord v, Ord f, PP.Pretty f, PP.Pretty v) => Solver -> Processor f Q v IO
defaultProcessor smtSolver =
  withLog (try simplify) ==> try (exhaustive (logAs "SCC" (sccDecompose simple)))
  where
    withLog p cs = logOpenConstraints cs *> p cs <* logInterpretation cs <* logConstraints cs
      -- logOpenConstraints cs *> p cs <* logInterpretation cs <* logConstraints cs

    logAs str p cs = logBlk (str++"...") (p cs)
    smtOpts = defaultSMTOpts { minimize = tryM (iterM 3 zeroOut) `andThenM` tryM (iterM 3 shiftMax) `andThenM` iterM 3 decreaseCoeffs }

    simple =
      logAs "SOLVE" $ timed $ withLog $
        try simplify
        ==> try (smt' "SMT-MSLI"   smtOpts { degree = 1, maxCoeff = Just 1, maxPoly = True })
        ==> try (smt' "SMT-SLI"    smtOpts { degree = 1, maxCoeff = Just 1 })
        ==> try (smt' "SMT-LI"     smtOpts { degree = 1 })
        ==> try (smt' "SMT-MMI(2)" smtOpts { degree = 2})
        ==> try (smt' "SMT-MI(2)"  smtOpts { degree = 2, shape = Mixed})
        ==> try (smt' "SMT-MMI(3)" smtOpts { degree = 3})
        ==> try (smt' "SMT-MI(3)"  smtOpts { degree = 3, shape = Mixed})
    smt' n o = logAs n $ timed $ smt smtSolver o
    simplify =
      logAs "Simplification" $
        try instantiate
        ==> try eliminate
        ==> try (exhaustive (propagateUp <=> propagateDown))


solveWithLog :: (Eq f, Eq v, Eq c, Monad m) => ConstraintSystem f v c -> Processor f c v m -> m (Answer f v c, ExecutionLog)
solveWithLog cs p = toAnswer <$> run I.empty (p (nub cs)) where
  toAnswer (Progress [],i,l)  = (Sat i, l)
  toAnswer (Progress cs',i,l) = (Open cs' i, l)
  toAnswer (NoProgress,i,l)   = (Open cs i, l)


solveWith :: (Eq f, Eq v, Eq c, Monad m) => 
  ConstraintSystem f v c -> Processor f c v m -> m (Answer f v c)
solveWith cs p = fst <$> solveWithLog cs p

interpretation :: Answer f v c -> Maybe (Interpretation f c)
interpretation (Sat i) = Just i
interpretation _       = Nothing

instance (PP.Pretty f, PP.Pretty c, Eq c,  Max c, PP.Pretty v) => PP.Pretty (Answer f v c) where
    pretty (Sat i)     = PP.text "SUCCESS" PP.<$$> PP.pretty i
    pretty (Open cs i) = PP.text "OPEN"    PP.<$$> PP.pretty cs PP.<$$> PP.pretty i

