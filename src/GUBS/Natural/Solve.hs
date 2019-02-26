module GUBS.Natural.Solve
  ( module N
  , defaultProcessor
  ) where


import qualified Text.PrettyPrint.ANSI.Leijen  as PP

import           GUBS.Algebra
import           GUBS.Natural.ConstraintSystem as N
import           GUBS.Natural.Solve.Encoding   as N
import           GUBS.Natural.Solve.SCC        as N
import           GUBS.Natural.Solve.Simplify   as N
import           GUBS.Natural.Strategy         as N



defaultProcessor :: (Ord v, Ord f, PP.Pretty f, PP.Pretty v) => Solver -> Processor f Q v IO
defaultProcessor smtSolver =
  withLog (try simplify) ==> try (exhaustive (logAs "SCC" (sccDecompose simple)))
  where
    withLog p cs = logOpenConstraints cs *> p cs <* logInterpretation cs <* logConstraints cs

    logAs str p cs = logBlk (str++"...") (p cs)
    smtOpts = defaultSMTOpts { domain = Rat, minimize = tryM (iterM 3 zeroOut) `andThenM` tryM (iterM 3 shiftMax) `andThenM` iterM 3 decreaseCoeffs }
    -- smtOpts = defaultSMTOpts { minimize = tryM (iterM 3 zeroOut) `andThenM` tryM (iterM 3 shiftMax) `andThenM` iterM 3 decreaseCoeffs }
    -- smtOpts = defaultSMTOpts { minimize = noneM }

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

