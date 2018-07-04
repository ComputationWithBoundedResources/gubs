module GUBS.Solve (
  solveWith
  , solveWithLog
  , defaultProcessor
  , Answer (..)
  , interpretation
  , module S
  , module P
  ) where


import           Data.List (nub)

import qualified GUBS.Interpretation as I
import           GUBS.Solve.SCC as P
import           GUBS.Solve.SMT as P
import           GUBS.Solve.Simplify as P
import qualified GUBS.Solve.Strategy as S
import           GUBS.ConstraintSystem
import           GUBS.Algebra
import           GUBS.Solve.Strategy
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data Answer f v c = Open (ConstraintSystem f v) (Interpretation f c) | Sat (Interpretation f c) deriving (Show)


defaultProcessor :: (Ord v, Ord f, PP.Pretty f, PP.Pretty v, Show f) => Solver -> Processor f Integer v IO
defaultProcessor smtSolver =
  withLog (try simplify) ==> try (exhaustive (logAs "SCC" (sccDecompose simple)))
  -- withLog (try simplify) ==> chainWith sccs simple
  where
    withLog p cs = 
      logOpenConstraints cs *> p cs <* logInterpretation cs <* logConstraints cs
      
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


solveWithLog :: (Eq c, Integral c, IsNat c, SemiRing c, Max c, PP.Pretty c, PP.Pretty f, Ord f, Ord v, PP.Pretty v, Monad m) =>
  ConstraintSystem f v -> Processor f c v m -> m (Answer f v c, ExecutionLog)
solveWithLog cs p = toAnswer <$> run I.empty (p (nub cs)) where
  toAnswer (Progress [],i,l) = (Sat i, l)
  toAnswer (Progress cs',i,l) = (Open cs' i, l) 
  toAnswer (NoProgress,i,l) = (Open cs i, l)   


solveWith :: (Eq c, Integral c, IsNat c, SemiRing c, Max c, PP.Pretty c, PP.Pretty f, Ord f, Ord v, PP.Pretty v, Monad m) =>
  ConstraintSystem f v -> Processor f c v m -> m (Answer f v c)
solveWith cs p = fst <$> solveWithLog cs p

interpretation :: Answer f v c -> Maybe (Interpretation f c)
interpretation (Sat i) = Just i
interpretation _ = Nothing

instance (PP.Pretty f, PP.Pretty c, Eq c, IsNat c, SemiRing c, Max c, PP.Pretty v) => PP.Pretty (Answer f v c) where
    pretty (Sat i) = PP.text "SUCCESS" PP.<$$> PP.pretty i
    pretty (Open cs i) = PP.text "OPEN" PP.<$$> PP.pretty cs PP.<$$> PP.pretty i
