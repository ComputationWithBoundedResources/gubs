module GUBS.Solve.SCC (sccDecompose, sccDecomposeWith, chainWith) where

import qualified GUBS.ConstraintSystem        as CS
import           GUBS.Solve.Strategy


sccDecompose :: (Eq f, Monad m) => Processor f c v m -> Processor f c v m
sccDecompose = sccDecomposeWith CS.sccs

sccDecomposeWith :: Monad m => (CS.ConstraintSystem f v -> [CS.ConstraintSystem f v]) -> Processor f c v m -> Processor f c v m
sccDecomposeWith f p cs =
  case f cs of
    [] -> return NoProgress
    (scc:sccs) -> do
      logMsg ("SCC: " ++ show (length sccs + 1) ++ " SCCs")
      toResult sccs <$> p scc
  where
    toResult sccs (Progress []) = Progress (concat sccs)
    toResult _    _             = NoProgress

-- @chainWith f p cs@ behaves similar to @try (exhaustive (sccDecomposeWith f p cs))@; but decomposition is applied
-- only once.
chainWith :: (Ord f, Ord v, Monad m, Integral c) => (CS.ConstraintSystem f v -> [[CS.TermConstraint f v]]) -> Processor f c v m -> Processor f c v m
chainWith f p cs = go (f cs) where
  go []     = return $ Progress []
  go (s:ss) = do
    logMsg ("Component: " ++ show (length ss + 1) ++ " Components")
    q <- p s
    case q of
      Progress [] -> go ss
      _           -> return $ Progress (concat $ s:ss)

