module GUBS.Natural.Solve.SCC
  ( sccDecompose
  , sccDecomposeWith
  , chainWith )
where

import           GUBS.Natural.ConstraintSystem (TermConstraint, ConstraintSystem)
import qualified GUBS.Natural.ConstraintSystem as CS (sccs)
import           GUBS.Natural.Strategy


sccDecompose :: (Eq f, Monad m) => Processor f c v m -> Processor f c v m
sccDecompose = sccDecomposeWith CS.sccs

sccDecomposeWith :: Monad m => (ConstraintSystem f v -> [ConstraintSystem f v]) -> Processor f c v m -> Processor f c v m
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
chainWith :: Monad m => (ConstraintSystem f v -> [[TermConstraint f v]]) -> Processor f c v m -> Processor f c v m
chainWith f p cs = go (f cs) where
  go []     = return $ Progress []
  go (s:ss) = do
    logMsg ("Component: " ++ show (length ss + 1) ++ " Components")
    q <- p s
    case q of
      Progress [] -> go ss
      _           -> return $ Progress (concat $ s:ss)

