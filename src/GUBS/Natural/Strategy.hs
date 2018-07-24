module GUBS.Natural.Strategy
  ( Processor
  , module M
  , logInterpretation
  , logConstraints
  , logOpenConstraints )
where


import           Text.PrettyPrint.ANSI.Leijen  (Pretty)
import qualified Text.PrettyPrint.ANSI.Leijen  as PP

import           GUBS.MaxPolynomial            (MaxPoly)
import           GUBS.MaxTerm                  (Term)
import           GUBS.Strategy                 as M hiding (ProcT, Processor)
import qualified GUBS.Strategy                 as P

import           GUBS.Natural.Constraint       (Constraint (..), lhs, rhs)
import           GUBS.Natural.ConstraintSystem (ConstraintSystem, funsCS)
import           GUBS.Natural.Interpretation


type ProcT f c m a     = P.ProcT f (MaxPoly Var c) m a
type Processor f c v m = P.Processor f (MaxPoly Var c) (ConstraintSystem f v c) m


logInterpretation :: (Pretty f, Pretty c, Eq f, Eq c, Num c, Monad m)
  => ConstraintSystem f v c -> ProcT f c m ()
logInterpretation cs =
  logBlk "Interpretation" $ fmap toList getInterpretation >>= mapM_ logBinding
  where
    fs = funsCS cs
    ppArgs i = PP.parens (PP.hcat (PP.punctuate (PP.text ",") [PP.pretty v | v <- take i variables]))
    logBinding ((f,i),p)
      | (f,i) `elem` fs = logMsg (PP.pretty f PP.<> ppArgs i PP.<+> PP.text "=" PP.<+> PP.pretty p)
      | otherwise       = return ()

logConstraint :: (Ord f, Pretty f, Pretty v, Pretty c, Num c,Monad m)
   => Interpretation f c -> Constraint (Term f v c) -> ProcT f c m ()
logConstraint i (l :>=: r) = logMsg $
  PP.pretty l PP.<+> PP.hang 2 (
    PP.text "=" PP.<+> pp i l
    PP.</> PP.text ">=" PP.<+> pp i r
    PP.</> PP.text "="  PP.<+> PP.pretty r )
  where pp j t = PP.pretty (pInterpret j t)

logConstraints', logConstraints, logOpenConstraints
  :: (Ord f, Pretty f, Pretty v, Pretty c, Num c, Monad m)
  => [Constraint (Term f v c)] -> P.ProcT f (MaxPoly Var c) m ()
logConstraints'  cs = do
    i <- getInterpretation
    mapM_ (logConstraint i) cs

logConstraints cs = logBlk "Constraints" $ logConstraints' cs

logOpenConstraints  cs =
  logBlk "Open Constraints" $ do
    i <- getInterpretation
    let nonInterpreted c = isNothing (interpret i (lhs c)) || isNothing (interpret i (rhs c))
    logConstraints' (filter nonInterpreted cs)
  where
    isNothing Nothing = True
    isNothing _       = False

