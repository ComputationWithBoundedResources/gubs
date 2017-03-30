module GUBS.Solve.Simplify where

import           Control.Monad
import           Data.Function (on)
import           Data.Either (partitionEithers)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Data.Function (on)
import qualified Data.Set as Set
import           Data.List (groupBy,sortBy,nub,(\\))

import           GUBS.Algebra
import qualified GUBS.Interpretation as I
import           GUBS.Term (Term (..))
import qualified GUBS.Term as T
import           GUBS.Constraint (Constraint (..))
import qualified GUBS.Constraint as C
import qualified GUBS.ConstraintSystem as CS
import qualified GUBS.MaxPolynomial as MP
import           GUBS.Solve.Strategy

renaming :: Eq v => [Term f v] -> [I.Var] -> Maybe [(v,MP.MaxPoly I.Var c)]
renaming [] _ = Just []
renaming (Var v:ts) (pv:pvs) = do
  s <- renaming ts pvs
  guard (v `notElem` map fst s)
  return ((v,MP.variable pv):s)
renaming _ _ = Nothing

funsOfArgs :: [Term f v] -> [(f,Int)]
funsOfArgs = foldr T.funsDL [] . foldr T.argsDL []

logBinding f s p =
    logMsg (PP.text "Propagated:"
            PP.<+> PP.pretty f PP.<> PP.parens (PP.cat (PP.punctuate PP.comma [ PP.pretty v | (_,v) <- s]))
            PP.<+> PP.text "↦" PP.<+> PP.pretty p)

groupWith :: (Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
groupWith f = groupBy (\eq1 eq2 -> f eq1 == f eq2) . sortBy (compare `on` f)

-- propagateEq :: (Eq c, Num c, PP.Pretty c, Eq f, Ord f, PP.Pretty f, PP.Pretty v, Ord v, Monad m) => Processor f c v m
-- propagateEq cs = do
--   i <- getInterpretation
--   toProgress <$> partitionEithers <$> mapM (propagate i) cs where
--   toProgress (_,[]) = NoProgress
--   toProgress (ls,_) = Progress ls
--   propagate i (Fun f ts :=: b)
--       | Just s <- renaming ts variables
--       , Just p <- interpret i b
--       , Nothing <- get i f ar
--       , all (`elem` P.sdomain s) (P.variables p) = do
--            let p' = P.substitute p s
--            logBinding f s p'
--            modifyInterpretation (\i' -> insert i' f ar p')
--            return (Right ())
--              where ar = length ts
--   propagate _ c = return (Left c)

substitute :: (Eq c, SemiRing c, Eq v) => [(v,MP.MaxPoly v' c)] -> MP.MaxPoly v c -> MP.MaxPoly v' c
substitute s = MP.substitute s' where
  s' v = case lookup v s of
    Nothing -> undefined -- MP.variable v
    Just p  -> p


partiallyInterpret :: (Ord f, Integral c, Ord v, Monad m) => Processor f c v m
partiallyInterpret cs = do
  i <- getInterpretation
  return (Progress [ I.pInterpret i l :>=: I.pInterpret i r | (l :>=: r) <- cs])

-- TODO: MS: variant of it that inlines symbols
-- f(x) >= g(x) + 1
-- h(x) >= f(h(x)) ~> f(x) = g(x) + 1, h(x) >= g(h(x)) + 1
-- | Propagates interpretation from right to left.
--
-- Given a paritally interpreted constraint @f(x) = f(x) >= x+1 = g(x)@ we set @f(x) = g(x) = x+1@ if @f@ only occurs
-- once on the lhs of the constraint system.
propagateUp :: (Eq c, IsNat c, SemiRing c, Integral c, Max c, PP.Pretty c, Eq f, Ord f, PP.Pretty f, PP.Pretty v, Ord v, Monad m) => Processor f c v m
propagateUp = propagateUp' ==> partiallyInterpret where
  propagateUp' cs = do
    i <- getInterpretation
    toProgress cs <$> concat <$> mapM (propagate i) (groupWith dsym cs)
      where
        dsym = T.definedSymbol . C.lhs
        toProgress cs cs' = if length cs > length cs' then Progress cs' else NoProgress
        propagate i [Fun f ts :>=: b]
          | Just s <- renaming ts I.variables
          , Just p <- I.interpret i b
          , Nothing <- I.get i f ar
          , all (`elem` map fst s) (MP.variables p)
          , (f,ar) `notElem`  funsOfArgs (CS.lhss cs) = do
              let p' = substitute s p
              logBinding f s p'
              modifyInterpretation (\i' -> I.insert i' f ar p')
              return []
                where ar = length ts
        propagate _ g = return g

-- | Like `propagateUp` but propagates interpretation from left to right.
propagateDown :: (Eq c, IsNat c, SemiRing c, Integral c, Max c, PP.Pretty c, Eq f, Ord f, PP.Pretty f, PP.Pretty v, Ord v, Monad m) => Processor f c v m
propagateDown = propagateDown' ==> partiallyInterpret where
  propagateDown' cs = do
    i <- getInterpretation
    toProgress cs <$> concat <$> mapM (propagate i) (groupWith dsym cs)
      where
        toProgress cs cs' = if length cs > length cs' then Progress cs' else NoProgress
        dsym = T.definedSymbol . C.rhs
        propagate i [ h :>=: Fun f ts ]
          | Just s <- renaming ts I.variables
          , Just p <- I.interpret i h
          , Nothing <- I.get i f ar
          , all (`elem` map fst s) (MP.variables p)
          , (f,ar) `notElem`  funsOfArgs (CS.rhss cs) = do
              let p' = substitute s p
              logBinding f s p'
              modifyInterpretation (\i' -> I.insert i' f ar p')
              return []
                where ar = length ts
        propagate _ g = return g

-- | Fixes the interpretation of symbols occuring only on the rhs of constraints to zero.
eliminate :: (Eq c, IsNat c, Integral c, Num c, PP.Pretty f, PP.Pretty v, Eq v, Ord v, Monad m, Ord f) => Processor f c v m
eliminate = partiallyInterpret <== \ cs ->  do
  i <- getInterpretation
  let fs = rfuns cs Set.\\ (Set.fromList (I.domain i) `Set.union` lfuns cs)
  eliminate' (Set.toList fs) cs
  where
    rfuns = Set.fromList . foldl (flip T.funsDL) [] . CS.rhss
    lfuns = Set.fromList . foldl (flip T.funsDL) [] . CS.lhss
    eliminate' [] _ = return NoProgress
    eliminate' fs cs = do
      forM fs $ \ (f,ar) -> do
        logMsg (PP.text "Eliminated:" PP.<+> PP.pretty f PP.<> PP.text "/" PP.<> PP.int ar)
        modifyInterpretation (\ i -> I.insert i f ar (fromNatural 0))
      return (Progress cs)

-- | Simplify constraints setting variables not occuring on the rhs to zero.
--
-- > f(x,y) >= g(x)  ~>  f(x,0) >= g(x)
instantiate :: (PP.Pretty f, PP.Pretty v, Eq v, Monad m) => Processor f c v m
instantiate cs = toProgress <$> partitionEithers <$> mapM inst cs where
  toProgress (_,[]) = NoProgress
  toProgress (ls,rs) = Progress (ls ++ rs)
  -- inst c@(s :=: t) = return (Left c)
  inst c@(s :>=: t) =
    case nub [ v | v <- T.vars s, v `notElem` T.vars t ] of
      [] -> return (Left c)
      vs -> do
        let subst v | v `elem` vs = fromNatural 0
                    | otherwise = Var v
        let c' = T.substitute subst s :>=: t
        logMsg (PP.text "Substituted:" PP.<+> PP.pretty c PP.<+> PP.text "↦" PP.<+> PP.pretty c')
        return (Right c')

-- | Fixes the interpretation of constructor symbols to strongly linear polynomials.
--
-- TRS transformed via trs2cs-0 contain for each constructor symbol has a sli constraint. @fixSli@ fixates the
-- interpretation for a constructor @c(x1,...,xn)@ to @x1+...+xn+1@.
--
-- This is clearly incomplete. And as experiments show only rarely useful.
fixSli :: (Ord f, PP.Pretty f, IsNat c, Integral c, Additive c, Eq c, Ord v, Monad m) => Processor f c v m
fixSli = partiallyInterpret <== \ cs -> do
  i <- getInterpretation
  let fs = filter (isConstructor cs) $ CS.funsCS cs \\ (I.domain i)
  fixate fs cs
  where
    isConstructor cs (f,_) = any (isSli f) cs
    isSli f (l :>=: T.Fun f' _) | f == f' = isSli' l
    isSli _ _                             = False
    isSli' (T.Plus t1 t2) = isSli' t1 && isSli' t2
    isSli' (T.Var _)      = True
    isSli' (T.Fun _ [])   = True
    isSli' _              = False

    fixate [] _  = return NoProgress
    fixate fs cs = do
      forM fs $ \ (f,ar) -> do
        logMsg (PP.text "Fixate:" PP.<+> PP.pretty f PP.<> PP.text "/" PP.<> PP.int ar)
        let sli = sumA (MP.variable <$> take ar I.variables) .+ (fromNatural 1)
        modifyInterpretation (\i' -> I.insert i' f ar sli)
      return (Progress cs)

