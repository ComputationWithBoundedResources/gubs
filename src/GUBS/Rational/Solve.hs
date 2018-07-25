module GUBS.Rational.Solve where


import           Control.Monad.Trans   (MonadIO, liftIO)
import qualified Data.Foldable         as F (toList)
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict       as M

import           GUBS.Algebra
import           GUBS.Interpretation   (Interpretation)
import qualified GUBS.Interpretation   as I
import           GUBS.Polynomial       (Polynomial)
import qualified GUBS.Polynomial       as P
import           GUBS.Solver           as SMT
import           GUBS.Solver.Formula   as SMT
import           GUBS.Strategy         hiding (Processor)
import qualified GUBS.Strategy         as S (Processor)


data Term f v c
  = Var v
  | Val c
  | Add [Term f v c]
  | Mul [Term f v c]
  | Fun f [Term f v c]
  deriving (Eq, Ord, Show)

-- addF, mulF :: (Foldable l, Num c) => l (Term f v c) -> Term f v c
-- addF (F.toList -> []) = zero
-- addF (F.toList -> ts) = Add ts
-- mulF (F.toList -> []) = one
-- mulF (F.toList -> ts) = Mul ts

instance Num c => Num (Term f v c) where
  s + t       = Add [s,t]
  s * t       = Mul [s,t]
  negate t    = Mul [t, negate one]
  fromInteger = Val . fromInteger
  abs    = error "GUBS.Rational.Num: abs undefined."
  signum = error "GUBS.Rational.Num: signum undefined."

fromTermM :: (Num a, Monad m) => (f -> [a] -> m a) -> (v -> m a) -> (c -> m a) -> Term f v c -> m a
fromTermM fun var val t = go t where
  go (Var v)    = var v
  go (Val v)    = val v
  go (Add ts)   = sumA <$> mapM go ts
  go (Mul ts)   = prod <$> mapM go ts
  go (Fun f ts) = fun f =<< mapM go ts

fromTerm :: Num a => (f -> [a] -> a) -> (v -> a) -> (c -> a) -> Term f v c -> a
fromTerm fun var val = runIdentity . fromTermM (\f as -> pure (fun f as)) (pure . var) (pure . val)

substitute :: Num c => (v -> Term f v' c) -> Term f v c -> Term f v' c
substitute s = fromTerm Fun s Val

interpret1 :: (Ord f, Num c') => Interpretation f (Term f' I.Var c') -> (c -> Term f' v' c') -> Term f v' c -> Term f' v' c'
interpret1 i val = fromTerm fun Var val where
  fun f ts =
    case I.get i f (length ts) of
      Just t  -> substitute (sigma (zip I.variables ts)) t
      Nothing -> fun f ts
  sigma m k = M.fromList m M.! k

-- encode

type Term' f v s     = Term f v (SMTExpression s)
type Polynomial' v s = Polynomial v (SMTExpression s)

encode1 :: (Ord v, SMTSolver s) => Term' f v s -> Maybe (Polynomial' v s)
encode1 = fromTermM (\_ _ -> Nothing) (pure . P.variable) (pure . P.coefficient)

decode1 :: SMTSolver s => Term' f v s -> SolverM s (Term f v Q)
decode1 = fromTermM (\f as -> pure (Fun f as)) (pure . Var) val where
  val t = Val <$> P.evalWithM getValue t


linear :: SMTSolver s => Int -> SolverM s (Term' f I.Var s)
linear ar = sumA <$> sequence (val : [ (*) <$> val <*> var v | v <- take ar I.variables ]) where
  val = (Val . P.variable) <$> freshReal
  var = pure . Var



-- constraints

data Constraint a = Constraint
  { premise     :: [a]
  , consequence ::  a }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

interpret :: (Num c', Ord f3) => Interpretation f3 (Term f' I.Var c')-> (c -> Term f' v' c') 
  -> [Constraint (Term f3 v' c)] -> [Constraint (Term f' v' c')]
interpret tint val = fmap $ fmap $ interpret1 tint val

encode :: (Ord v, SMTSolver s) => [Constraint (Term' f v s)] -> Maybe ([Constraint (Polynomial' v s)])
encode = mapM (traverse encode1)


entscheide1 :: (Ord v, SMTSolver s) => Constraint (Polynomial' v s) -> SolverM s ()
entscheide1 Constraint{..} = do
  let
    lambda = do
      l <- P.variable <$> freshNat
      assert $ SMT.geqA l zero
      return $ P.coefficient l
    scaleM p = (*) <$> lambda <*> pure p
  premise' <- mapM scaleM $ premise
  SMT.assert $ SMT.smtAll (uncurry SMT.eqA) $
    P.zipCoefficients (sumA premise') consequence

entscheide :: (Ord v, SMTSolver s) => [Constraint (Polynomial' v s)] -> SolverM s ()
entscheide = mapM_ entscheide1

signature :: Ord f => [Constraint (Term f v c)] -> M.Map (f,Int) Int
signature = foldr k M.empty . concatMap F.toList where
  k (Fun f ts) m = M.insert (f,length ts) (length ts) m
  k _          m = m
  

type Processor f v m = S.Processor f (Term f I.Var Q) [Constraint (Term f v Q)] m

smt :: (Ord f, Ord v, MonadIO m) => Processor f v m
smt []  = return NoProgress
smt cs = do
  tint <- getInterpretation
  let 
    cs1 = interpret tint Val cs
    solver = do
      aint <- I.Inter <$> traverse linear (signature cs1)
      let 
        cs2      = interpret aint (Val . P.coefficient) cs1
        Just cs3 = encode cs2
      entscheide cs3
      checkSat >>= \b -> if b then Just <$> traverse decode1 aint else pure Nothing
  mi <- liftIO $ SMT.runSMTLib2 "z3" ["-smt2", "-in"] solver
  case mi of
    Nothing -> return NoProgress
    Just t  -> modifyInterpretation (I.union t) >> return (Progress [])

