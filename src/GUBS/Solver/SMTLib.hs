{-# LANGUAGE DataKinds #-}
module GUBS.Solver.SMTLib (
  z3
  , z3'
  ) where

import GUBS.Algebra
import GUBS.Solver.Class
import qualified GUBS.Expression as E
import qualified GUBS.MaxPolynomial as MP
import qualified GUBS.Solver.Formula as F
import qualified Language.SMTLib2 as SMT
import qualified Language.SMTLib2.Pipe as SMT
import qualified Language.SMTLib2.Debug as SMT

import Control.Monad.State.Strict
import qualified Text.PrettyPrint.ANSI.Leijen as PP


instance SMT.Backend b => IsNat (SMT.SMT b (SMT.Expr b 'SMT.IntType)) where
  fromNatural_ = SMT.cint

instance SMT.Backend b => Additive (SMT.SMT b (SMT.Expr b 'SMT.IntType)) where
  zero = SMT.cint 0
  (.+) = (SMT..+.)

instance SMT.Backend b => Multiplicative (SMT.SMT b (SMT.Expr b 'SMT.IntType)) where
  one = SMT.cint 1
  (.*) = (SMT..*.)


toSMTExp :: SMT.Backend b => SMTExpression (SMTLibSolver b) -> SMT.SMT b (SMT.Expr b 'SMT.IntType)
toSMTExp = fromMP . E.fromPolynomial MP.variable MP.constant -- to exploit simplification
  where
    fromMP (MP.Var (NLit (_,v))) = return v
    fromMP (MP.Const i)          = fromNatural i
    fromMP (MP.Plus a b)         = fromMP a .+ fromMP b
    fromMP (MP.Mult a b)         = fromMP a .* fromMP b
    fromMP MP.Max{}              = error "max encountered"


toSMTFormula :: SMT.Backend b => SMTFormula (SMTLibSolver b) -> SMT.SMT b (SMT.Expr b 'SMT.BoolType)
toSMTFormula Top                             = SMT.cbool True
toSMTFormula Bot                             = SMT.cbool False
toSMTFormula (Lit (BoolLit (BLit (_,l))))    = return l
toSMTFormula (Lit (NegBoolLit (BLit (_,l)))) = SMT.not' (return l)
toSMTFormula (Atom (Geq l r))                = toSMTExp l SMT..>=. toSMTExp r
toSMTFormula (Atom (Eq l r))                 = toSMTExp l SMT..==. toSMTExp r
toSMTFormula (And f1 f2)                     = toSMTFormula f1 SMT..&. toSMTFormula f2
toSMTFormula (Or f1 f2)                      = toSMTFormula f1 SMT..|. toSMTFormula f2    
toSMTFormula (Iff{})                         = error "SMTLib.toSMTFormula: unexpected Iff."
toSMTFormula (LetB{})                        = error "SMTLib.toSMTFormula: unexpected LetB."


liftSMT :: SMT.Backend b => SMT.SMT b a -> SolverM (SMTLibSolver b) a
liftSMT = S . lift

freshVar :: (SMT.Backend b) => SMT.Repr t -> SolverM (SMTLibSolver b) (Integer, SMT.Expr b t)
freshVar tpe = do
  i <- S get
  v <- liftSMT (SMT.declareVar tpe)
  S (put (i+1))
  return (i,v)

data SMTLibSolver b = SMTLibSolver b

  
instance SMT.Backend b => SMTSolver (SMTLibSolver b) where
  data SolverM (SMTLibSolver b) a = S (StateT Integer (SMT.SMT b) a)
  data NLiteral (SMTLibSolver b) = NLit (Integer, (SMT.Expr b 'SMT.IntType))
  data BLiteral (SMTLibSolver b) = BLit (Integer, (SMT.Expr b 'SMT.BoolType))

  freshNat = do
    n <- NLit <$> freshVar SMT.int
    assertFormula (E.variable n `F.geqA` fromNatural 0)
    return n
    
  freshBool = BLit <$> freshVar SMT.bool

  push = liftSMT SMT.push
  pop = liftSMT SMT.pop

  assertFormula f = liftSMT (toSMTFormula f >>= SMT.assert)
  checkSat = (==) SMT.Sat <$> liftSMT SMT.checkSat
  getValue (NLit (_,v)) = liftSMT $ do
    r <- SMT.getValue v
    case r of SMT.IntValue r -> return r

    
instance SMT.Backend b => Functor (SolverM (SMTLibSolver b)) where
  fmap f (S a) = S (fmap f a)
  
instance SMT.Backend b => Applicative (SolverM (SMTLibSolver b)) where
  pure a = S (pure a)
  S a1 <*> S a2 = S (a1 <*> a2)

instance SMT.Backend b => Monad (SolverM (SMTLibSolver b)) where
  return a = S (return a)
  S m >>= f = S (m >>= \ a -> case f a of S m2 -> m2)


instance Eq (NLiteral (SMTLibSolver b)) where
  (NLit (a,_)) == (NLit (b,_)) = a == b
  
instance Ord (NLiteral (SMTLibSolver b)) where
  (NLit (a,_)) `compare` (NLit (b,_)) = a `compare` b

instance Show (NLiteral (SMTLibSolver b)) where
  show (NLit (i,_)) = "v" ++ show i
instance Show (BLiteral (SMTLibSolver b)) where
  show (BLit (i,_)) = "v" ++ show i

instance PP.Pretty (NLiteral (SMTLibSolver b)) where
  pretty (NLit (i,_)) = PP.text "v" PP.<> PP.integer i
instance PP.Pretty (BLiteral (SMTLibSolver b)) where
  pretty (BLit (i,_)) = PP.text "v" PP.<> PP.integer i


z3 :: SolverM (SMTLibSolver SMT.SMTPipe) a -> IO a
z3 (S m) = SMT.withBackend (SMT.createPipe "z3" ["-smt2","-in"]) (setLIA >> evalStateT m 0) where
  setLIA = SMT.setOption (SMT.SMTLogic "QF_NIA")

z3' :: SolverM (SMTLibSolver (SMT.DebugBackend SMT.SMTPipe)) a -> IO a
z3' (S m) = SMT.withBackend (SMT.debugBackend <$> SMT.createPipe "z3" ["-smt2","-in"]) (setLIA >> evalStateT m 0) where
  setLIA = SMT.setOption (SMT.SMTLogic "QF_NIA")


