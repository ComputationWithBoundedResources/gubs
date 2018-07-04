module GUBS.Solve.Strategy (
  Interpretation
  , ProcT
  , Processor
  , Result (..)
  , ExecutionLog
  , run
  , logMsg
  , logBlk
  , liftTrace
  , (==>)
  , (<==)  
  , (<=>)
  , abort
  , try
  , exhaustive
  , getInterpretation
  , modifyInterpretation
  , logConstraints
  , logOpenConstraints
  , logInterpretation
  , timed
  , timeout
  ) where


import           Data.Maybe (isNothing)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Control.Monad.State
import           Control.Monad.Trace
import           Data.Tree (Forest)
import           Data.Time        
import qualified System.Timeout as Timeout

import           GUBS.Utils
import           GUBS.Algebra
import           GUBS.Interpretation hiding (get)
import           GUBS.Constraint
import           GUBS.ConstraintSystem
import qualified GUBS.Interpretation as I

type ExecutionLog = Forest String
                           
newtype ProcT f c m a = ProcT { runProcT_ :: StateT (Interpretation f c) (TraceT String m) a }
  deriving (Applicative, Functor, Monad, MonadState (Interpretation f c), MonadTrace String, MonadIO)


instance MonadTrans (ProcT f c) where lift = ProcT . lift . lift

run :: Monad m => Interpretation f c -> ProcT f c m a -> m (a, Interpretation f c, ExecutionLog)
run i = liftM (\((a,j),l) -> (a,j,l)) . runTraceT .  flip runStateT i . runProcT_

liftTrace :: Monad m => TraceT String m a -> ProcT f c m a
liftTrace = ProcT . lift 

getInterpretation :: Monad m => ProcT f c m (Interpretation f c)
getInterpretation = get

modifyInterpretation :: Monad m => (Interpretation f c -> Interpretation f c) -> ProcT f c m ()
modifyInterpretation = modify


logInterpretation :: (Eq c, IsNat c, SemiRing c, Max c, PP.Pretty c, PP.Pretty f, Ord f, Monad m) =>  ConstraintSystem f v -> ProcT f c m ()
logInterpretation cs =
  logBlk "Interpretation" $ fmap toList getInterpretation >>= mapM_ logBinding
  where
    fs = funsCS cs
    ppArgs i = PP.parens (PP.hcat (PP.punctuate (PP.text ",") [PP.pretty v | v <- take i I.variables]))    
    logBinding ((f,i),p)
      | (f,i) `elem` fs = logMsg (PP.pretty f PP.<> ppArgs i PP.<+> PP.text "=" PP.<+> PP.pretty p)
      | otherwise       = return ()
 
logConstraints' :: (Multiplicative c, Max c, IsNat c, Integral c, PP.Pretty c, PP.Pretty f, Ord f, Ord v, PP.Pretty v, Monad m) =>  ConstraintSystem f v -> ProcT f c m ()
logConstraints'  cs = do
    i <- getInterpretation
    mapM_ (logConstraint i) cs
    where
      logConstraint i (l :>=: r) = logConstraint' ">=" i l r
      -- logConstraint i (l :=: r) = logConstraint' "=" i l r
      logConstraint' eq i l r = logMsg $
        PP.pretty l PP.<+> PP.hang 2 (PP.text "=" PP.<+> pp i l
                                      PP.</> PP.text eq PP.<+> pp i r
                                      PP.</> PP.text "=" PP.<+> PP.pretty r)
      pp i t = PP.pretty (pInterpret i t)

logConstraints :: (Multiplicative c, Max c, IsNat c, Integral c, PP.Pretty c, PP.Pretty f, Ord f, Ord v, PP.Pretty v, Monad m) =>  ConstraintSystem f v -> ProcT f c m ()
logConstraints cs = logBlk "Constraints" $ logConstraints' cs

logOpenConstraints :: (Multiplicative c, Max c, IsNat c, Integral c, PP.Pretty c, PP.Pretty f, Ord f, Ord v, PP.Pretty v, Monad m) =>  ConstraintSystem f v -> ProcT f c m ()
logOpenConstraints  cs = 
  logBlk "Open Constraints" $ do 
    i <- getInterpretation
    let nonInterpreted c = isNothing (interpret i (lhs c)) || isNothing (interpret i (rhs c))
    logConstraints' (filter nonInterpreted cs)


data Result f v = Progress (ConstraintSystem f v)
                | NoProgress

type Processor f c v m = ConstraintSystem f v -> ProcT f c m (Result f v)

abort :: Monad m => Processor f c v m
abort _ = return NoProgress

timed :: MonadIO m => Processor f c v m -> Processor f c v m
timed p cs = do
  start <- liftIO getCurrentTime
  logMsg ("Staring timer: " ++ show start)
  r <- p cs
  end <- liftIO getCurrentTime
  logMsg ("Stopping timer: " ++ show end ++ "(+"++ show (diffUTCTime end start) ++")")
  return r

timeout :: Int -> Processor f c v IO -> Processor f c v IO
timeout to p cs = do
  i <- getInterpretation
  mr <- liftIO (Timeout.timeout to (run i (p cs)))
  case mr of
    Nothing -> return NoProgress
    Just (r,i',l) -> do
      modifyInterpretation (const i')
      putTrace l
      return r

try :: Monad m => Processor f c v m -> Processor f c v m
try p cs = do
  inter <- get
  r <- p cs          
  case r of 
    Progress cs' -> return (Progress cs')
    NoProgress -> put inter >> return (Progress cs)                       

(<=>) :: Monad m => Processor f c v m -> Processor f c v m -> Processor f c v m
p1 <=> p2 = \cs -> do 
  inter <- get
  r <- p1 cs
  case r of 
    Progress cs' -> return (Progress cs')
    NoProgress -> put inter >> p2 cs

(<==) :: Monad m => Processor f c v m -> Processor f c v m -> Processor f c v m
(<==) = flip (==>)

(==>) :: Monad m => Processor f c v m -> Processor f c v m -> Processor f c v m
p1 ==> p2 = \cs -> do 
  r <- p1 cs
  case r of
    Progress [] -> return (Progress [])
    Progress cs' -> p2 cs'
    NoProgress -> return NoProgress

  
exhaustive :: Monad m => Processor f c v m -> Processor f c v m
exhaustive p = p ==> try (exhaustive p)


