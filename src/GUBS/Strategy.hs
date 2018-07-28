module GUBS.Strategy (
  ProcT
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
  , timed
  , timeout
  , Answer (..)
  , interpretation
  , solveWith
  , solveWithLog
  ) where


import           Control.Monad.State
import           Control.Monad.Trace
import           Data.Time
import           Data.Tree                    (Forest)
import qualified System.Timeout               as Timeout
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           GUBS.Interpretation          hiding (get)
import           GUBS.Utils

type ExecutionLog = Forest String

newtype ProcT f i m a = ProcT { runProcT_ :: StateT (Interpretation f i) (TraceT String m) a }
  deriving (Applicative, Functor, Monad, MonadState (Interpretation f i), MonadTrace String, MonadIO)


instance MonadTrans (ProcT f c) where lift = ProcT . lift . lift

run :: Monad m => Interpretation f c -> ProcT f c m a -> m (a, Interpretation f c, ExecutionLog)
run i = liftM (\((a,j),l) -> (a,j,l)) . runTraceT .  flip runStateT i . runProcT_

liftTrace :: Monad m => TraceT String m a -> ProcT f c m a
liftTrace = ProcT . lift

getInterpretation :: Monad m => ProcT f i m (Interpretation f i)
getInterpretation = get

modifyInterpretation :: Monad m => (Interpretation f i -> Interpretation f i) -> ProcT f i m ()
modifyInterpretation = modify


data Result  cs
  = Progress cs
  | NoProgress

type Processor f i cs m = cs -> ProcT f i m (Result cs)

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

(==>),(<==) :: (Foldable t, Monad m) => Processor f i (t cs) m -> Processor f i (t cs) m -> Processor f i (t cs) m
p1 ==> p2 = \cs -> do
  r <- p1 cs
  case r of
    Progress cs'
      | null cs'  -> return (Progress cs')
      | otherwise -> p2 cs'
    NoProgress              -> return NoProgress
(<==) = flip (==>)


exhaustive :: (Foldable t, Monad m) => Processor f i (t cs) m -> Processor f i (t cs) m
exhaustive p = p ==> try (exhaustive p)


-- Answer

data Answer f i cs = Open cs (Interpretation f i) | Sat (Interpretation f i) deriving (Show)


solveWithLog :: (Monad m, Foldable t) =>  t cs -> Processor f i (t cs) m -> m (Answer f i (t cs), ExecutionLog)
solveWithLog cs0 p = toAnswer <$> run empty (p cs0) where
  toAnswer (Progress cs1,i,l)
    | null cs1              = (Sat i, l)
    | otherwise             = (Open cs1 i, l)
  toAnswer (NoProgress,i,l) = (Open cs0 i, l)

solveWith :: (Monad m, Foldable t) =>  t cs -> Processor f i (t cs) m -> m (Answer f i (t cs))
solveWith cs p = fst <$> solveWithLog cs p

interpretation :: Answer f i cs -> Maybe (Interpretation f i)
interpretation (Sat i) = Just i
interpretation _       = Nothing

instance (PP.Pretty f, PP.Pretty i, PP.Pretty cs) => PP.Pretty (Answer f i cs) where
  pretty (Sat i)     = PP.text "SUCCESS" PP.<$$> PP.pretty i
  pretty (Open cs i) = PP.text "OPEN"    PP.<$$> PP.pretty cs PP.<$$> PP.pretty i

