-- This module provides an interface over Stdin/Stdout for solvers compliant with the `SMT-LIBv2` standard.
--
-- tested with
--   * z3 4.4.2
--   * yices 2.5.2 (do not use --interactive flag) but --incremental is not supported for QF_NIA
--
-- MS: This module should replace Gubs.Solver.SMTLib and the remove the dependency to the smtlib2 package.
module GUBS.Solver.SMTLib2 (
  SMTLib2
  , runSMTLib2
  , runSMTLib2Using
  ) where


import           Control.Exception            (evaluate)
import           Control.Monad                (unless, void)
import qualified Control.Monad.State          as St
import           Control.Monad.Trans          (MonadIO, liftIO)
import qualified Data.ByteString.Builder      as BS
import           System.IO
import           System.Process.Typed
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Read                    hiding (Symbol, lift)

import           GUBS.Algebra
import           GUBS.Solver.Class
import           GUBS.Solver.Script

import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)


newtype Symbol  = Symbol Int deriving (Eq, Ord)

data SolverState = SolverState
  { freshId   :: Int
  , inHandle  :: Handle
  , outHandle :: Handle }

runSMTLib2 :: String -> [String] -> SolverM SMTLib2 a -> IO a
runSMTLib2 = runSMTLib2Using "QF_NIRA"

runSMTLib2Using :: String -> String -> [String] -> SolverM SMTLib2 a -> IO a
runSMTLib2Using logic cmd args m' = go (send (setLogicBS logic) *> m' <* send exitBS) where
  go (SMTLib2 m) =
    let cfg = setStdin createPipe $ setStdout createPipe $ setStderr createPipe $ proc cmd args in
    withProcess cfg  $ \p -> do
      let inh = getStdin  p
      hSetBinaryMode inh True
      hSetBuffering inh (BlockBuffering Nothing)
      r <- St.evalStateT
        m
        SolverState
          { freshId   = 0
          , inHandle  = inh
          , outHandle = getStdout p }
      err  <- hGetContents $ getStderr p
      void $ evaluate err
      unless (null err) $ hPutStrLn stderr err
      return r

send :: BS.Builder -> SolverM SMTLib2 ()
send msg = St.gets inHandle >>= \inh -> liftIO $ do
  -- BS.putStrLn (BS.toLazyByteString msg) -- print script on stdout
  BS.hPutBuilder inh msg
  BS.hPutBuilder inh (charBS '\n')
  hFlush inh

receive :: SolverM SMTLib2 String
receive = St.gets outHandle >>= \outh -> liftIO $ do
  -- _ <- hWaitForInput outh (-1)
  out <- hGetLine outh
  void $ evaluate out
  return out

ask :: BS.Builder -> SolverM SMTLib2 String
ask msg = send msg *> receive

freshSymbol :: SolverM SMTLib2 Symbol
freshSymbol = do
  st@SolverState{..} <- St.get
  let sym = freshId `seq` Symbol freshId
  St.put st { freshId = freshId + 1 }
  return sym


data SMTLib2

instance SMTSolver SMTLib2 where
  newtype SolverM  SMTLib2 a = SMTLib2 (St.StateT SolverState IO a)
    deriving (Functor, Applicative, Monad, St.MonadState SolverState, MonadIO)
  newtype NLiteral SMTLib2   = NLit Symbol deriving (Eq, Ord)
  newtype BLiteral SMTLib2   = BLit Symbol deriving (Eq, Ord)

  freshBool = do
    v <- BLit <$> freshSymbol
    send $ declareBoolBS v
    return v

  freshNat = do
    v <- NLit <$> freshSymbol
    send $ declareIntBS v
    send $ app ">=" [ stringBS (show v), natBS 0 ]
    return v

  freshInt = do
    v <- NLit <$> freshSymbol
    send $ declareIntBS v
    return v

  freshRat = do
    v <- NLit <$> freshSymbol
    n <- NLit <$> freshSymbol
    d <- NLit <$> freshSymbol
    send $ declareRealBS v
    send $ declareIntBS n
    send $ declareIntBS d
    send $ 
      app "assert" 
        [ app "=" [ stringBS (show v), app "/" $ (stringBS . show) <$> [n,d] ] ]
    return v

  freshReal = do
    v <- NLit <$> freshSymbol
    send $ declareRealBS v
    return v

  -- use yices2/z3 as reference; yices2 requires '(push nat)'
  push            = send $ app "push" [natBS 1]
  pop             = send $ app "pop"  [natBS 1]
  assertFormula c = send $ assertBS c

  getValue l = parseValue <$> ask (getValueBS l)
    where parseValue = readAssignment

  checkSat = (== "sat") <$> ask checkSatBS

instance Show (NLiteral SMTLib2) where
  show (NLit (Symbol i)) = 'n': show i

instance Show (BLiteral SMTLib2) where
  show (BLit (Symbol i)) = 'b': show i

instance Read (NLiteral SMTLib2) where
  readPrec = get >>= readLit where
    readLit 'n' = NLit . Symbol <$> readPrec
    readLit  _  = pfail

instance Read (BLiteral SMTLib2) where
  readPrec = get >>= readLit where
    readLit 'b' = BLit . Symbol <$> readPrec
    readLit  _  = pfail

instance PP.Pretty (NLiteral SMTLib2) where
  pretty l = PP.text (show l)

instance PP.Pretty (BLiteral SMTLib2) where
  pretty l = PP.text (show l)

