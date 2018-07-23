{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where


import           GUBS.Natural.Solve           hiding (Solver (..))
import qualified GUBS.Natural.Solve           as N (Solver (..))
import           GUBS.Utils

import           Control.Monad                (when)
import           Data.Tree                    (Tree (..), drawTree)
import           System.Console.CmdArgs
import           System.Exit                  (exitFailure, exitSuccess)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))


data Solver = MiniSmt | Z3 | Yices2 deriving Data

data GUBS = GUBS
  { solver  :: Solver
  , input   :: FilePath
  , verbose :: Bool }
  deriving Data


toSolver :: Solver -> N.Solver
toSolver MiniSmt = N.MiniSmt
toSolver Z3      = N.Pipe "z3" ["-in"]
toSolver Yices2  = N.Pipe "yices-smt2" []


defaultConfig :: GUBS
defaultConfig =
  GUBS
    { input   = def &= typFile &= argPos 0
    , solver  = Z3  &= help "SMT solver (minismt, z3, yices2). Defaults to z3."
    , verbose = False }
  &= summary "GUBS Upper Bound Solver 0.4"


main :: IO ()
main = do
  GUBS{..} <- cmdArgs defaultConfig
  parsed   <- csFromFile input
  case parsed of
    Left err -> do
      putDocLn (text "ERROR" <$$> text (show err))
      exitFailure
    Right cs -> do
      (r,l) <- cs `solveWithLog` defaultProcessor (toSolver solver)
      putDocLn r
      when verbose (putLog l)
      exitSuccess
  where
    putLog l = putDocErrLn (text "" <$$> text (drawTree (Node "ExecutionLog" l)))

