module GUBS.Solver.MiniSMT (
  miniSMT
  ) where

import           Prelude hiding (lookup)
import           Text.Read hiding (Symbol,lift)

import           Control.Applicative ((<|>))
import qualified Control.Monad.State as St
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Builder as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           System.IO (hClose, hFlush, hSetBinaryMode, stderr, hPutStrLn)
import           System.IO.Temp (withSystemTempFile)
import           System.Exit
import           System.Process
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified GUBS.Polynomial as Poly
import           GUBS.Solver.Class

data MiniSMT = MiniSMT

newtype Symbol = Symbol Int deriving (Eq, Ord)
data SymbolType = BoolType | NatType deriving (Eq,Ord)

instance Show Symbol where
  show (Symbol i) = "v" ++ show i
  
instance Show SymbolType where
  show BoolType = "Bool"
  show NatType = "Nat"  

type Assign = Map Symbol Integer

lookup :: Symbol -> Assign -> Integer
lookup s a = fromMaybe 0 (Map.lookup s a)

data Frame = Frame { fFreeVars    :: Set (Symbol,SymbolType)
                   , fAssign      :: Maybe Assign
                   , fConstraints :: [SMTFormula MiniSMT]}

data SolverState = SolverState { freshId    :: Int
                               , curFrame   :: Frame
                               , frameStack :: [Frame] }

initialState :: SolverState
initialState = SolverState { freshId = 0
                           , curFrame = Frame Set.empty Nothing [] 
                           , frameStack = [] }
               
assign :: SolverState -> Maybe Assign
assign = fAssign . curFrame
             
freeVars :: SolverState -> [(Symbol, SymbolType)]
freeVars = Set.toList . fFreeVars . curFrame

constraints :: SolverState -> [SMTFormula MiniSMT]
constraints st = concatMap fConstraints (curFrame st : frameStack st)

pushFrame :: SolverState -> SolverState
pushFrame st@SolverState{..} =
  st { curFrame = Frame (fFreeVars curFrame) (fAssign curFrame) []
     , frameStack = curFrame : frameStack }

popFrame :: SolverState -> SolverState
popFrame st =
  case frameStack st of
    [] -> error "MiniSMT: pop on empty stack"
    f : fs -> st { curFrame = f { fAssign = fAssign (curFrame st) <|> fAssign f } , frameStack = fs }    

addConstraint :: SMTFormula MiniSMT -> SolverState -> SolverState
addConstraint c st@SolverState{..} =
  st { curFrame = curFrame { fConstraints = c : fConstraints curFrame }}



-- smt script formatter
----------------------------------------------------------------------

stringBS :: String -> BS.Builder
stringBS = BS.string8

integerBS :: Integer -> BS.Builder
integerBS n | n >= 0 = BS.integerDec n
            | otherwise = app "-" [integerBS (-n)]

charBS :: Char -> BS.Builder
charBS = BS.char8

(<+>) :: BS.Builder -> BS.Builder -> BS.Builder
e1 <+> e2 = e1 <> charBS ' ' <> e2

(</>) :: BS.Builder -> BS.Builder -> BS.Builder
e1 </> e2 = e1 <> charBS '\n' <> e2

sepWith :: (BS.Builder -> BS.Builder -> BS.Builder) -> [BS.Builder] -> BS.Builder
sepWith _  [] = mempty
sepWith _ [e]  = e
sepWith sep (e:es) = e `sep` sepWith sep es

vsep,hsep :: [BS.Builder] -> BS.Builder
vsep = sepWith (</>)
hsep = sepWith (<+>)

sexpr :: [BS.Builder] -> BS.Builder
sexpr es = charBS '(' <> hsep es <> charBS ')'

app :: String -> [BS.Builder] -> BS.Builder
app f es = sexpr (stringBS f : es)
  
toScript :: [(Symbol, SymbolType)] -> [SMTFormula MiniSMT] -> BS.Builder
toScript vs cs =
  app "set-logic" [stringBS "QF_NIA"]
  </> vsep [ app "declare-fun" [stringBS (show v), sexpr [], stringBS (show tpe)]
           | (v,tpe) <- vs]
  </> vsep [ app "assert" [formToBS d] | d <- cs]
  </> app "check-sat" []
  where
    formToBS Top                         = stringBS "true"
    formToBS Bot                         = stringBS "false"
    formToBS (Lit (BoolLit (BLit l)))    = stringBS (show l)
    formToBS (Lit (NegBoolLit (BLit l))) = app "not" [stringBS (show l)]
    formToBS (Atom (Eq e1 e2))           = app "=" [ expressionToBS e1, expressionToBS e2 ]
    formToBS (Atom (Geq e1 e2))          = app ">=" [ expressionToBS e1, expressionToBS e2 ]
    formToBS (And f1 f2)                 = app "and" [ formToBS f1, formToBS f2 ]
    formToBS (Or f1 f2)                  = app "or" [ formToBS f1, formToBS f2 ]
    formToBS (Iff{})                     = error "MiniSMT.toScript: unexpected Iff."
    formToBS (LetB{})                    = error "MiniSMT.toScript: unexpected LetB."
    -- TODO lets?

    expressionToBS e = polyToBS e where
      add []       = integerBS 0
      add [a]      = a
      add as       = app "+" as
      mul 0 _      = integerBS 0
      mul c []     = integerBS c
      mul 1 [a]    = a
      mul (-1) [a] = app "-" [a]
      mul 1 as     = app "*" as
      mul c as     = app "*" (integerBS c : as)
      
      polyToBS (Poly.toMonos -> ms) = add [ monoToBS c m | (c,m) <- ms ]
      monoToBS c (Poly.toPowers -> ps) = mul c (concat [ replicate e (stringBS (show v)) | (NLit v,e) <- ps ])
    

-- result parser
----------------------------------------------------------------------

parseOut :: String -> Maybe Assign
parseOut out =
  case lines out of
    "sat" : _ : ls -> Just $ 
      Map.fromList [ (case read var of NLit s -> s, read val)
                   | l <- ls
                   , let (var,_:val) = break (== '=') (filter (/= ' ') l)]    
    _ -> Nothing

-- minismt wrapper
----------------------------------------------------------------------
runMiniSMT :: MonadIO m => [(Symbol,SymbolType)] -> [SMTFormula MiniSMT] -> m (Maybe Assign)
runMiniSMT vs cs = do
  let script = toScript vs cs
  liftIO $ withSystemTempFile "smt" $ \file hfile -> do
    hSetBinaryMode hfile True
    BS.hPutBuilder hfile script
    hFlush hfile
    hClose hfile
    (code, out, err) <- readProcessWithExitCode "minismt" ["-m","-v2",file] ""
    case code of
      ExitFailure _ -> hPutStrLn stderr err >> return Nothing
      ExitSuccess   -> return (parseOut out)
      -- where
      --   ppScript = PP.vcat . map PP.text . map ("   " ++) . lines . unpack . BS.toLazyByteString
        
-- SMTSolver instance
----------------------------------------------------------------------

freshSymbol :: SymbolType -> SolverM MiniSMT Symbol
freshSymbol tpe = do
    st@SolverState{..} <- St.get
    let sym = Symbol freshId
    St.put st { freshId = freshId + 1
              , curFrame = curFrame { fFreeVars = Set.insert (sym,tpe) (fFreeVars curFrame) } }
    return sym

instance SMTSolver MiniSMT where
  data SolverM MiniSMT a = S (St.StateT SolverState IO a) deriving (Functor)
  data NLiteral MiniSMT = NLit Symbol deriving (Eq, Ord, Show)
  data BLiteral MiniSMT = BLit Symbol deriving (Eq, Ord, Show)  

  freshBool = BLit <$> freshSymbol BoolType
  freshNat = NLit <$> freshSymbol NatType  
  
  push = St.modify pushFrame
  pop = St.modify popFrame
  assertFormula c = St.modify (addConstraint c) 

  getValue (NLit s) = 
    maybe (error "model not available") (lookup s) <$> assign <$> St.get

  checkSat = do
    st <- St.get
    ma <- S (runMiniSMT (freeVars st) (constraints st))
    St.modify (\ st -> st {curFrame = (curFrame st) {fAssign = ma} })
    return (isJust ma)
    
  
instance Applicative (SolverM MiniSMT) where
  pure a = S (pure a)
  S a1 <*> S a2 = S (a1 <*> a2)

instance Monad (SolverM MiniSMT) where
  return a = S (return a)
  S m >>= f = S (m >>= \ a -> case f a of S m2 -> m2)

instance St.MonadState SolverState (SolverM MiniSMT) where
  put s = S (St.put s)
  get = S St.get

instance Read (NLiteral MiniSMT) where
  readPrec = get >>= readLit where
    readLit 'v' = NLit <$> Symbol <$> readPrec
    readLit  _  = pfail

instance Read (BLiteral MiniSMT) where
  readPrec = get >>= readLit where
    readLit 'v' = BLit <$> Symbol <$> readPrec
    readLit  _  = pfail

instance PP.Pretty (NLiteral MiniSMT) where
  pretty (NLit l) = PP.text (show l)
instance PP.Pretty (BLiteral MiniSMT) where
  pretty (BLit l) = PP.text (show l)

miniSMT :: SolverM MiniSMT a -> IO a
miniSMT (S m) = St.evalStateT m initialState
