module GUBS.ConstraintSystem where

import           GUBS.Algebra
import           GUBS.Constraint( Constraint( (:>=:) ) )
import qualified GUBS.Constraint as C
import qualified GUBS.Term as T
import           GUBS.Utils

import           Control.Monad.IO.Class
import           Control.Monad (join, void)
import           System.IO
import           Data.Char (digitToInt)
import           Data.List (nub,foldl')
import           Data.Graph
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Parsec
import           Text.ParserCombinators.Parsec (CharParser)

type TermConstraint f v = C.Constraint (T.Term f v)
type ConstraintSystem f v = [TermConstraint f v]

funs :: Eq f => TermConstraint f v -> [(f,Int)]
funs c = nub (T.funsDL (C.lhs c) (T.funsDL (C.rhs c) []))

funsCS :: Eq f => ConstraintSystem f v -> [(f,Int)]
funsCS = nub . foldr funsC [] where
  funsC c = T.funsDL (C.lhs c) . T.funsDL (C.rhs c)

lhss,rhss :: ConstraintSystem f v -> [T.Term f v]
lhss = map C.lhs
rhss = map C.rhs

sccsWith :: (v -> v -> Bool) -> [v] -> [[v]]
sccsWith p cs = map flattenSCC ccs where
  ccs = stronglyConnComp [ (c, i, succs c) | (i, c) <- ecs ]
  ecs = zip [0::Int ..] cs
  succs from = [ j | (j, to) <- ecs, p from to ]

-- MS: counterexample completeness
-- (1) f(x) >= g(x)
-- (2) x    >= g(x)
-- we obtain (2) -> (1); if we set (1) f(x) = 0 >= 0 = g(x) we obtain (2) x >= 0
-- | Default scc decomposition.
sccs :: Eq f => ConstraintSystem f v -> [ConstraintSystem f v]
sccs = sccsWith $ \ from to -> any (`elem` funs from) (T.funs (C.lhs to))

-- MS: using @trs2cs-0@ constraints for polynomial interpretations for TRSs contain stronly linear constraints, eg
-- @x1+...+xn+k() >= c(x1,...,xn)@. Since k() is fresh such constraints are always sinlge node SCCs in `sccs`.
sccs' :: Eq f => ConstraintSystem f v -> [ConstraintSystem f v]
sccs' = sccsWith $ \from to ->
  if isSli to || isSli from
    then any (`elem` funs from) (funs to)
    else any (`elem` funs from) (T.funs (C.lhs to))
  where
    isSli (l :>=: T.Fun{}) = isSli' l
    isSli _                = False
    isSli' (T.Plus t1 t2) = isSli' t1 && isSli' t2
    isSli' (T.Var _)      = True
    isSli' (T.Fun _ [])   = True
    isSli' _              = False

-- pretty printing

instance {-# OVERLAPPING #-} (PP.Pretty f, PP.Pretty v) => PP.Pretty (ConstraintSystem f v) where
  pretty = PP.vcat . map PP.pretty

instance (PP.Pretty f, PP.Pretty v) => PrettySexp (ConstraintSystem f v) where
  prettySexp = PP.vcat . map prettySexp

-- parsing

type Parser = CharParser ()

newtype Variable = Variable String deriving (Eq, Ord, Show)
newtype Symbol = Symbol String deriving (Eq, Ord, Show)

instance PP.Pretty Variable where pretty (Variable v) = PP.text v
instance PP.Pretty Symbol where pretty (Symbol v) = PP.text v

whiteSpace1 :: Parser String
whiteSpace1 = many1 ((space <|> tab <|> newline) <?> "whitespace")

parens :: Parser a -> Parser a
parens = between (lexeme (char '(')) (lexeme (char ')'))

lexeme :: Parser a -> Parser a
lexeme p = p <* many whiteSpace1

literal :: String -> Parser ()
literal s = void (lexeme (string s))

identifier :: Parser String
identifier = lexeme (many1 (try alphaNum <|> oneOf "'_/#?*+-!:@[]"))

natural :: Parser Int
natural = lexeme (foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit)

term :: Parser (T.Term Symbol Variable)
term = try constant <|> parens (try var <|> compound) where
  var = literal "var" >> (T.Var . Variable <$> identifier)
  constant = fromNatural . fromIntegral <$> natural
  compound = join (toTerm <$> identifier <*> many (lexeme term))
  toTerm f ts | f `notElem` ["*","+","max"] = return (T.Fun (Symbol f) ts)
  toTerm "*" ts  = return (prod ts)
  toTerm "+" ts  = return (sumA ts)
  toTerm "max" ts  = return (maximumA ts)
  toTerm _ _ = fail "toTerm"

constraint :: Parser (TermConstraint Symbol Variable)
constraint = parens $ do
  c <- literal ">=" >> return (:>=:)
       -- <|> (literal "=" >> return (:=:))
  c <$> lexeme term <*> lexeme term

constraintSystem :: Parser (ConstraintSystem Symbol Variable)
constraintSystem = many (lexeme constraint)

csFromFile :: MonadIO m => FilePath -> m (Either ParseError (ConstraintSystem Symbol Variable))
csFromFile file = runParser parser () sn <$> liftIO (readFile file) where
  sn = "<file " ++ file ++ ">"
  parser = many whiteSpace1 *> constraintSystem <* eof

csToFile :: (MonadIO m, PP.Pretty f, PP.Pretty v) => ConstraintSystem f v -> FilePath -> m ()
csToFile cs f = liftIO $ do
   handle <- openFile f WriteMode
   PP.hPutDoc handle (prettySexp cs)
   hClose handle
