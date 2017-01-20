module GUBS.ConstraintSystem where

import           GUBS.Algebra
import           GUBS.Constraint( Constraint( (:>=:) ) )
import qualified GUBS.Constraint as C
import qualified GUBS.Term as T
import           GUBS.Utils

import           Control.Monad.IO.Class
import           Control.Monad (join)
import           System.IO 
import           Data.Char (digitToInt)
import           Data.List (nub,foldl')
import           Data.String
import           Data.Graph
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Parsec
import           Text.ParserCombinators.Parsec (CharParser)

type TermConstraint f v = C.Constraint (T.Term f v)
type ConstraintSystem f v = [TermConstraint f v]

funs :: Eq f => TermConstraint f v -> [(f,Int)]
funs c = nub (T.funsDL (C.lhs c) (T.funsDL (C.rhs c) []))

lhss,rhss :: ConstraintSystem f v -> [T.Term f v]
lhss = map C.lhs
rhss = map C.rhs

--TODO check if that makes sense
sccs :: Eq f => ConstraintSystem f v -> [ConstraintSystem f v]
sccs cs = map flattenSCC sccs'
  where
    sccs' = stronglyConnComp [ (c, i, succs c) | (i, c) <- ecs ]
    ecs = zip [0 ..] cs
    -- succs c@(l :=: r) = succs (l :>=: r) ++ succs (r :>=: l)
    succs c@(l :>=: r) = [ j | (j, c') <- ecs
                             , any (`elem` (funs c)) (T.funs (C.lhs c')) ]


-- pretty printing

instance {-# OVERLAPPING #-} (PP.Pretty f, PP.Pretty v) => PP.Pretty (ConstraintSystem f v) where
  pretty = PP.vcat . map PP.pretty

instance (PP.Pretty f, PP.Pretty v) => PrettySexp (ConstraintSystem f v) where
  prettySexp = PP.vcat . map PP.pretty

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
literal s = lexeme (string s) >> return ()

identifier :: Parser String
identifier = lexeme (many (try alphaNum <|> oneOf "'_/#?*+-"))

natural :: Parser Int
natural = lexeme (foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit)

term :: Parser (T.Term Symbol Variable)
term = try constant <|> parens (try var <|> compound) where
  var = literal "var" >> (T.Var <$> Variable <$> identifier)
  constant = fromNatural <$> fromIntegral <$> natural
  compound = join (toTerm <$> identifier <*> many (lexeme term))
  toTerm f ts | f `notElem` ["*","+","max"] = return (T.Fun (Symbol f) ts)
  toTerm "*" [t1,t2] = return (t1 .* t2)
  toTerm "+" [t1,t2] = return (t1 .+ t2)
  toTerm "max" [t1,t2] = return (t1 `maxA` t2)
  -- toTerm "neg" [t] = return (neg t)
  -- toTerm "-" [t1,t2] = return (t1 - t2)
  toTerm f _ = parserFail $ "Unexpected number of arguments for operation '" ++ show f ++ "'"
  
constraint :: Parser (TermConstraint Symbol Variable)
constraint = parens $ do
  c <- literal ">=" >> return (:>=:)
       -- <|> (literal "=" >> return (:=:))
  c <$> lexeme term <*> lexeme term

constraintSystem :: Parser (ConstraintSystem Symbol Variable)
constraintSystem = many (lexeme constraint)

csFromFile :: MonadIO m => FilePath -> m (Either ParseError (ConstraintSystem Symbol Variable))
csFromFile file = runParser parse () sn <$> liftIO (readFile file) where
  sn = "<file " ++ file ++ ">"
  parse = many (whiteSpace1) *> constraintSystem <* eof

csToFile :: (MonadIO m, PP.Pretty f, PP.Pretty v) => ConstraintSystem f v -> FilePath -> m ()
csToFile cs f = liftIO $ do
   handle <- openFile f WriteMode
   PP.hPutDoc handle (prettySexp cs)
   hClose handle