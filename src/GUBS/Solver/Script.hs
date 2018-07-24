module GUBS.Solver.Script where

import qualified Data.ByteString.Builder      as BS
import Data.Monoid ((<>))

import GUBS.Solver.Class
import GUBS.Solver.Formula ()
import qualified GUBS.Polynomial as Poly
import GUBS.Algebra

-- smt script formatter
----------------------------------------------------------------------

stringBS :: String -> BS.Builder
stringBS = BS.string8

qBS :: Q -> BS.Builder
qBS q
  | n == 0    = integerBS 0
  | d == 1    = integerBS n
  | otherwise = app "/" [integerBS n, integerBS d]
  where (n,d) = (numerator q, denominator q)

integerBS :: Integer -> BS.Builder
integerBS n | n >= 0 = BS.integerDec n
            | otherwise = app "-" [integerBS (-n)]

natBS :: Int -> BS.Builder
natBS = BS.intDec

charBS :: Char -> BS.Builder
charBS = BS.char8

eol :: BS.Builder -> BS.Builder
eol bs = bs <> charBS '\n'

sepWith :: (BS.Builder -> BS.Builder -> BS.Builder) -> [BS.Builder] -> BS.Builder
sepWith _ []       = mempty
sepWith _ [e]      = e
sepWith sep (e:es) = e `sep` sepWith sep es

(<+>),(</>) :: BS.Builder -> BS.Builder -> BS.Builder
e1 <+> e2 = e1 <> charBS ' ' <> e2
e1 </> e2 = e1 <> charBS '\n' <> e2

vsep,hsep :: [BS.Builder] -> BS.Builder
vsep = sepWith (</>)
hsep = sepWith (<+>)

sexpr :: [BS.Builder] -> BS.Builder
sexpr es = charBS '(' <> hsep es <> charBS ')'

app :: String -> [BS.Builder] -> BS.Builder
app f es = sexpr (stringBS f : es)


-- smtlib2-command

qfniaBS :: BS.Builder
qfniaBS =app "set-logic" [stringBS "QF_NIA"]

exitBS :: BS.Builder
exitBS = app "exit" []

declareFunBS :: Show a => a -> String -> BS.Builder
declareFunBS v t = app "declare-fun" [stringBS (show v), sexpr [], stringBS t]

declareIntBS :: Show (NLiteral s) => NLiteral s -> BS.Builder
declareIntBS v = declareFunBS v "Int"

declareBoolBS :: Show (BLiteral s) => BLiteral s -> BS.Builder
declareBoolBS v = declareFunBS v "Bool"

assertBS :: (Show (BLiteral s), Show (NLiteral s)) => SMTFormula s -> BS.Builder
assertBS f                      = app "assert" [formToBS f] where
  formToBS Top                  = stringBS "true"
  formToBS Bot                  = stringBS "false"
  formToBS (Lit (BoolLit l))    = stringBS (show l)
  formToBS (Lit (NegBoolLit l)) = app "not" [stringBS (show l)]
  formToBS (Atom (Eq e1 e2))    = app "=" [ expressionToBS e1, expressionToBS e2 ]
  formToBS (Atom (Geq e1 e2))   = app ">=" [ expressionToBS e1, expressionToBS e2 ]
  formToBS (And f1 f2)          = app "and" [ formToBS f1, formToBS f2 ]
  formToBS (Or f1 f2)           = app "or" [ formToBS f1, formToBS f2 ]
  formToBS Iff{}                = error "SmtLib2.assert: Iff not defined"
  formToBS LetB{}               = error "SmtLib2.assert: Iff not defined"

  expressionToBS e = polyToBS e where
    add []       = qBS 0
    add [a]      = a
    add as       = app "+" as
    mul 0 _      = qBS 0
    mul c []     = qBS c
    mul 1 [a]    = a
    mul (-1) [a] = app "-" [a]
    mul 1 as     = app "*" as
    mul c as     = app "*" (qBS c : as)

    polyToBS (Poly.toMonos -> ms)    = add [ monoToBS c m | (c,m) <- ms ]
    monoToBS c (Poly.toPowers -> ps) = mul c (concat [ replicate ex (stringBS (show v)) | (v,ex) <- ps ])

checkSatBS :: BS.Builder
checkSatBS = app "check-sat" []

getValueBS :: Show (NLiteral s) => NLiteral s -> BS.Builder
getValueBS l = app "get-value" [sexpr [stringBS (show l)]]

