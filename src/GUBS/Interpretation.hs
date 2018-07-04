module GUBS.Interpretation where

import           Data.Maybe (fromMaybe)
import           Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           GUBS.Algebra
import qualified GUBS.MaxPolynomial as P
import           GUBS.Term (Term (..))
import qualified GUBS.Term as T
-- import qualified GUBS.Term as T

newtype Var = V Int deriving (Eq, Ord, Show)

variables :: [Var]
variables = [V i | i <- [0..]]

newtype Interpretation f c = Inter (M.Map (f,Int) (P.MaxPoly Var c))
  deriving (Show, Functor, Foldable, Traversable)

domain :: Interpretation f c -> [(f,Int)]
domain (Inter m) = M.keys m

image :: Interpretation f c -> [P.MaxPoly Var c]
image (Inter m) = M.elems m

maxDegree :: Interpretation f c -> Int
maxDegree = maximum . fmap (P.degree . snd) . toList

get :: Ord f => Interpretation f c -> f -> Int -> Maybe (P.MaxPoly Var c)
get (Inter m) f i = M.lookup (f,i) m

get' :: Ord f => Interpretation f c -> f -> Int -> P.MaxPoly Var c
get' inter f i = fromMaybe err (get inter f i) where
   err = error "GUBS.interpretation: function symbol not found"  

insert :: Ord f => Interpretation f c -> f -> Int -> P.MaxPoly Var c -> Interpretation f c 
insert (Inter m) f i p = Inter (M.insert (f,i) p m)

empty :: Interpretation f c
empty = Inter M.empty

union :: Ord f => Interpretation f c -> Interpretation f c -> Interpretation f c
union (Inter m1) (Inter m2) = Inter (m1 `M.union` m2)

unions :: Ord f => [Interpretation f c] -> Interpretation f c
unions = foldl' union empty 

restrict :: Ord f => (f -> Int -> Bool) -> Interpretation f c -> Interpretation f c
restrict p (Inter m) = Inter (M.filterWithKey (\(f,i) _ -> p f i) m)

apply :: (Eq c, SemiRing c, Ord v) => P.MaxPoly Var c -> [P.MaxPoly v c] -> P.MaxPoly v c
apply p args = P.substitute s p where
  s (V i) | i < length args = args !! i
          | otherwise       = error "Interpretation.apply: insufficient arguments"

fromList :: Ord f => [((f,Int), P.MaxPoly Var c)] -> Interpretation f c
fromList = Inter . M.fromList

toList :: Interpretation f c -> [((f,Int), P.MaxPoly Var c)]
toList (Inter m) = M.toList m

mapInter :: (P.MaxPoly Var c -> P.MaxPoly Var c') -> Interpretation f c -> Interpretation f c'
mapInter f (Inter m) = Inter (M.map f m)

interpret :: (Ord f, Ord v, Eq c, Max c, SemiRing c, IsNat c) => Interpretation f c -> Term f v -> Maybe (P.MaxPoly v c)
interpret i = T.interpretM (return . P.variable) im where
  im f as = apply <$> get i f (length as) <*> return as

pInterpret :: (Ord f, Integral c) => Interpretation f c -> Term f v -> Term f v
pInterpret i = T.interpret Var im where
  im f as = case get i f (length as) of
              Nothing -> Fun f ts
              Just p -> T.substitute s (P.fromMaxPoly Var fromNatural p)
    where
      ts = pInterpret i `map` as
      s (V j) | j < length ts = ts !! j
              | otherwise      = error "Interpretation.pInterpret: insufficient arguments"

-- pretty printers
instance PP.Pretty Var where
  pretty (V i) = PP.text "x" PP.<> PP.int i

instance (Eq c, IsNat c, SemiRing c, PP.Pretty f, PP.Pretty c) => PP.Pretty (Interpretation f c) where
  pretty inter = PP.vcat [ pp b | b <- toList inter ] where
    pp ((f,i),p) = PP.pretty f PP.<> PP.parens (PP.hcat (PP.punctuate (PP.text ",") [PP.pretty v | v <- take i variables]))
                PP.<+> PP.text "=" PP.<+> PP.pretty p PP.<> PP.text ";"

