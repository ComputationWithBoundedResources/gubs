module GUBS.Interpretation where


import           Data.List                    (foldl')
import qualified Data.Map.Strict              as M
import           Data.Maybe                   (fromMaybe)
import qualified Text.PrettyPrint.ANSI.Leijen as PP


newtype Var = V Int deriving (Eq, Ord, Show)

variables :: [Var]
variables = [V i | i <- [0..]]

newtype Interpretation fun img = Inter (M.Map (fun,Int) img)
  deriving (Show, Functor, Foldable, Traversable)

domain :: Interpretation f i -> [(f,Int)]
domain (Inter m) = M.keys m

image :: Interpretation f i -> [i]
image (Inter m) = M.elems m

get :: Ord f => Interpretation f i -> f -> Int -> Maybe i
get (Inter m) f i = M.lookup (f,i) m

get' :: Ord f => Interpretation f i -> f -> Int -> i
get' inter f i = fromMaybe err (get inter f i) where
   err = error "GUBS.Interpretation: function symbol not found."

insert :: Ord f => Interpretation f i -> f -> Int -> i -> Interpretation f i
insert (Inter m) f i p = Inter (M.insert (f,i) p m)

empty :: Interpretation f i
empty = Inter M.empty

union :: Ord f => Interpretation f i -> Interpretation f i -> Interpretation f i
union (Inter m1) (Inter m2) = Inter (m1 `M.union` m2)

unions :: Ord f => [Interpretation f i] -> Interpretation f i
unions = foldl' union empty

restrict :: (f -> Int -> Bool) -> Interpretation f i -> Interpretation f i
restrict p (Inter m) = Inter (M.filterWithKey (\(f,i) _ -> p f i) m)


fromList :: Ord f => [((f,Int), i)] -> Interpretation f i
fromList = Inter . M.fromList

toList :: Interpretation f i -> [((f,Int), i)]
toList (Inter m) = M.toList m

mapInter :: (i -> i') -> Interpretation f i -> Interpretation f i'
mapInter f (Inter m) = Inter (M.map f m)



-- pretty printers

instance PP.Pretty Var where
  pretty (V i) = PP.text "x" PP.<> PP.int i

instance (PP.Pretty f, PP.Pretty i) => PP.Pretty (Interpretation f i) where
  pretty inter = PP.vcat [ pp b | b <- toList inter ] where
    pp ((f,n),i) = PP.pretty f PP.<> PP.parens (PP.hcat (PP.punctuate (PP.text ",") [PP.pretty v | v <- take n variables]))
                PP.<+> PP.text "=" PP.<+> PP.pretty i PP.<> PP.text ";"

