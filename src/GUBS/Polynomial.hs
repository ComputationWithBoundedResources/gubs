module GUBS.Polynomial where

import           Data.Maybe (fromMaybe)
import           Data.List (foldl1')
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Data.Functor.Identity        (runIdentity)
import           Data.MultiSet (MultiSet)
import qualified Data.Map.Strict as M
import qualified Data.MultiSet as MS
import qualified Data.Set as S

import           GUBS.Algebra
import           GUBS.Constraint (Constraint(..))

newtype Monomial v = Mono (MultiSet v)
  deriving (Eq, Ord, Show)

newtype Polynomial v c = Poly (M.Map (Monomial v) c)
  deriving (Eq, Ord, Show)

deriving instance Functor (Polynomial v)
deriving instance Foldable (Polynomial v)
deriving instance Traversable (Polynomial v)


-- monomials
fromPowers :: Ord v => [(v,Int)] -> Monomial v
fromPowers = Mono . MS.fromOccurList

toPowers :: Monomial v -> [(v,Int)]
toPowers (Mono m) = MS.toOccurList m

unitMono :: Monomial v
unitMono = Mono MS.empty

fromMono :: IsNat c => Monomial v -> Polynomial v c 
fromMono m = Poly (M.singleton m (fromNatural 1))

monoVariables :: Ord v => Monomial v -> [v]
monoVariables (Mono m) = S.toAscList (MS.toSet m)

monoIsFactorOf :: Ord v => Monomial v -> Monomial v -> Bool
Mono m1 `monoIsFactorOf` Mono m2 = m1 `MS.isSubsetOf` m2

-- polynomials

coefficient :: c -> Polynomial v c
coefficient c = Poly (M.singleton (Mono MS.empty) c)

variable :: (IsNat c, Ord v) => v -> Polynomial v c
variable v = Poly (M.singleton (Mono (MS.singleton v)) (fromNatural 1))

fromMonos :: (IsNat c, Ord v) => [(c,Monomial v)] -> Polynomial v c
fromMonos ms = Poly (M.fromList [ (m,c) | (c,m) <- ms])

toMonos :: Polynomial v c -> [(c,Monomial v)]
toMonos (Poly ms) = [ (c,m) | (m,c) <- M.toList ms ]

toMonoMap :: Polynomial v c -> M.Map (Monomial v) c
toMonoMap (Poly m) = m

coefficientOf :: (Ord v, Additive c) => Monomial v -> Polynomial v c -> c
coefficientOf m (Poly p) = fromMaybe zero (M.lookup m p)

instance IsNat c => IsNat (Polynomial v c) where
  fromNatural_ 0 = Poly M.empty
  fromNatural_ c = coefficient (fromNatural c)

-- scale :: (IsNat c, SemiRing c) => c -> Polynomial v c -> Polynomial v c
-- scale c (Poly ms) 
--   | isZero c  = fromNatural 0
--   | otherwise = Poly (M.map (.* c) ms)

coefficients :: Polynomial v c -> [c]
coefficients (Poly ms) = M.elems ms

variables :: (Eq v, Ord v) => Polynomial v c -> [v]
variables p = S.toAscList (S.unions [ MS.toSet m | (_,Mono m) <- toMonos p ])

rename :: Ord v' => (v -> v') -> Polynomial v c -> Polynomial v' c
rename f (Poly ms) = Poly (M.mapKeys (\ (Mono m) -> Mono (MS.map f m)) ms)

norm :: (IsNat c, Eq c) => Polynomial v c -> Polynomial v c
norm (Poly ms) = Poly (M.filter (fromNatural 0 /=) ms)

isZero :: (Eq c, IsNat c) => Polynomial v c -> Bool
isZero p = and [ c == fromNatural 0 || null (toPowers m) | (c,m) <- toMonos p ]

zeroPoly :: IsNat c => Polynomial v c
zeroPoly = fromNatural 0

instance (IsNat c, Additive c, Ord v) => Additive (Polynomial v c) where
  zero = zeroPoly
  Poly ms1 .+ Poly ms2 = Poly (M.unionWith (.+) ms1 ms2) -- norm

instance (IsNat c, AdditiveGroup c, Ord v) => AdditiveGroup (Polynomial v c) where
  neg = fmap neg

instance (SemiRing c, Ord v, IsNat c) => Multiplicative (Polynomial v c) where
  one = fromNatural 1
  Poly ms1 .* Poly ms2 = Poly (M.fromListWith (.+) ms) where -- norm
    ms = [ (m1 `mult` m2, c1 .* c2) | (m1,c1) <- M.toList ms1, (m2,c2) <- M.toList ms2 ]
    Mono ps1 `mult` Mono ps2 = Mono (MS.union ps1 ps2)

factorise :: (Eq c, IsNat c, Integral c, SemiRing c, Ord v) => [Polynomial v c] -> Maybe ((c,Monomial v), [Polynomial v c])
factorise (fmap (toMonos . norm) -> ps)
  | leq1 monos       = Nothing
  | MS.size msf == 0 = Nothing
  | otherwise        = Just ( (cf,Mono msf) , map factor ps)
  where
    monos  = concat ps
    
    msf = foldl1' MS.intersection [ m | (_,Mono m) <- monos ]
    cf = foldl1' gcd [ c | (c,_) <- monos]

    factor p = fromMonos [ (c `div` cf, Mono (m MS.\\ msf))  | (c, Mono m) <- p]
    
    leq1 []  = True
    leq1 [_] = True
    leq1 _   = False

substitute :: (Ord v', IsNat c, SemiRing c) => (v -> Polynomial v' c) -> Polynomial v c -> Polynomial v' c 
substitute s p = sumA [ substMono m | (_,m) <- toMonos p]
    where
      substMono m = prod [ sumA (replicate n (s v)) | (v,n) <- toPowers m]

fromPolynomial :: SemiRing a => (v -> a) -> (c -> a) -> Polynomial v c -> a
fromPolynomial var coeff p = sumA [ coeff c .* fromMonomial m | (c,m) <- toMonos p] where
  fromMonomial m = prod [ var v .^ vp | (v,vp) <- toPowers m]
 
evalWithM :: (SemiRing c, Monad m) => (v -> m c) -> Polynomial v c -> m c
evalWithM getValue = eval where
  eval p = sumA <$> sequence [ (.*) c <$> evalMono m | (c,m) <- toMonos p]
  evalMono m = prod <$> mapM evalPower (toPowers m)
  evalPower (v,e) = do
    c <- getValue v
    return (c .^ e)

evalWith :: SemiRing c => (v -> c) -> Polynomial v c -> c
evalWith getValue = runIdentity . evalWithM (return . getValue)

-- toNatural :: (IsNat c, IsNat n, SemiRing n) => (c -> n) -> (v -> n) -> Polynomial v c -> n
-- toNatural fromCoeff fromVar = fromPoly where
--   fromPoly p = sumA [ fromCoeff c .* fromMono m | (c,m) <- toMonos p ]
--   fromMono m = prod [ fromVar v .^ e | (v,e) <- toPowers m ]
  
-- pretty printers
   
ppPower :: PP.Pretty a => (a, Int) -> PP.Doc
ppPower (v,i) = PP.pretty v PP.<> if i == 1 then PP.empty else PP.char '^' PP.<> PP.int i

instance PP.Pretty v => PP.Pretty (Monomial v) where
  pretty mono = pretty' (toPowers mono) where
    pretty' [] = PP.char '1'
    pretty' ps = PP.hcat (PP.punctuate (PP.char '*') [ppPower p | p <- ps])

instance (PP.Pretty c, IsNat c, Eq c, PP.Pretty v) => PP.Pretty (Polynomial v c) where
  pretty poly = pretty' [p | p <- toMonos poly, fst p /= fromNatural 0] where
    pretty' [] = PP.char '0'
    pretty' ps = PP.hcat (PP.punctuate (PP.string " + ") (ppMono `map` ps))
    ppMono (c,mono) | c == fromNatural 1 = PP.pretty mono
    ppMono (c,toPowers -> []) = PP.pretty c
    ppMono (c,mono) = PP.pretty c PP.<> PP.char '*' PP.<> PP.pretty mono

   


data Diff c = Diff { posAC :: c , negAC :: c }

toDiff :: Additive c => c -> Diff c
toDiff c = Diff { posAC = c, negAC = zero }

negateDiff :: Diff c -> Diff c
negateDiff p = Diff { posAC = negAC p, negAC = posAC p}

instance IsNat c => IsNat (Diff c) where
  fromNatural_ n = Diff { posAC = fromNatural_ n, negAC = fromNatural 0 }

instance Additive c => Additive (Diff c) where
  zero = toDiff zero
  d1 .+ d2 = Diff { posAC = posAC d1 .+ posAC d2, negAC = negAC d1 .+ negAC d2 }

instance Additive c => AdditiveGroup (Diff c) where
  neg = negateDiff

type DiffPolynomial v c = Polynomial v (Diff c)

toDiffPoly :: Additive c => Polynomial v c -> DiffPolynomial v c
toDiffPoly = fmap toDiff 

absolutePositive :: DiffPolynomial v c -> [Constraint c]
absolutePositive p = [ posAC c :>=: negAC c | c <- coefficients p ]
 
minus :: (IsNat c, Additive c, Ord v) => Polynomial v c -> Polynomial v c -> DiffPolynomial v c
p1 `minus` p2 = toDiffPoly p1 .- toDiffPoly p2 
