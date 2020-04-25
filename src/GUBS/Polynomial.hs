{-# LANGUAGE TupleSections #-}
module GUBS.Polynomial where


import           Data.Functor.Identity        (runIdentity)
import           Data.List                    (foldl1')
import qualified Data.Map.Merge.Strict        as M
import qualified Data.Map.Strict              as M
import           Data.Maybe                   (fromMaybe)
import           Data.MultiSet                (MultiSet)
import qualified Data.MultiSet                as MS
import qualified Data.Set                     as S
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           GUBS.Algebra


newtype Monomial v = Mono (MultiSet v)
  deriving (Eq, Ord, Show)

newtype Polynomial v c = Poly (M.Map (Monomial v) c)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)



-- monomials

unitMono :: Monomial v
unitMono = Mono MS.empty

fromPowers :: Ord v => [(v,Int)] -> Monomial v
fromPowers = Mono . MS.fromOccurList

toPowers :: Monomial v -> [(v,Int)]
toPowers (Mono m) = MS.toOccurList m

monoVariables :: Monomial v -> [v]
monoVariables (Mono m) = S.toAscList (MS.toSet m)

monoIsFactorOf :: Ord v => Monomial v -> Monomial v -> Bool
Mono m1 `monoIsFactorOf` Mono m2 = m1 `MS.isSubsetOf` m2


-- polynomials

unit :: Polynomial c v
unit = Poly M.empty

coefficient :: (Eq c, Num c) => c -> Polynomial v c
coefficient c = norm (Poly (M.singleton (Mono MS.empty) c))

variable :: Num c => v -> Polynomial v c
variable v = Poly (M.singleton (Mono (MS.singleton v)) one)

fromMono :: Num c =>  Monomial v -> Polynomial v c
fromMono m = Poly (M.singleton m one)

fromMonos :: Ord v => [(c,Monomial v)] -> Polynomial v c
fromMonos ms = Poly (M.fromList [ (m,c) | (c,m) <- ms])

toMonos :: Polynomial v c -> [(c,Monomial v)]
toMonos (Poly ms) = [ (c,m) | (m,c) <- M.toList ms ]

toMonoMap :: Polynomial v c -> M.Map (Monomial v) c
toMonoMap (Poly m) = m


coefficientOf :: (Ord v, Num c) => Monomial v -> Polynomial v c -> c
coefficientOf m (Poly p) = fromMaybe zero (M.lookup m p)

coefficients :: Polynomial v c -> [c]
coefficients (Poly ms) = M.elems ms

variables :: Ord v => Polynomial v c -> [v]
variables p = S.toAscList (S.unions [ MS.toSet m | (_,Mono m) <- toMonos p ])

rename :: (Eq c, Num c, Ord v') => (v -> v') -> Polynomial v c -> Polynomial v' c
rename f (Poly ms) = norm $ Poly (M.mapKeysWith (+) (\ (Mono m) -> Mono (MS.map f m)) ms)

norm :: (Num c, Eq c) => Polynomial v c -> Polynomial v c
norm (Poly ms) = Poly (M.filter (zero /=) ms)

isZero :: (Num c, Eq c) => Polynomial v c -> Bool
isZero p = and [ c == zero || null (toPowers m) | (c,m) <- toMonos p ]

zeroPoly :: Polynomial v c
zeroPoly = Poly (M.empty)

instance (Eq c, Ord v, Num c) => Num (Polynomial v c) where
  Poly ms1 + Poly ms2 = norm $ Poly (M.unionWith (+) ms1 ms2)
  Poly ms1 * Poly ms2 = norm $ Poly (M.fromListWith (+) ms) where
    ms =
      [ (Mono (m1 `MS.union` m2), c1 * c2)
      | (Mono m1,                 c1) <- M.toList ms1
      , (Mono m2,                 c2) <- M.toList ms2 ]
  negate      = fmap negate
  fromInteger = coefficient . fromInteger
  abs         = error "Polynomial.Num.abs: not defined."
  signum      = error "Polynomial.Num.signum: not defined."



-- eval

substitute :: (Eq c, Num c, Ord v') => (v -> Polynomial v' c) -> Polynomial v c -> Polynomial v' c
substitute s = substPoly where
    substPoly p = sumA [ coefficient c * substMono m | (c,m) <- toMonos p]
    substMono m = prod [ prod $ replicate p (s v)     | (v,p) <- toPowers m]

fromPolynomialM :: (Num a, Applicative f) => (v -> f a) -> (c -> f a) -> Polynomial v c -> f a
fromPolynomialM var coeff = evalPoly where
  evalPoly p = sumA <$> sequenceA [ (*) <$> coeff c <*> evalMono m | (c,m) <- toMonos  p ]
  evalMono m = prod <$> sequenceA [ (^) <$> var v   <*> pure p     | (v,p) <- toPowers m ]

fromPolynomial :: Num a => (v -> a) -> (c -> a) -> Polynomial v c -> a
fromPolynomial var coeff = runIdentity . fromPolynomialM (pure . var) (pure . coeff)

evalWithM :: (Num c, Applicative m) => (v -> m c) -> Polynomial v c -> m c
evalWithM getValue = evalPoly where
  evalPoly p = sumA <$> sequenceA [ (c *) <$> evalMono m | (c,m) <- toMonos p]
  evalMono m = prod <$> sequenceA [ (^ p) <$> getValue v | (v,p) <- toPowers m ]

evalWith :: Num c => (v -> c) -> Polynomial v c -> c
evalWith getValue = runIdentity . evalWithM (return . getValue)

zipCoefficientsWith :: Ord v => (c1 -> c3) -> (c2 -> c3) -> (c1 -> c2 -> c3) -> Polynomial v c1 -> Polynomial v c2 -> Polynomial v c3
zipCoefficientsWith f1 f2 f (Poly m1) (Poly m2) = Poly $
  M.merge
    (M.mapMissing     (\_ c1    -> f1 c1))
    (M.mapMissing     (\_ c2    -> f2 c2))
    (M.zipWithMatched (\_ c1 c2 -> f c1 c2))
    m1
    m2

zipCoefficients :: (Ord v, Num c1, Num c2) => Polynomial v c1 -> Polynomial v c2 -> [(c1,c2)]
zipCoefficients p1 p2 = coefficients $
  zipCoefficientsWith (,zero) (zero,) (,) p1 p2

factorise :: (Integral c, Ord v) => [Polynomial v c] -> Maybe ((c,Monomial v), [Polynomial v c])
factorise (fmap (toMonos . norm) -> ps)
  | leq1 monos                  = Nothing
  | MS.size msf == 0 && cf == 1 = Nothing
  | otherwise                   = Just ( (cf,Mono msf) , map factor ps)
  where
    monos  = concat ps

    msf = foldl1' MS.intersection [ m | (_,Mono m) <- monos ]
    cf = foldl1' gcd [ c | (c,_) <- monos]

    factor p = fromMonos [ (c `div` cf, Mono (m MS.\\ msf))  | (c, Mono m) <- p]

    leq1 []  = True
    leq1 [_] = True
    leq1 _   = False

factoriseMono :: (Ord v) => [Polynomial v c] -> Maybe (Monomial v, [Polynomial v c])
factoriseMono (fmap toMonos -> ps)
  | leq1 monos       = Nothing
  | MS.size msf == 0 = Nothing
  | otherwise        = Just (Mono msf , map factor ps)
  where
    monos  = concat ps

    msf = foldl1' MS.intersection [ m | (_,Mono m) <- monos ]

    factor p = fromMonos [ (c, Mono (m MS.\\ msf))  | (c, Mono m) <- p]

    leq1 []  = True
    leq1 [_] = True
    leq1 _   = False


-- pretty printers

ppPower :: (v -> PP.Doc) -> (v, Int) -> PP.Doc
ppPower ppVar (v,i) = ppVar v PP.<> if i == 1 then PP.empty else PP.char '^' PP.<> PP.int i

ppMono :: (v -> PP.Doc) -> Monomial v -> PP.Doc
ppMono _ (toPowers -> []) = PP.char '1'
ppMono ppVar (toPowers -> ps) = PP.hcat (PP.punctuate (PP.char '·') [ppPower ppVar p | p <- ps])


ppPoly :: (Num c, Eq c) => (v -> PP.Doc) -> (c -> PP.Doc) -> Polynomial v c -> PP.Doc
ppPoly _ _ (toMonos -> []) = PP.char '0'
ppPoly ppVar ppCoeff (toMonos -> ps) = PP.hcat (PP.punctuate (PP.string " + ") (ppMono' `map` ps))
  where
    ppMono' (c,mono) | c == one        = ppMono ppVar mono
    ppMono' (c,mono) | c == negate one = PP.pretty "-" PP.<> ppMono ppVar mono
    ppMono' (c,toPowers -> [])         = ppCoeff c
    ppMono' (c,mono)                   = ppCoeff c PP.<> PP.char '·' PP.<> PP.parens (ppMono ppVar mono)


instance (PP.Pretty v) => PP.Pretty (Monomial v) where
  pretty = ppMono PP.pretty

instance (Num c, Eq c, PP.Pretty c, PP.Pretty v) => PP.Pretty (Polynomial v c) where
  pretty = ppPoly PP.pretty PP.pretty


-- Difference Polynomial

data Diff c = Diff { posAC :: c , negAC :: c } deriving Eq
type DiffPolynomial v c = Polynomial v (Diff c)

instance Num c => Num (Diff c) where
  d1 + d2       = Diff { posAC = posAC d1 + posAC d2, negAC = negAC d1 * negAC d2 }
  d1 * d2       = Diff { posAC = posAC d1 * posAC d2, negAC = negAC d1 * negAC d2 }
  negate d      = Diff { posAC = negAC d, negAC = posAC d }
  fromInteger c = Diff { posAC = fromInteger c, negAC = zero }
  abs           = error "Polynomial.Num.abs: not defined."
  signum        = error "Polynomial.Num.signum: not defined."

minus :: (Eq c, Ord v,  Num c) => Polynomial v c -> Polynomial v c -> DiffPolynomial v c
minus p1 p2 = fmap k p1 - fmap k p2
  where k c =  Diff { posAC = c, negAC = zero }
