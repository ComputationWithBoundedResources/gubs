module GUBS.Natural.Interpretation
 ( module I
 , Interpretation
 , maxDegree
 , apply
 , interpret
 , pInterpret 
 )
where


import           GUBS.Algebra
import           GUBS.Interpretation as I hiding (Interpretation)
import qualified GUBS.Interpretation as I (Interpretation)
import qualified GUBS.MaxPolynomial  as MP
import qualified GUBS.MaxTerm        as MT


type Interpretation f c = I.Interpretation f (MP.MaxPoly Var c)


maxDegree :: Interpretation f c -> Int
maxDegree = maximum . fmap (MP.degree . snd) . toList

apply :: (Eq c, SemiRing c) => MP.MaxPoly Var c -> [MP.MaxPoly v c] -> MP.MaxPoly v c
apply p args = MP.substitute s p where
  s (V i) | i < length args = args !! i
          | otherwise       = error "Interpretation.apply: insufficient arguments"

interpret :: (Ord f, Eq c, Max c, SemiRing c, IsNat c) => Interpretation f c -> MT.Term f v -> Maybe (MP.MaxPoly v c)
interpret i = MT.interpretM (return .  MP.variable) im where
  im f as = apply <$> get i f (length as) <*> return as

pInterpret :: (Ord f, Integral c) => Interpretation f c -> MT.Term f v -> MT.Term f v
pInterpret i = MT.interpret MT.Var im where
  im f as = case get i f (length as) of
              Nothing -> MT.Fun f ts
              Just p -> MT.substitute s (MP.fromMaxPoly MT.Var fromNatural p)
    where
      ts = pInterpret i `map` as
      s (V j) | j < length ts = ts !! j
              | otherwise      = error "Interpretation.pInterpret: insufficient arguments"

