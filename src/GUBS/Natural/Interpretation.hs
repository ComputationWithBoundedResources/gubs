module GUBS.Natural.Interpretation
 ( module I
 , Interpretation
 , maxDegree
 , apply
 , interpret
 , pInterpret
 )
where


import           GUBS.Interpretation as I hiding (Interpretation)
import qualified GUBS.Interpretation as I (Interpretation)
import qualified GUBS.MaxPolynomial  as MP
import qualified GUBS.MaxTerm        as MT


type Interpretation f c = I.Interpretation f (MP.MaxPoly Var c)


maxDegree :: Interpretation f c -> Int
maxDegree = maximum . fmap (MP.degree . snd) . toList

apply :: Num c => MP.MaxPoly Var c -> [MP.MaxPoly v' c] -> MP.MaxPoly v' c
apply p args = MP.substitute s p where
  s (V i) | i < length args = args !! i
          | otherwise       = error "Interpretation.apply: insufficient arguments"

interpret :: (Ord f, Num c) => Interpretation f c -> MT.Term f v c -> Maybe (MP.MaxPoly v c)
interpret i = MT.interpretM (pure .  MP.variable) im (pure . MP.constant) where
  im f as = apply <$> get i f (length as) <*> return as

pInterpret :: (Num c, Ord f) => Interpretation f c -> MT.Term f v c -> MT.Term f v c
pInterpret i = MT.interpret MT.Var im MT.Const where
  im f as = case get i f (length as) of
              Nothing -> MT.Fun f ts
              Just p  -> MT.substitute s (MP.fromMaxPoly MT.Var MT.Const p)
    where
      ts = pInterpret i `map` as
      s (V j) | j < length ts = ts !! j
              | otherwise      = error "Interpretation.pInterpret: insufficient arguments"

