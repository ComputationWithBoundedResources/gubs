module GUBS.Solver.Formula where

import Data.Foldable (toList)


data Atom e = Geq e e | Eq e e
  deriving (Functor, Foldable, Traversable)

data BoolLit l = BoolLit l | NegBoolLit l
  deriving (Functor, Foldable, Traversable)

-- TODO gadtify
data Formula l e =
  Top
  | Bot
  | Lit (BoolLit l)
  | Atom (Atom e)
  | Or (Formula l e) (Formula l e)
  | And (Formula l e) (Formula l e)
  | Iff (Formula l e) (Formula l e)
  | LetB (Formula l e) (l -> Formula l e)

subst :: (Atom e -> Formula l e') -> Formula l e -> Formula l e'
subst _ Top = Top
subst _ Bot = Bot
subst _ (Lit l) = Lit l
subst f (Atom a) = f a
subst f (Or e1 e2) = Or (subst f e1) (subst f e2)
subst f (And e1 e2) = And (subst f e1) (subst f e2)
subst f (LetB e1 e2) = LetB (subst f e1) (subst f . e2)
subst f (Iff e1 e2) = Iff (subst f e1) (subst f e2)

literal :: l -> Formula l e
literal = Lit . BoolLit

atoms :: Formula l e -> [Atom e]
atoms (Atom a) = [a]
atoms Top = []
atoms Bot = []
atoms Lit{} = []
atoms (Or f1 f2) = atoms f1 ++ atoms f2
atoms (And f1 f2) = atoms f1 ++ atoms f2
atoms (Iff f1 f2) = atoms f1 ++ atoms f2
atoms (LetB f1 f2) = atoms f1 ++ atoms (f2 undefined)


negLiteral :: l -> Formula l e
negLiteral = Lit . NegBoolLit

eqA,geqA :: e -> e -> Formula l e
eqA e1 e2  = Atom (Eq e1 e2)
geqA e1 e2 = Atom (Geq e1 e2)


smtTop, smtBot :: Formula b e
smtTop = Top
smtBot = Bot

smtBool :: Bool -> Formula b e
smtBool True  = Top
smtBool False = Bot

-- smtNot :: (IsNat e, Additive e) => Formula l e -> Formula l e
-- smtNot Top = Bot
-- smtNot Bot = Top
-- smtNot (Lit (BoolLit l)) = smtNot (Lit (NegBoolLit l))
-- smtNot (Lit (NegBoolLit l)) = smtNot (Lit (BoolLit l))
-- smtNot (Atom (Geq e1 e2)) = e2 `gtA` e1
-- smtNot (Atom (Eq e1 e2)) = Or (e1 `gtA` e2) (e2 `gtA` e1)
-- smtNot (Or f1 f2) = And (smtNot f1) (smtNot f2)
-- smtNot (And f1 f2) = Or (smtNot f1) (smtNot f2)
-- smtNot (Iff f1 f2) = Iff (smtNot f1) f2
-- smtNot (LetB f1 f2) = LetB f1 (smtNot . f2)


smtAnd, smtOr :: Formula l e -> Formula l e -> Formula l e
Top `smtAnd` f2  = f2
f1  `smtAnd` Top = f1
Bot `smtAnd` _   = Bot
_   `smtAnd` Bot = Bot
f1  `smtAnd` f2  = And f1 f2

Bot `smtOr` f2  = f2
f1  `smtOr` Bot = f1
Top `smtOr` _   = Top
_   `smtOr` Top = Top
f1  `smtOr` f2  = Or f1 f2


smtBigOr, smtBigAnd :: [Formula l e] -> Formula l e
smtBigOr = foldr smtOr smtBot
smtBigAnd = foldr smtAnd smtTop

smtAll, smtAny :: Foldable t => (a -> Formula l e) -> t a -> Formula l e
smtAll f t = smtBigAnd [ f a | a <- toList t]
smtAny f t = smtBigOr [ f a | a <- toList t]


letB ::  Formula l e -> (l -> Formula l e) -> Formula l e
letB = LetB

letB' :: [Formula l e] -> ([l] -> Formula l e) -> Formula l e
letB' = walk [] where
  walk ls [] f = f (reverse ls)
  walk ls (e:es) f = LetB e (\ l -> walk (l:ls) es f)

