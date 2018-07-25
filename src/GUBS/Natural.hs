module GUBS.Natural (module N) where


import GUBS.Algebra                  as S
import GUBS.MaxPolynomial            as S hiding (Const, Max, Mult, Plus, Var, constant, degree, substitute, variable)
import GUBS.MaxTerm                  as S hiding (funs, interpret, substitute)
import GUBS.Utils                    as S (PrettySexp (..))

import GUBS.Natural.Constraint       as S hiding (constraint)
import GUBS.Natural.ConstraintSystem as S hiding (constraint, funs)
import GUBS.Natural.Interpretation   as S hiding (variables)
import GUBS.Natural.Solve            as S

