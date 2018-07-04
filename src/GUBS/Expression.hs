module GUBS.Expression
       (
         Expression
       , variable
       , zero
       , fromPolynomial
       , evalWithM
       , evalWith
       ) where

import           GUBS.Polynomial

type Expression v = Polynomial v Integer

zero :: Expression v
zero = zeroPoly
