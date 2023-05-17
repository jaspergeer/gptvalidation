module SymbolicExpression where

import qualified AST
import Data.SBV.Dynamic
import Prelude hiding (Integral)

type Name = String

-- untyped symbolic expressions

-- Upd should never appear except as a subexpression of Sel

data Expr = ArithExpr Expr AST.ArithOp Expr
          | LogExpr Expr AST.LogOp Expr
          | BitExpr Expr AST.BitOp Expr
          | RelExpr Expr AST.RelOp Expr
          | UnExpr AST.Unop Expr
          | Sel Expr [Expr]
          | Upd Expr [Expr] Expr
          | FunCall Name [Expr]
          | Choice Expr Expr Expr
          | Literal Integer
          | NewArr Int Integral
          | FromType Integral Expr
          | PtrTo Name

data Integral = Int32 | Int8 | U32 | Ptr Type
data Complex = Arr Int Integral | Fun [Kind] Kind

data Type = Integral Integral | Complex Complex

dim :: Type -> Int
dim tau = case tau of
  Integral _ -> 0
  Complex (Arr i _) -> i
  Complex (Fun {}) -> error "IMPOSSIBLE: compute dim of function"

base :: Type -> Integral
base tau = case tau of
  Integral t -> t
  Complex (Arr _ t) -> t
  _ -> error "IMPOSSIBLE: compute base of function"
