module SymbolicExpression where

import qualified AST
import Data.SBV.Dynamic
import Prelude hiding (Integral)

type Name = String

-- untyped symbolic expressions

-- Upd should never appear except as a subexpression of Sel

-- Perhaps array forms should be a seperate type

data Expr = BinExpr Expr AST.BinOp Expr
          | LogExpr Expr AST.LogOp Expr
          | RelExpr Expr AST.RelOp Expr
          | ShiftExpr Expr AST.ShiftOp Expr
          | UnExpr AST.Unop Expr
          | Sel Expr [Expr]
          | Upd Expr [Expr] Expr
          | FunCall Name [Expr]
          | Choice Expr Expr Expr
          | Literal Integer
          | NewArr Int Integral
          | FromType Integral Expr
          | PtrTo Name
          | Free Name
          deriving Show

data Integral = Int32 | Int8 | U32 | Ptr Type deriving Show
data Complex = Arr Int Integral | Fun [Kind] Kind deriving Show

data Type = Integral Integral | Complex Complex deriving Show

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
