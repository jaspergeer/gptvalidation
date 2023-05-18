module SymbolicExpression where

import qualified AST
import Prelude hiding (Integral)
import qualified Types as T

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
          | NewArr Int T.Integral
          | FromType T.Integral Expr
          | PtrTo Name
          | Free Name
          deriving Show