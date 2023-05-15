module SymbolicExpression where

import qualified AST
import Data.SBV.Dynamic

type Name = String

-- untyped symbolic expressions

-- Upd should never appear except as a subexpression of Sel

data Expr = BinExpr Expr AST.Binop Expr
          | UnExpr AST.Unop Expr
          | Sel Expr [Expr]
          | Upd Expr [Expr] Expr
          | FunCall Name [Expr]
          | Choice Expr Expr Expr
          | Literal Integer
          | NewArr Int Base
          | Enforce Type Expr
          | PtrTo Name

data Base = Int32 | Int8 | U32
data Generic = Ptr Type | Arr Int Base | Fun [Kind] Kind

data Type = Generic Generic | Base Base

dim :: Type -> Int
dim tau = case tau of
  Base _ -> 0
  Generic (Arr i _) -> i
  Generic (Ptr tau') -> 1 + dim tau'
  Generic (Fun {}) -> error "dim of function"

base :: Type -> Base
base tau = case tau of
  (Base b) -> b
  (Generic (Ptr tau')) -> base tau'
  _ -> error "TODO"
