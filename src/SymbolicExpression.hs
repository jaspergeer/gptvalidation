module SymbolicExpression where

import qualified AST

type Name = String

data Expr = BinExpr Expr AST.Binop Expr
          | UnExpr AST.Unop Expr
          | Sel Expr [Expr]
          | Upd Expr [Expr] Expr
          | FunCall Name [Expr]
          | Choice Expr Expr Expr
          | I32 Int
          | I8 Int
          | NewArr Int Base
          | Enforce Type Expr
          | PtrTo Name

data Base = Int32 | Int8 | U32
data Generic = Ptr Type | Arr Int Base | Fun [Base] Base

data Type = Generic Generic | Base Base