module SymbolicExpression where

import qualified AST
import Data.Int

type Name = String

data Expr = BinExpr Expr AST.Binop Expr
          | UnExpr AST.Unop Expr
          | Sel Expr [Expr]
          | Upd Expr [Expr] Expr
          | FunCall Name [Expr]
          | Choice Expr Expr Expr
          | I32 Int32
          | I8 Int8
          | NewArr Int Base
          | Cast Type Expr
          | PtrTo Name

data Base = Int32 | Int8 | U32
data Generic = Ptr Type | Fun [Base] Base

data Type = Generic Generic | Base Base