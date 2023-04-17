module SymbolicExpression where

import qualified AST
import Data.Int

type Name = String

data TypedExpr

data Expr = BinExpr Expr AST.Binop Expr
          | Unop AST.Unop Expr
          | Sel Expr [Expr]
          | FunCall Name [Expr]
          | Choice Expr Expr Expr
          | I32 Int32
          | I8 Int8
          | NewArr [Int] Type
          | Cast Type Expr

data Type = Ptr Type | Int32 | Int8 | U32