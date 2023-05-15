module UnambiguousAST where

import qualified SymbolicExpression as SX
import qualified AST

type Name = String

data Expr = ArithExpr Expr AST.ArithOp Expr
          | LogExpr Expr AST.LogOp Expr
          | BitExpr Expr AST.BitOp Expr
          | RelExpr Expr AST.RelOp Expr
          | UnExpr AST.Unop Expr
          | Assign Expr Expr
          | Var Name
          | FunCall Name [Expr]
          | Index Name [Expr]
          | Int Integer
          | Char Char
          | Deref Expr

data Stmt = CompoundStmt [Stmt]
          | Expr Expr
          | IfElse Expr Stmt Stmt
          | DeclareHeapObj SX.Generic Name Name
          | DeclareStack SX.Base Name
          | DeclareStackObj SX.Generic Name Name
          | While Expr [Stmt]
          | For Stmt Expr Stmt [Stmt]
          | Return Expr