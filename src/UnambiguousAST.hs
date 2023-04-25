module UnambiguousAST where

import qualified SymbolicExpression as SX
import qualified AST

type Name = String

data Expr = BinExpr Expr AST.Binop Expr
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
          | Upd Expr Expr -- *e = e;
          | While Expr [Stmt]
          | For Stmt Expr Stmt [Stmt]
          | Return Expr