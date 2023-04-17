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

data Stmt = CompoundStmt [Stmt]
          | Expr Expr
          | IfElse Expr [Stmt] [Stmt]
          | DeclareHeap SX.Type Name Name
          | DeclareStackArr SX.Type Name Name
          | DeclareStack SX.Type Name
          | Upd Expr Expr -- *e = e;
          | While Expr [Stmt]
          | For Stmt Expr Stmt [Stmt]
          | Return Expr