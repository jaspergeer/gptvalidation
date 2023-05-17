module UnambiguousAST where

import qualified SymbolicExpression as X
import qualified AST

type Name = String

data Function = Function X.Integral Name [(Name, X.Integral)] Stmt

data Expr = BinExpr Expr AST.BinOp Expr
          | LogExpr Expr AST.LogOp Expr
          | RelExpr Expr AST.RelOp Expr
          | ShiftExpr Expr AST.ShiftOp Expr
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
          | DeclareHeapObj X.Complex Name Name
          | DeclareStack X.Integral Name
          | DeclareStackObj X.Complex Name Name
          | While Expr [Stmt]
          | For Stmt Expr Stmt [Stmt]
          | Return Expr