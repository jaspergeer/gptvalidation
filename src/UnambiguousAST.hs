module UnambiguousAST where

import qualified SymbolicExpression as X
import qualified AST

type Name = String

data Function = Function X.Integral Name [(Name, X.Integral)] Stmt

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
          | DeclareHeapObj X.Complex Name Name
          | DeclareStack X.Integral Name
          | DeclareStackObj X.Complex Name Name
          | While Expr [Stmt]
          | For Stmt Expr Stmt [Stmt]
          | Return Expr