module UnambiguousAST where

import qualified SymbolicExpression as X
import qualified AST
import qualified Types as T

type Name = String

data Function = Function T.Integral Name [(Name, T.Type)] Stmt

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
          | DeclareHeapObj T.Complex Name Name
          | DeclareStack T.Integral Name
          | DeclareStackObj T.Complex Name
          | While Expr [Stmt]
          | For Stmt Expr Stmt [Stmt]
          | Return Expr