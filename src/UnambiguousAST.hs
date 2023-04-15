module UnambiguousAST where

import qualified SymbolicExpression as SX

import Data.Int
-- import Data.Word

type Name = String

data Binop = Add | Sub | Mul | Mod | Div -- arithmetic
           | LAnd | LOr -- boolean
           | BAnd | BOr | Shl | Shr -- binary
           | Eq | Leq | Geq | Lt | Gt -- comparison

data Unop = Neg | LNot | BNot | Deref

data Expr = BinExpr Expr Binop Expr
                | UnExpr Unop Expr
                | Assign Expr Expr
                | Var Name
                | FunCall Name [Expr]
                | Index Name [Expr]
                | I32 Int32
                | I8 Int8

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