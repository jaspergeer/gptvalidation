module AST where

import qualified Types as T

data BinOp = Add | Sub | Mul | Mod | Div | BAnd | BOr | BXor deriving Show -- arithmetic/bitwise
data LogOp = LAnd | LOr deriving Show -- boolean
data RelOp = Eq | Leq | Geq | Neq | Lt | Gt deriving Show -- comparison
data ShiftOp = Shl | Shr deriving Show

data Unop = Neg | LNot | BNot deriving Show

data Function = Function T.Integral Name [(Name, T.Type)] Stmt deriving Show

data Expr = BinExpr Expr BinOp Expr
          | LogExpr Expr LogOp Expr
          | ShiftExpr Expr ShiftOp Expr
          | RelExpr Expr RelOp Expr
          | UnExpr Unop Expr
          | AssignExpr Expr Expr
          | Var Name
          | FunCall Name [Expr]
          | Int Integer
          | Str String
          | Char Char
          | Index Name [Expr]
          | Deref Expr
          | Alloc T.Integral Int
          deriving Show

type Name = String

data Stmt = CompoundStmt [Stmt]
          | Expr Expr
          | IfElse Expr Stmt Stmt
          | DeclareAssign T.Type Name Expr
          | Declare T.Type Name
          | While Expr [Stmt]
          | For Stmt Expr Stmt [Stmt]
          | Return Expr
          deriving Show
