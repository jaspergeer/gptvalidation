module AST where

data Binop = Add | Sub | Mul | Mod | Div -- arithmetic
           | LAnd | LOr -- boolean
           | BAnd | BOr | Shl | Shr -- bitwise
           | Eq | Leq | Geq | Lt | Gt -- comparison

data Unop = Neg | LNot | BNot

data Expr = BinExpr Expr Binop Expr
          | UnExpr Unop Expr
          | AssignExpr Expr Expr
          | Var Name
          | FunCall Name [Expr]
          | Int Int
          | Str String
          | Char Char
          | Index Name [Expr]
          | Deref Expr

type Name = String

data Stmt = CompoundStmt [Stmt]
          | Expr Expr
          | IfElse Expr Stmt Stmt
          | DeclareAssign Name Expr
          | Declare Name
          | Upd Expr Expr -- *e = e;
          -- | While Expr [Stmt]
          -- | For Stmt Expr Stmt [Stmt]
          | Return Expr