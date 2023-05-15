module AST where

data ArithOp = Add | Sub | Mul | Mod | Div -- arithmetic
data LogOp = LAnd | LOr -- boolean
data BitOp = BAnd | BOr | Shl | Shr -- bitwise
data RelOp = Eq | Leq | Geq | Lt | Gt -- comparison

data Unop = Neg | LNot | BNot

data Expr = ArithExpr Expr ArithOp Expr
          | LogExpr Expr LogOp Expr
          | BitExpr Expr BitOp Expr
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