module AST where

data ArithOp = Add | Sub | Mul | Mod | Div deriving Show -- arithmetic
data LogOp = LAnd | LOr deriving Show -- boolean
data BitOp = BAnd | BOr | Shl | Shr deriving Show -- bitwise
data RelOp = Eq | Leq | Geq | Lt | Gt deriving Show -- comparison

data Unop = Neg | LNot | BNot deriving Show

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
          deriving Show

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
          deriving Show