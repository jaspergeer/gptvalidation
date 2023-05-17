module AST where

data BinOp = Add | Sub | Mul | Mod | Div | BAnd | BOr deriving Show -- arithmetic/bitwise
data LogOp = LAnd | LOr deriving Show -- boolean
data RelOp = Eq | Leq | Geq | Lt | Gt deriving Show -- comparison
data ShiftOp = Shl | Shr deriving Show

data Unop = Neg | LNot | BNot deriving Show

data Expr = BinExpr Expr BinOp Expr
          | LogExpr Expr LogOp Expr
          | ShiftOp Expr ShiftOp Expr
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