module SymbolicExpression where

import Data.Int
import Data.Word

type Name = String

data Expr = BinExpr Expr Binop Expr
          | Unop Expr
          | Sel Expr [Expr]
          | FunCall Name [Expr]
          | Choice Expr Expr Expr
          | I32 Int32
          | I8 Int8
          | NewArr [Int] Type


data Binop = Add | Sub | Mul | Mod | Div -- arithmetic
           | LAnd | LOr -- boolean
           | BAnd | BOr | Shl | Shr -- binary
           | Eq | Leq | Geq | Lt | Gt -- comparison

data Unop = Neg | LNot | BNot | Deref

data Type = Ptr Type | Int32 | Int8