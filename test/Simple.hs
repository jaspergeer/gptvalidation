module Simple where

import qualified AST
import qualified SymbolicExpression as X
import UnambiguousAST
    ( Stmt(Return, CompoundStmt), Expr(Var, ArithExpr), Function(..) )
import SymbolicExecution ( function, initState )
import Data.SBV ( Symbolic, EqSymbolic((.==)), SInt32, runSMT )
import TestUtils (assertProvable, assertNotProvable)
import SBVConvert (convertExp)
import Test.HUnit (Assertion, Test (TestList, TestCase))

tests :: Test
tests = TestList [TestCase test1, TestCase test2]

-- Test 1
-- Trivially equivalent programs

addxy :: Function
addxy =
  Function X.Int32 "f" [("x", X.Int32), ("y", X.Int32)]
    (CompoundStmt
      [ Return (ArithExpr (Var "x") AST.Add (Var "y")) ])
      
addyx :: Function
addyx =
  Function X.Int32 "f" [("x", X.Int32), ("y", X.Int32)]
    (CompoundStmt
      [ Return (ArithExpr (Var "y") AST.Add (Var "x")) ])

test1 :: Assertion
test1 = let
  [(_, s1)] = function (initState, addxy)
  [(_, s2)] = function (initState, addyx)
  in do
    sbv1 <- runSMT (convertExp s1 :: Symbolic SInt32)
    sbv2 <- runSMT (convertExp s2 :: Symbolic SInt32)
    assertProvable (sbv1 .== sbv2)

-- Test 2
-- Trivially non-equivalent programs

subxy :: Function
subxy =
  Function X.Int32 "f" [("x", X.Int32), ("y", X.Int32)]
    (CompoundStmt
      [ Return (ArithExpr (Var "x") AST.Sub (Var "y")) ])
      
subyx :: Function
subyx =
  Function X.Int32 "f" [("x", X.Int32), ("y", X.Int32)]
    (CompoundStmt
      [ Return (ArithExpr (Var "y") AST.Sub (Var "x")) ])

test2 :: Assertion
test2 = let
  [(_, s1)] = function (initState, subxy)
  [(_, s2)] = function (initState, subyx)
  in do
    sbv1 <- runSMT (convertExp s1 :: Symbolic SInt32)
    sbv2 <- runSMT (convertExp s2 :: Symbolic SInt32)
    assertNotProvable (sbv1 .== sbv2)

