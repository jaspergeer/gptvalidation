module Simple where

import qualified AST
import qualified SymbolicExpression as X
import UnambiguousAST
    ( Stmt(Return, CompoundStmt), Expr(Var, ArithExpr), Function(..) )
import Data.SBV ( (.<=>), runSMT )
import TestUtils (assertProvable, assertNotProvable)
import Test.HUnit (Assertion, Test (TestList, TestCase))
import PassManager (sbvOfFunction)

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
test1 = do
    sbv1 <- runSMT (sbvOfFunction addxy)
    sbv2 <- runSMT (sbvOfFunction addyx)
    assertProvable (sbv1 .<=> sbv2)

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
test2 = do
    sbv1 <- runSMT (sbvOfFunction subxy)
    sbv2 <- runSMT (sbvOfFunction subyx)
    assertNotProvable (sbv1 .<=> sbv2)
