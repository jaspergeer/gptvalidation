-- Sanity checks

module Simple where

import qualified AST
import qualified SymbolicExpression as X
import UnambiguousAST
import Data.SBV ( (.<=>), runSMT )
import TestUtils (assertProvable, assertNotProvable)
import Test.HUnit (Assertion, Test (TestList, TestCase, TestLabel))
import PassManager (sbvOfFunction)
import qualified Types as T

tests :: Test
tests = TestLabel "simple"
          (TestList $ map TestCase  [ test1
                                    , test2
                                    , test3
                                    , test4
                                    , test5
                                    , test6
                                    , test7
                                    , test8 ])

-- Test 1
-- Trivially equivalent programs

mulx2 :: Function
mulx2 =
  Function T.Int32 "f" [("x", T.Int32)]
    (CompoundStmt
      [ Return (BinExpr (Var "x") AST.Mul (Int 2)) ])
      
addxx :: Function
addxx =
  Function T.Int32 "f" [("x", T.Int32)]
    (CompoundStmt
      [ Return (BinExpr (Var "x") AST.Add (Var "x")) ])

test1 :: Assertion
test1 = do
    sbv1 <- runSMT (sbvOfFunction mulx2)
    sbv2 <- runSMT (sbvOfFunction addxx)
    assertProvable (sbv1 .<=> sbv2)

-- Test 2
-- Trivially non-equivalent programs

subxy :: Function
subxy =
  Function T.Int32 "f" [("x", T.Int32), ("y", T.Int32)]
    (CompoundStmt
      [ Return (BinExpr (Var "x") AST.Sub (Var "y")) ])
      
subyx :: Function
subyx =
  Function T.Int32 "f" [("x", T.Int32), ("y", T.Int32)]
    (CompoundStmt
      [ Return (BinExpr (Var "y") AST.Sub (Var "x")) ])

test2 :: Assertion
test2 = do
    sbv1 <- runSMT (sbvOfFunction subxy)
    sbv2 <- runSMT (sbvOfFunction subyx)
    assertNotProvable (sbv1 .<=> sbv2)

-- Test 3
-- De Morgan's laws

bool1 :: Function
bool1 =
  Function T.Int8 "f" [("a", T.Int8), ("b", T.Int8)]
    (Return (UnExpr AST.LNot (LogExpr (Var "a") AST.LAnd (Var "b"))))

bool2 :: Function
bool2 =
  Function T.Int8 "f" [("a", T.Int8), ("b", T.Int8)]
    (Return (LogExpr (UnExpr AST.LNot (Var "a")) AST.LOr (UnExpr AST.LNot (Var "b")) ))

test3 :: Assertion
test3 = do
    sbv1 <- runSMT (sbvOfFunction bool1)
    sbv2 <- runSMT (sbvOfFunction bool2)
    assertProvable (sbv1 .<=> sbv2)

-- Test 4
-- Array indexing (heap)

arr1 :: Function
arr1 =
  Function T.Int32 "f" [("i", T.Int32)]
    (CompoundStmt
      [ DeclareHeapObj (T.Arr 1 T.Int32) "arr" "arr_obj"
      , Expr (Assign (Var "i") (BinExpr (Var "i") AST.Add (Var "i")))
      , Expr (Assign (Index "arr" [Var "i"]) (Int 1))
      , Return (Index "arr" [Var "i"]) ])

arr2 :: Function
arr2 =
  Function T.Int32 "f" [("i", T.Int32)]
    (CompoundStmt
      [ DeclareHeapObj (T.Arr 1 T.Int32) "arr" "arr_obj"
      , Expr (Assign (Var "i") (BinExpr (Var "i") AST.Mul (Int 2)))
      , Expr (Assign (Index "arr" [Var "i"]) (Int 1))
      , Return (Index "arr" [Var "i"]) ])

test4 :: Assertion
test4 = do
    sbv1 <- runSMT (sbvOfFunction arr1)
    sbv2 <- runSMT (sbvOfFunction arr2)
    assertProvable (sbv1 .<=> sbv2)

-- Test 5
-- Array indexing (stack)

arr3 :: Function
arr3 =
  Function T.Int32 "f" [("i", T.Int32)]
    (CompoundStmt
      [ DeclareStackObj (T.Arr 1 T.Int32) "arr" "arr_obj"
      , Expr (Assign (Var "i") (BinExpr (Var "i") AST.Add (Var "i")))
      , Expr (Assign (Index "arr" [Var "i"]) (Int 1))
      , Return (Index "arr" [Var "i"]) ])

arr4 :: Function
arr4 =
  Function T.Int32 "f" [("i", T.Int32)]
    (CompoundStmt
      [ DeclareStackObj (T.Arr 1 T.Int32) "arr" "arr_obj"
      , Expr (Assign (Var "i") (BinExpr (Var "i") AST.Mul (Int 2)))
      , Expr (Assign (Index "arr" [Var "i"]) (Int 1))
      , Return (Index "arr" [Var "i"]) ])

test5 :: Assertion
test5 = do
    sbv1 <- runSMT (sbvOfFunction arr3)
    sbv2 <- runSMT (sbvOfFunction arr4)
    assertProvable (sbv1 .<=> sbv2)

-- Test 6
-- Control flow, equivalent

ifdemo1 :: Function
ifdemo1 =
  Function T.Int32 "f" [("x", T.Int32)]
    (CompoundStmt
      [ IfElse (RelExpr (Var "x") AST.Eq (Int 0))
          (Return (Int 0))
          (Return (BinExpr (Var "x") AST.Mul (Var "x")))
      ])

square :: Function
square =
  Function T.Int32 "f" [("x", T.Int32)]
    (Return (BinExpr (Var "x") AST.Mul (Var "x")))

test6 :: Assertion
test6 = do
    sbv1 <- runSMT (sbvOfFunction ifdemo1)
    sbv2 <- runSMT (sbvOfFunction square)
    assertProvable (sbv1 .<=> sbv2)

-- Test 6
-- Control flow, not equivalent

ifdemo2 :: Function
ifdemo2 =
  Function T.Int32 "f" [("x", T.Int32)]
    (CompoundStmt
      [ IfElse (RelExpr (Var "x") AST.Eq (Int 0))
          (Return (Int 1))
          (Return (BinExpr (Var "x") AST.Mul (Var "x")))
      ])

test7 :: Assertion
test7 = do
    sbv1 <- runSMT (sbvOfFunction ifdemo1)
    sbv2 <- runSMT (sbvOfFunction ifdemo2)
    assertNotProvable (sbv1 .<=> sbv2)

-- Test 7
-- Simple "improvement" suggested by ChatGPT

func1 :: Function
func1 =
  Function T.Int32 "f" [("a", T.Int32), ("b", T.Int32)]
    (CompoundStmt
      [ DeclareStack T.Int32 "sum"
      , Expr (Assign (Var "sum") (BinExpr (Var "a") AST.Add (Var "b")))
      , IfElse (RelExpr (Var "sum") AST.Eq (Int 6))
          (Return (Int 3))
          (Return (BinExpr (Var "sum") AST.Div (Int 2)))
      ])

func2 :: Function
func2 =
  Function T.Int32 "f" [("a", T.Int32), ("b", T.Int32)]
    (CompoundStmt
      [ DeclareStack T.Int32 "sum"
      , Expr (Assign (Var "sum") (BinExpr (Var "a") AST.Add (Var "b")))
      , IfElse (RelExpr (Var "sum") AST.Eq (Int 6))
          (Return (Int 3))
          (Return (ShiftExpr (Var "sum") AST.Shr (Int 1)))
      ])

test8 :: Assertion
test8 = do
    sbv1 <- runSMT (sbvOfFunction func1)
    sbv2 <- runSMT (sbvOfFunction func2)
    assertProvable (sbv1 .<=> sbv2)