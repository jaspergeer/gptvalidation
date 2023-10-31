-- Sanity checks

module Simple where

import qualified AST
import Data.SBV (runSMT, (.<=>))
import PassManager (sbvOfNormalized)
import SymbolicExecution (execute)
import Test.HUnit (Assertion, Test (TestCase, TestLabel, TestList))
import TestUtils
import Types
import NormalizedAST

tests :: Test
tests =
  TestLabel
    "simple"
    ( TestList $
        map
          TestCase
          [ test1
          , test2
          , test3
          , test4
          , test5
          , test6
          , test7
          , test8
          {-, test9 -}
          ]
    )

-- Test 1
-- Trivially equivalent programs

mulx2 :: Function
mulx2 =
  Function
    Int32
    "f"
    [("x", int32)]
    ( CompoundStmt
        [Return (BinExpr (Var "x") AST.Mul (Int 2))]
    )

addxx :: Function
addxx =
  Function
    Int32
    "f"
    [("x", int32)]
    ( CompoundStmt
        [Return (BinExpr (Var "x") AST.Add (Var "x"))]
    )

test1 :: Assertion
test1 = do
  sbv1 <- runSMT (sbvOfNormalized mulx2)
  sbv2 <- runSMT (sbvOfNormalized addxx)
  assertProvable (sbv1 .<=> sbv2)

-- Test 2
-- Trivially non-equivalent programs

subxy :: Function
subxy =
  Function
    Int32
    "f"
    [("x", int32), ("y", int32)]
    ( CompoundStmt
        [Return (BinExpr (Var "x") AST.Sub (Var "y"))]
    )

subyx :: Function
subyx =
  Function
    Int32
    "f"
    [("x", int32), ("y", int32)]
    ( CompoundStmt
        [Return (BinExpr (Var "y") AST.Sub (Var "x"))]
    )

test2 :: Assertion
test2 = do
  sbv1 <- runSMT (sbvOfNormalized subxy)
  sbv2 <- runSMT (sbvOfNormalized subyx)
  assertNotProvable (sbv1 .<=> sbv2)

-- Test 3
-- De Morgan's laws

bool1 :: Function
bool1 =
  Function
    Int8
    "f"
    [("a", int8), ("b", int8)]
    (Return (UnExpr AST.LNot (LogExpr (Var "a") AST.LAnd (Var "b"))))

bool2 :: Function
bool2 =
  Function
    Int8
    "f"
    [("a", int8), ("b", int8)]
    (Return (LogExpr (UnExpr AST.LNot (Var "a")) AST.LOr (UnExpr AST.LNot (Var "b"))))

test3 :: Assertion
test3 = do
  sbv1 <- runSMT (sbvOfNormalized bool1)
  sbv2 <- runSMT (sbvOfNormalized bool2)
  assertProvable (sbv1 .<=> sbv2)

-- Test 4
-- Array indexing (heap)

arr1 :: Function
arr1 =
  Function
    Int32
    "f"
    [("i", int32)]
    ( CompoundStmt
        [ Declare (Ptr (Integral Int32)) "arr"
        , Expr (Assign (Var "arr") (Alloc Int32 1 "arr_obj"))
        , Expr (Assign (Var "i") (BinExpr (Var "i") AST.Add (Var "i")))
        , Expr (Assign (Index "arr" [Var "i"]) (Int 1))
        , Return (Index "arr" [Var "i"])
        ]
    )

arr2 :: Function
arr2 =
  Function
    Int32
    "f"
    [("i", int32)]
    ( CompoundStmt
        [ DeclareHeapObj (Arr Int32 1) "arr" "arr_obj"
        , Expr (Assign (Var "i") (BinExpr (Var "i") AST.Mul (Int 2)))
        , Expr (Assign (Index "arr" [Var "i"]) (Int 1))
        , Return (Index "arr" [Var "i"])
        ]
    )

test4 :: Assertion
test4 = do
  sbv1 <- runSMT (sbvOfNormalized arr1)
  sbv2 <- runSMT (sbvOfNormalized arr2)
  assertProvable (sbv1 .<=> sbv2)

-- Test 5
-- Array indexing (stack)

arr3 :: Function
arr3 =
  Function
    Int32
    "f"
    [("i", int32)]
    ( CompoundStmt
        [ DeclareStackObj (Arr Int32 1) "arr"
        , Expr (Assign (Var "i") (BinExpr (Var "i") AST.Add (Var "i")))
        , Expr (Assign (Index "arr" [Var "i"]) (Int 1))
        , Return (Index "arr" [Var "i"])
        ]
    )

arr4 :: Function
arr4 =
  Function
    Int32
    "f"
    [("i", int32)]
    ( CompoundStmt
        [ DeclareStackObj (Arr Int32 1) "arr"
        , Expr (Assign (Var "i") (BinExpr (Var "i") AST.Mul (Int 2)))
        , Expr (Assign (Index "arr" [Var "i"]) (Int 1))
        , Return (Index "arr" [Var "i"])
        ]
    )

test5 :: Assertion
test5 = do
  sbv1 <- runSMT (sbvOfNormalized arr3)
  sbv2 <- runSMT (sbvOfNormalized arr4)
  assertProvable (sbv1 .<=> sbv2)

-- Test 6
-- Control flow, equivalent

ifdemo1 :: Function
ifdemo1 =
  Function
    Int32
    "f"
    [("x", int32)]
    ( CompoundStmt
        [ IfElse
            (RelExpr (Var "x") AST.Eq (Int 0))
            (Return (Int 0))
            (Return (BinExpr (Var "x") AST.Mul (Var "x")))
        ]
    )

square :: Function
square =
  Function
    Int32
    "f"
    [("x", int32)]
    (Return (BinExpr (Var "x") AST.Mul (Var "x")))

test6 :: Assertion
test6 = do
  sbv1 <- runSMT (sbvOfNormalized ifdemo1)
  sbv2 <- runSMT (sbvOfNormalized square)
  assertProvable (sbv1 .<=> sbv2)

-- Test 7
-- Control flow, not equivalent

ifdemo2 :: Function
ifdemo2 =
  Function
    Int32
    "f"
    [("x", int32)]
    ( CompoundStmt
        [ IfElse
            (RelExpr (Var "x") AST.Eq (Int 0))
            (Return (Int 1))
            (Return (BinExpr (Var "x") AST.Mul (Var "x")))
        ]
    )

test7 :: Assertion
test7 = do
  sbv1 <- runSMT (sbvOfNormalized ifdemo1)
  sbv2 <- runSMT (sbvOfNormalized ifdemo2)
  assertNotProvable (sbv1 .<=> sbv2)

-- Test 8
-- Simple "improvement" suggested by ChatGPT

func1 :: Function
func1 =
  Function
    Int32
    "f"
    [("a", int32), ("b", int32)]
    ( CompoundStmt
        [ DeclareStack Int32 "sum"
        , Expr (Assign (Var "sum") (BinExpr (Var "a") AST.Add (Var "b")))
        , IfElse
            (RelExpr (Var "sum") AST.Eq (Int 6))
            (Return (Int 3))
            (Return (BinExpr (Var "sum") AST.Div (Int 2)))
        ]
    )

func2 :: Function
func2 =
  Function
    Int32
    "f"
    [("a", int32), ("b", int32)]
    ( CompoundStmt
        [ DeclareStack Int32 "sum"
        , Expr (Assign (Var "sum") (BinExpr (Var "a") AST.Add (Var "b")))
        , IfElse
            (RelExpr (Var "sum") AST.Eq (Int 6))
            (Return (Int 3))
            (Return (ShiftExpr (Var "sum") AST.Shr (Int 1)))
        ]
    )

test8 :: Assertion
test8 = do
  sbv1 <- runSMT (sbvOfNormalized func1)
  sbv2 <- runSMT (sbvOfNormalized func2)
  assertProvable (sbv1 .<=> sbv2)

-- Test 9
-- pointer to array as argument
-- DOES NOT WORK (SBV issue?)

arrptrparam1 :: Function
arrptrparam1 =
  Function
    Int32
    "f"
    [("i", int32), ("arr", arr Int32 1)]
    ( CompoundStmt
        [ Expr (Assign (Var "i") (BinExpr (Var "i") AST.Add (Var "i")))
        , Expr (Assign (Index "arr" [Var "i"]) (Int 1))
        , Return (Index "arr" [Var "i"])
        ]
    )

arrptrparam2 :: Function
arrptrparam2 =
  Function
    Int32
    "f"
    [("i", int32), ("arr", arr Int32 1)]
    ( CompoundStmt
        [ Expr (Assign (Var "i") (BinExpr (Var "i") AST.Mul (Int 3)))
        , Expr (Assign (Index "arr" [Var "i"]) (Int 1))
        , Return (Index "arr" [Var "i"])
        ]
    )

test9 :: Assertion
test9 = do
  print (execute arrptrparam1)
  sbv1 <- runSMT (sbvOfNormalized arrptrparam1)
  sbv2 <- runSMT (sbvOfNormalized arrptrparam2)
  assertProvable (sbv1 .<=> sbv2)
