module GPT where

import qualified AST
import qualified SymbolicExpression as X
import UnambiguousAST
import Data.SBV ( (.<=>), runSMT )
import TestUtils (assertProvable, assertNotProvable)
import Test.HUnit (Assertion, Test (TestList, TestCase, TestLabel))
import PassManager (sbvOfFunction)
import qualified Types as T

calculateResult1 =
  Function 