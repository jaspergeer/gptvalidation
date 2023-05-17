{-# LANGUAGE FlexibleContexts #-}

module TestUtils where
import Data.SBV
import Test.HUnit (assertFailure, Assertion)

assertProvable :: (Provable a) => a -> Assertion
assertProvable x =
  do
    result <- prove x
    case result of
      ThmResult (Unsatisfiable {}) -> do
        print result
        return ()
      _ -> do
        assertFailure (show result)

assertNotProvable :: (Provable a) => a -> Assertion
assertNotProvable x =
  do
    result <- prove x
    case result of
      ThmResult (Unsatisfiable {}) -> do
        assertFailure (show result)
      _ -> do
        print result
        return ()