module Main where

import qualified Test.HUnit as Unit
import qualified System.Exit as Exit

main :: IO ()
main = do
    result <- Unit.runTestTT tests
    if Unit.failures result > 0 then Exit.exitFailure else Exit.exitSuccess

tests :: Unit.Test
tests = undefined