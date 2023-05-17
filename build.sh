#!/bin/bash

echo "Building test suite..."
cabal clean > /dev/null
cabal run tinyvalidator-testsuite 2> testsuite.out 1> /dev/null
echo "Running tests..."
TEST_SUCCESS=`grep -q "Errors: 0  Failures: 0" testsuite.out`
rm testsuite.out
if ! $TEST_SUCCESS; then
  echo "Failure"
  exit 1
fi
echo "Success"
cabal build tinyvalidator