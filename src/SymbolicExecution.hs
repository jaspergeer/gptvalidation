module SymbolicExecution where

import Prelude hiding (sequence)

import qualified SymbolicExpression as SX
import qualified Env as E
import qualified UnambiguousAST as U
import qualified HughesList as H

-- append
(@) :: H.List a -> H.List a -> H.List a
a @ b = a H.@ b

-- cons
(&) :: a -> H.List a -> H.List a
a & b = a H.& b

zero :: SX.Expr
zero = SX.I32 0

void :: a -> H.List (a, SX.Expr)
void s = H.singleton (s, zero)

-- symbolic state: (g, rho, mu)
type SymbolicState = (SX.Expr, E.Env SX.Expr, E.Env SX.Expr)

-- symbolic execution with branches
type SymbolicExecutor a = (SymbolicState, a) -> H.List (SymbolicState, SX.Expr)

execmany :: SymbolicExecutor a -> H.List (SymbolicState, a) -> H.List (SymbolicState, SX.Expr)
execmany exec = foldr (\x branches -> exec x @ branches) H.empty

sequence :: SymbolicExecutor a -> SymbolicExecutor [a]
sequence exec (s1, as) =
  let
    start = void s1
    makeNewStarts branches a = foldr (\(state, _) starts -> (state, a) & starts) H.empty branches
  in foldl (\branches a -> execmany exec (makeNewStarts branches a)) start as

statement :: SymbolicExecutor U.Stmt
statement (s, c) = case c of
  U.CompoundStmt cs -> sequence statement (s, cs)
  U.Expr e -> fmap (\(s', _) -> (s', zero)) (expression (s, e))
  _ -> error "TODO"

expression :: SymbolicExecutor U.Expr
expression = undefined