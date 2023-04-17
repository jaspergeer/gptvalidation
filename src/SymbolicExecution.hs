module SymbolicExecution where

import Prelude hiding (sequence, exp)

import qualified SymbolicExpression as SX
import qualified Env as E
import qualified UnambiguousAST as U

zero :: SX.Expr
zero = SX.I32 0

void :: a -> [(a, SX.Expr)]
void s = [(s, zero)]

type VarEnv = E.Env (SX.Expr, SX.Type)

-- symbolic state: (g, rho, mu)
type SymbolicState = (SX.Expr, VarEnv, VarEnv)

-- symbolic execution with branches
type SymbolicExecutor a = (SymbolicState, a) -> [(SymbolicState, SX.Expr)]

sequence :: SymbolicExecutor a -> SymbolicExecutor [a]
sequence exec (s1, as) =
  let
    start = void s1
    makeNewStarts branches a = foldr (\(state, _) starts -> (state, a) : starts) [] branches
  in foldl (\branches a -> makeNewStarts branches a >>= exec) start as

stmt :: SymbolicExecutor U.Stmt
stmt (state, c) = case c of
  U.CompoundStmt cs -> sequence stmt (state, cs)
  U.Expr e -> fmap (\(s', _) -> (s', zero)) (exp (state, e))
  _ -> error "TODO"

exp :: SymbolicExecutor U.Expr
exp (state, e) = case e of
  U.BinExpr e1 op e2 -> do
    (state1 , s1) <- exp (state, e1)
    (state2, s2) <- exp (state1, e2)
    return (state2, SX.BinExpr s1 op s2)
  _ -> error "TODO"