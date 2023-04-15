module SymbolicExecution where

import qualified SymbolicExpression as SX
import qualified Env as E
import qualified UnambiguousAST as U

-- symbolic state: (g, rho, mu)

type SymbolicState = (SX.Expr, E.Env SX.Expr, E.Env SX.Expr)

type SymbolicExecutor a = SymbolicState -> a -> [SymbolicState]

statement :: SymbolicExecutor U.Stmt
statement s (U.CompoundStmt cs) = undefined