module SymbolicExecution where

import Prelude hiding (sequence, exp)

import qualified SymbolicExpression as SX
import qualified Env as E
import qualified UnambiguousAST as U
import qualified List as L

-- append
(@) :: L.List a -> L.List a -> L.List a
a @ b = a L.@ b

-- cons
(&) :: a -> L.List a -> L.List a
a & b = a L.& b

zero :: SX.Expr
zero = SX.I32 0

void :: a -> L.List (a, SX.Expr)
void s = L.singleton (s, zero)

type VarEnv = E.Env (SX.Expr, SX.Type)

-- symbolic state: (g, rho, mu)
type SymbolicState = (SX.Expr, VarEnv, VarEnv)

data Branching a b = Branching { runBranching :: a -> L.List b}

-- instance Applicative (Branching a) where
--   f <*> a = Branching ()
--   pure x = Branching (\_ -> L.singleton x)

-- instance Monad (Branching a) where
--   (>>=) :: Branching a1 a2 -> (a2 -> Branching a1 b) -> Branching a1 b
--   a >>= f = Branching (\x -> 
--     let 
--       as = runBranching a x
--       cs = foldr (\a cs -> f a @ cs) L.empty as
--     in undefined)

-- symbolic execution with branches
type SymbolicExecutor a = (SymbolicState, a) -> L.List (SymbolicState, SX.Expr)

execmany :: SymbolicExecutor a -> L.List (SymbolicState, a) -> L.List (SymbolicState, SX.Expr)
execmany exec = foldr (\x branches -> exec x @ branches) L.empty

sequence :: SymbolicExecutor a -> SymbolicExecutor [a]
sequence exec (s1, as) =
  let
    start = void s1
    makeNewStarts branches a = foldr (\(state, _) starts -> (state, a) & starts) L.empty branches
  in foldl (\branches a -> execmany exec (makeNewStarts branches a)) start as

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