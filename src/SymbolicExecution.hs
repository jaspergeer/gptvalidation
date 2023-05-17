{-# LANGUAGE InstanceSigs #-}
module SymbolicExecution where

import Prelude hiding (sequence, exp)

import qualified SymbolicExpression as X
import qualified Env as E
import qualified UnambiguousAST as U

import qualified AST
import Control.Exception (throw, Exception)
import Data.Foldable (foldlM)

-- this whole module can probably be reimplemented with Reader or State
-- but I wanted to keep it pure

execute :: U.Function -> [(SymbolicState, X.Expr)]
execute f = function (initState, f)

zero :: X.Expr
zero = X.Literal 0

void :: a -> [(a, X.Expr)]
void s = [(s, zero)]

type Name = String

type VarEnv = E.Env (X.Expr, X.Type)

data SymExException = NotBound Name Name
                    | NotPointer Name
                    | AlreadyBound Name Name
                    | UnknownAlias
                    | Unsupported String

instance Show SymExException where
  show :: SymExException -> String
  show ex = "Symbolic Executor: " ++ case ex of
    NotBound x env -> "name " ++ x ++ " not bound in environment " ++ env
    NotPointer x -> "name " ++ x ++ " is not bound to a pointer"
    AlreadyBound x env -> "name " ++ x ++ " already bound in environemnt " ++ env
    UnknownAlias -> "cannot determine the memory location pointed to by expression"
    Unsupported feature -> feature ++ " not supported"

instance Exception SymExException

-- helper functions for interacting with environments

typeIn :: (SymbolicState -> VarEnv) -> Name -> (Name -> SymbolicState -> X.Type)
typeIn getenv envname x state = case E.find x (getenv state) of
  Just (_, tau) -> tau
  _ -> throw (NotBound x envname)

typeInRho :: E.Name -> SymbolicState -> X.Type
typeInRho = typeIn rho "rho"

typeInMu :: E.Name -> SymbolicState -> X.Type
typeInMu = typeIn mu "mu"

valIn :: (SymbolicState -> VarEnv) -> Name -> (Name -> SymbolicState -> X.Expr)
valIn getenv envname x state = case E.find x (getenv state) of
  Just (s, _) -> s
  _ -> throw (NotBound x envname)

valInRho :: Name -> SymbolicState -> X.Expr
valInRho = valIn rho "rho"

valInMu :: Name -> SymbolicState -> X.Expr
valInMu = valIn mu "mu"

bindIn :: (SymbolicState -> VarEnv) -> Name -> (Name -> X.Expr -> SymbolicState -> VarEnv)
bindIn getenv envname x e state =
  let env' = getenv state
      tau = typeIn getenv envname x state in
      E.bind x (e, tau) env'

bindInRho :: Name -> X.Expr -> SymbolicState -> E.Env (X.Expr, X.Type)
bindInRho = bindIn rho "rho"

bindInMu :: Name -> X.Expr -> SymbolicState -> E.Env (X.Expr, X.Type)
bindInMu = bindIn mu "mu"

bindNew :: (SymbolicState -> VarEnv) -> Name -> (Name -> (X.Expr, X.Type) -> SymbolicState -> E.Env (X.Expr, X.Type))
bindNew getenv envname x y state =
  let env' = getenv state in
    if env' `E.binds` x then throw (AlreadyBound x envname)
    else E.bind x y env'

bindNewRho :: Name -> (X.Expr, X.Type) -> SymbolicState -> E.Env (X.Expr, X.Type)
bindNewRho = bindNew rho "rho"

bindNewMu :: Name -> (X.Expr, X.Type) -> SymbolicState -> E.Env (X.Expr, X.Type)
bindNewMu = bindNew mu "mu"

-- symbolic state: {g, rho, mu}
data SymbolicState = SymbolicState { g :: [X.Expr], rho :: VarEnv, mu :: VarEnv }

initState :: SymbolicState
initState = SymbolicState { g = [], rho = E.empty, mu = E.empty }

-- symbolic execution with branches
type SymbolicExecutor a = (SymbolicState, a) -> [(SymbolicState, X.Expr)]

sequence :: SymbolicExecutor a -> SymbolicExecutor [a]
sequence exec (s1, as) =
  let
    start = void s1
    makeNewStarts branches a = foldr (\(state, _) starts -> (state, a) : starts) [] branches
  in foldl (\branches a -> makeNewStarts branches a >>= exec) start as

function :: SymbolicExecutor U.Function
function (state, U.Function returnty _ params body) =
  let
    state' = foldr (\(n, t) state_i -> state_i {rho = bindNewRho n (X.Free n, X.Integral t) state_i}) state params
  in
    do
      (state'', s) <- stmt (state', body)
      return (state'', X.FromType returnty s)

stmt :: SymbolicExecutor U.Stmt
stmt (state, c) = case c of
  U.CompoundStmt cs -> sequence stmt (state, cs)
  U.Expr e -> (\(s', _) -> (s', zero)) <$> exp (state, e)
  U.IfElse e c1 c2 -> do
    (state', g1) <- exp (state, e)
    let state_1 = state' { g = g1 : g state' }
    let notg1 = X.UnExpr AST.LNot g1
    let state_2 = state' { g = notg1 : g state' }
    r1 <- stmt (state_1, c1)
    r2 <- stmt (state_2, c2)
    [r1, r2]
  U.DeclareStack b x ->
    return (state { rho = bindNewRho x (zero, X.Integral b) state}, zero)
  U.DeclareStackObj t x a ->
    let
      tau = X.Complex t
      state' = state { rho = bindNewRho a (X.NewArr (X.dim tau) (X.base tau), tau) state }
      state'' = state' { rho = bindNewRho x (X.PtrTo a, X.Integral (X.Ptr tau)) state}
    in return (state'', zero)
  U.DeclareHeapObj t x a ->
    let
      tau = X.Complex t
      state' = state { mu = bindNewMu a (X.NewArr (X.dim tau) (X.base tau), tau) state }
      state'' = state' { rho = bindNewRho x (X.PtrTo a, X.Integral (X.Ptr tau)) state}
    in return (state'', zero)
  U.Return e -> exp (state, e)
  _ -> throw (Unsupported "loops")

toOffset :: X.Expr -> X.Expr
toOffset s = case s of
  X.PtrTo _ -> X.ArithExpr s AST.Add zero
  X.ArithExpr s1 AST.Add s2 -> case
    (toOffset s1, toOffset s2) of
      (X.ArithExpr s3@(X.PtrTo _) AST.Add s4, s2') -> X.ArithExpr s3 AST.Add (X.ArithExpr s4 AST.Add s2')
      (s1', X.ArithExpr s3@(X.PtrTo _) AST.Add s4) -> X.ArithExpr s3 AST.Add (X.ArithExpr s1' AST.Add s4)
      _ -> s
  X.ArithExpr s1 AST.Sub s2 -> case
    toOffset s1 of
      X.ArithExpr s3@(X.PtrTo _) AST.Add s4 -> X.ArithExpr s3 AST.Add (X.ArithExpr s4 AST.Sub s2)
      _ -> s
  _ -> s

many :: SymbolicState -> SymbolicExecutor U.Expr -> [U.Expr] -> [(SymbolicState, [X.Expr])]
many state exec = foldlM (\(state_i, is) e' -> do
        (state_i1, i) <- exec (state_i, e')
        return (state_i1, i:is)) (state, [])

updAtAddr :: Name -> [X.Expr] -> X.Expr -> SymbolicState -> SymbolicState
updAtAddr addr indices newval state =
  if rho state `E.binds` addr then
    let
      s = valInRho addr state
    in state { rho = bindInRho addr (X.Upd s indices newval) state }
  else
    let
      s = valInMu addr state
    in state { mu = bindInMu addr (X.Upd s indices newval) state }

exp :: SymbolicExecutor U.Expr
exp (state, e) = case e of
  U.ArithExpr e_1 op e_2 -> do
    (state_1 , s_1) <- exp (state, e_1)
    (state_2, s_2) <- exp (state_1, e_2)
    return (state_2, X.ArithExpr s_1 op s_2)
  U.LogExpr e_1 op e_2 -> do
    (state_1 , s_1) <- exp (state, e_1)
    (state_2, s_2) <- exp (state_1, e_2)
    return (state_2, X.LogExpr s_1 op s_2)
  U.BitExpr e_1 op e_2 -> do
    (state_1 , s_1) <- exp (state, e_1)
    (state_2, s_2) <- exp (state_1, e_2)
    return (state_2, X.BitExpr s_1 op s_2)
  U.RelExpr e_1 op e_2 -> do
    (state_1 , s_1) <- exp (state, e_1)
    (state_2, s_2) <- exp (state_1, e_2)
    return (state_2, X.RelExpr s_1 op s_2)
  U.UnExpr op e_1 -> do
    (state', s) <- exp (state, e_1)
    return (state', X.UnExpr op s)
  U.Assign (U.Var x) e_1 | rho state `E.binds` x -> do
    let tau = typeInRho x state
    (state', s) <- exp (state, e_1)
    return (state' { rho = E.bind x (s, tau) (rho state') }, s)
  U.Assign (U.Index x es) e_v -> do
    (state_n1, is) <- many state exp es
    (state', s') <- exp (state_n1, e_v)
    case valInRho x state_n1 of
      X.PtrTo a -> return (updAtAddr a is s' state', s')
      _ -> throw (NotPointer x)
  U.Assign (U.Deref e_1) e_2 -> do
    (state_1, s_1) <- exp (state, e_1)
    (state_2, s_2) <- exp (state_1, e_2)
    case toOffset s_1 of
      X.ArithExpr (X.PtrTo a) AST.Add i ->
        return (updAtAddr a [i] s_2 state_2, s_2)
      _ -> throw UnknownAlias
  U.Assign {} -> throw (Unsupported "assignment with complex left-hand side")
  U.Deref e_1 -> do
    (state', s) <- exp (state, e_1)
    case toOffset s of
      X.ArithExpr (X.PtrTo a) AST.Add s_1 -> do
        let (getenv, envname) = if rho state' `E.binds` a then (rho, "rho") else (mu, "mu")
        let t = X.base (typeIn getenv envname a state')
        return (state', X.FromType t (X.Sel (valIn getenv envname a state') [s_1]))
      _ -> throw UnknownAlias
  U.Var x -> case typeInRho x state of
      X.Integral t -> return (state, X.FromType t (valInRho x state))
      _ -> error "IMPOSSIBLE: stack object referenced by name"
  U.Index x es -> do
    (state_n1, is) <- many state exp es
    let (getenv, envname) = if rho state `E.binds` x then (rho, "rho") else (mu, "mu")
    let t = X.base (typeIn getenv envname x state_n1)
    return (state, X.FromType t (X.Sel (valIn getenv envname x state_n1) is))
  U.Int i -> return (state, X.Literal i)
  U.Char c -> return (state, X.Literal (toInteger (fromEnum c)))
  U.FunCall f es -> do
    (state', args) <- many state exp es
    return (state', X.FunCall f args)
