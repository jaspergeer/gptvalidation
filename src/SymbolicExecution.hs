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

zero :: X.Expr
zero = mkI32 0

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
  show ex = case ex of
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
data SymbolicState = SymbolicState { g :: X.Expr, rho :: VarEnv, mu :: VarEnv }

-- symbolic execution with branches
type SymbolicExecutor a = (SymbolicState, a) -> [(SymbolicState, X.Expr)]

sequence :: SymbolicExecutor a -> SymbolicExecutor [a]
sequence exec (s1, as) =
  let
    start = void s1
    makeNewStarts branches a = foldr (\(state, _) starts -> (state, a) : starts) [] branches
  in foldl (\branches a -> makeNewStarts branches a >>= exec) start as

stmt :: SymbolicExecutor U.Stmt
stmt (state, c) = case c of
  U.CompoundStmt cs -> sequence stmt (state, cs)
  U.Expr e -> (\(s', _) -> (s', zero)) <$> exp (state, e)
  U.IfElse e c1 c2 -> do
    (state', g1) <- exp (state, e)
    let state_1 = state' {g = X.BinExpr (g state') AST.LAnd g1}
    let notg1 = X.UnExpr AST.LNot g1
    let state_2 = state' {g = X.BinExpr (g state') AST.LAnd notg1}
    r1 <- stmt (state_1, c1)
    r2 <- stmt (state_2, c2)
    [r1, r2]
  U.DeclareStack b x ->
    return (state { rho = bindNewRho x (zero, X.Base b) state}, zero)
  U.DeclareStackObj t x a ->
    let
      tau = X.Generic t
      state' = state { rho = bindNewRho a (X.NewArr (X.dim tau) (X.base tau), tau) state }
      state'' = state' { rho = bindNewRho x (X.PtrTo a, X.Generic (X.Ptr tau)) state}
    in return (state'', zero)
  U.DeclareHeapObj t x a ->
    let
      tau = X.Generic t
      state' = state { mu = bindNewMu a (X.NewArr (X.dim tau) (X.base tau), tau) state }
      state'' = state' { rho = bindNewRho x (X.PtrTo a, X.Generic (X.Ptr tau)) state}
    in return (state'', zero)
  U.Return e -> exp (state, e)
  _ -> throw (Unsupported "loops")

toOffset :: X.Expr -> X.Expr
toOffset s = case s of
  X.PtrTo _ -> X.BinExpr s AST.Add zero
  X.BinExpr s1 AST.Add s2 -> case
    (toOffset s1, toOffset s2) of
      (X.BinExpr s3@(X.PtrTo _) AST.Add s4, s2') -> X.BinExpr s3 AST.Add (X.BinExpr s4 AST.Add s2')
      (s1', X.BinExpr s3@(X.PtrTo _) AST.Add s4) -> X.BinExpr s3 AST.Add (X.BinExpr s1' AST.Add s4)
      _ -> s
  X.BinExpr s1 AST.Sub s2 -> case
    toOffset s1 of
      X.BinExpr s3@(X.PtrTo _) AST.Add s4 -> X.BinExpr s3 AST.Add (X.BinExpr s4 AST.Sub s2)
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
      tau = X.Base $ X.base (typeInRho addr state)
    in state { rho = bindInRho addr (X.Upd (X.Enforce tau s) indices newval) state }
  else
    let
      s = valInMu addr state
      tau = X.Base $ X.base (typeInMu addr state)
    in state { mu = bindInMu addr (X.Upd (X.Enforce tau s) indices newval) state }

mkI32 :: Integer -> X.Expr
mkI32 = X.Enforce (X.Base X.Int32) . X.Literal

mkI8 :: Integer -> X.Expr
mkI8 = X.Enforce (X.Base X.Int8) . X.Literal

exp :: SymbolicExecutor U.Expr
exp (state, e) = case e of
  U.BinExpr e_1 op e_2 -> do
    (state_1 , s_1) <- exp (state, e_1)
    (state_2, s_2) <- exp (state_1, e_2)
    return (state_2, X.BinExpr s_1 op s_2)
  U.UnExpr op e_1 -> do
    (state', s) <- exp (state, e_1)
    return (state', X.UnExpr op s)
  U.Assign (U.Var x) e_1 | rho state `E.binds` x -> do
    let tau = typeInRho x state
    (state', s) <- exp (state, e_1)
    return (state' { rho = E.bind x (X.Enforce tau s, tau) (rho state') }, s)
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
      X.BinExpr (X.PtrTo a) AST.Add i ->
        return (updAtAddr a [i] s_2 state_2, s_2)
      _ -> throw UnknownAlias
  U.Assign {} -> throw (Unsupported "assignment with complex left-hand side")
  U.Deref e_1 -> do
    (state', s) <- exp (state, e_1)
    case s of
      X.BinExpr (X.PtrTo a) AST.Add s_1 ->
        if rho state' `E.binds` a then
          return (state', X.Sel (valInRho a state') [s_1])
        else
          return (state', X.Sel (valInMu a state') [s_1])
      _ -> throw UnknownAlias
  U.Var x -> return (state, valInRho x state)
  U.Index x es -> do
    (state_n1, is) <- many state exp es
    if rho state `E.binds` x then
      return (state, X.Sel (valInRho x state_n1) is)
    else
      return (state, X.Sel (valInMu x state_n1) is)
  U.Int i -> return (state, mkI32 i)
  U.Char c -> return (state, mkI8 (toInteger (fromEnum c)))
  U.FunCall f es -> do
    (state', args) <- many state exp es
    return (state', X.FunCall f args)
