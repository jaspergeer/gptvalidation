module SymbolicExecution where

import Prelude hiding (sequence, exp)

import qualified SymbolicExpression as SX
import qualified Env as E
import qualified UnambiguousAST as U

import qualified AST
import Control.Exception (throw, Exception)
import Data.Foldable (foldlM)

zero :: SX.Expr
zero = SX.I32 0

void :: a -> [(a, SX.Expr)]
void s = [(s, zero)]

type Name = String

type VarEnv = E.Env (SX.Expr, SX.Type)

data SymExException = NotBound Name Name
                    | NotPointer Name
                    | AlreadyBound Name Name
                    | UnknownAlias
                    | Unsupported String

instance Show SymExException where
  show (NotBound x env) = "name " ++ x ++ " not bound in environment " ++ env
  show (NotPointer x) = "name " ++ x ++ " is not bound to a pointer"
  show (AlreadyBound x env) = "name " ++ x ++ " already bound in environemnt " ++ env
  show UnknownAlias = "cannot determine the memory location pointed to by expression"
  show (Unsupported feature) = feature ++ " is not supported"

instance Exception SymExException

typeInRho :: Name -> SymbolicState -> SX.Type
typeInRho x state = case E.find x (rho state) of
  Just (_, tau) -> tau
  _ -> throw (NotBound x "rho")

typeInMu :: Name -> SymbolicState -> SX.Type
typeInMu x state = case E.find x (mu state) of
  Just (_, tau) -> tau
  _ -> throw (NotBound x "mu")

valInRho :: Name -> SymbolicState -> SX.Expr
valInRho x state = case E.find x (rho state) of
  Just (s, _) -> s
  _ -> throw (NotBound x "rho")

valInMu :: Name -> SymbolicState -> SX.Expr
valInMu x state = case E.find x (mu state) of
  Just (s, _) -> s
  _ -> throw (NotBound x "mu")


bindInRho :: Name -> SX.Expr -> SymbolicState -> E.Env (SX.Expr, SX.Type)
bindInRho x e state =
  let rho' = rho state
      tau = typeInRho x state in
        E.bind x (e, tau) rho'

bindInMu :: Name -> SX.Expr -> SymbolicState -> E.Env (SX.Expr, SX.Type)
bindInMu x e state =
  let mu' = mu state
      tau = typeInMu x state in
        E.bind x (e, tau) mu'

bindNewRho :: E.Name -> (SX.Expr, SX.Type) -> SymbolicState -> E.Env (SX.Expr, SX.Type)
bindNewRho x y state =
  let rho' = rho state in
    if rho' `E.binds` x then throw (AlreadyBound x "rho")
    else E.bind x y rho'


bindNewMu :: E.Name -> (SX.Expr, SX.Type) -> SymbolicState -> E.Env (SX.Expr, SX.Type)
bindNewMu x y state =
  let mu' = mu state in
    if mu' `E.binds` x then throw (AlreadyBound x "rho")
    else E.bind x y mu'

-- symbolic state: (g, rho, mu)
data SymbolicState = SymbolicState { g ::SX.Expr, rho :: VarEnv, mu :: VarEnv }

-- symbolic execution with branches
type SymbolicExecutor a = (SymbolicState, a) -> [(SymbolicState, SX.Expr)]

sequence :: SymbolicExecutor a -> SymbolicExecutor [a]
sequence exec (s1, as) =
  let
    start = void s1
    makeNewStarts branches a = foldr (\(state, _) starts -> (state, a) : starts) [] branches
  in foldl (\branches a -> makeNewStarts branches a >>= exec) start as

dim :: SX.Type -> Int
dim tau = case tau of
  SX.Base _ -> 0
  SX.Generic (SX.Ptr tau') -> 1 + dim tau'
  SX.Generic (SX.Fun {}) -> error "dim of function"

base :: SX.Type -> SX.Base
base tau = case tau of
  (SX.Base b) -> b
  (SX.Generic (SX.Ptr tau')) -> base tau'
  _ -> error "unimp"

stmt :: SymbolicExecutor U.Stmt
stmt (state, c) = case c of
  U.CompoundStmt cs -> sequence stmt (state, cs)
  U.Expr e -> (\(s', _) -> (s', zero)) <$> exp (state, e)
  U.IfElse e c1 c2 -> do
    (state', g1) <- exp (state, e)
    let state_1 = state' {g = SX.BinExpr (g state') AST.LAnd g1}
    let notg1 = SX.UnExpr AST.LNot g1
    let state_2 = state' {g = SX.BinExpr (g state') AST.LAnd notg1}
    r1 <- stmt (state_1, c1)
    r2 <- stmt (state_2, c2)
    [r1, r2]
  U.DeclareStack b x ->
    return (state { rho = bindNewRho x (zero, SX.Base b) state}, zero)
  U.DeclareStackObj t x a ->
    let
      tau = SX.Generic t
      state' = state { rho = bindNewRho a (SX.NewArr (dim tau) (base tau), tau) state }
      state'' = state' { rho = bindNewRho x (SX.PtrTo a, SX.Generic (SX.Ptr tau)) state}
    in return (state'', zero)
  U.DeclareHeapObj t x a ->
    let
      tau = SX.Generic t
      state' = state { mu = bindNewMu a (SX.NewArr (dim tau) (base tau), tau) state }
      state'' = state' { rho = bindNewRho x (SX.PtrTo a, SX.Generic (SX.Ptr tau)) state}
    in return (state'', zero)
  U.Return e -> exp (state, e)
  _ -> throw (Unsupported "loops")

toOffset :: SX.Expr -> SX.Expr
toOffset s = case s of
  SX.PtrTo _ -> SX.BinExpr s AST.Add zero
  SX.BinExpr s1 AST.Add s2 -> case
    (toOffset s1, toOffset s2) of
      (SX.BinExpr s3@(SX.PtrTo _) AST.Add s4, s2') -> SX.BinExpr s3 AST.Add (SX.BinExpr s4 AST.Add s2')
      (s1', SX.BinExpr s3@(SX.PtrTo _) AST.Add s4) -> SX.BinExpr s3 AST.Add (SX.BinExpr s1' AST.Add s4)
      _ -> s
  SX.BinExpr s1 AST.Sub s2 -> case
    toOffset s1 of
      SX.BinExpr s3@(SX.PtrTo _) AST.Add s4 -> SX.BinExpr s3 AST.Add (SX.BinExpr s4 AST.Sub s2)
      _ -> s
  _ -> s

many :: SymbolicState -> SymbolicExecutor U.Expr -> [U.Expr] -> [(SymbolicState, [SX.Expr])]
many state exec = foldlM (\(state_i, is) e' -> do
        (state_i1, i) <- exec (state_i, e')
        return (state_i1, i:is)) (state, [])

updAtAddr :: Name -> [SX.Expr] -> SX.Expr -> SymbolicState -> SymbolicState
updAtAddr addr indices newval state =
  if rho state `E.binds` addr then
    let
      s = valInRho addr state
    in state { rho = bindInRho addr (SX.Upd s indices newval) state }
  else
    let
      s = valInMu addr state
    in state { mu = bindInMu addr (SX.Upd s indices newval) state }

exp :: SymbolicExecutor U.Expr
exp (state, e) = case e of
  U.BinExpr e_1 op e_2 -> do
    (state_1 , s_1) <- exp (state, e_1)
    (state_2, s_2) <- exp (state_1, e_2)
    return (state_2, SX.BinExpr s_1 op s_2)
  U.UnExpr op e_1 -> do
    (state', s) <- exp (state, e_1)
    return (state', SX.UnExpr op s)
  U.Assign (U.Var x) e_1 | rho state `E.binds` x -> do
    (state', s) <- exp (state, e_1)
    return (state' { rho = E.bind x (s, typeInRho x state) (rho state') }, s)
  U.Assign (U.Index x es) e_v -> do
    (state_n1, is) <- many state exp es
    (state', s') <- exp (state_n1, e_v)
    case valInRho x state_n1 of
      SX.PtrTo a -> return (updAtAddr a is s' state', s')
      _ -> throw (NotPointer x)
  U.Assign (U.Deref e_1) e_2 -> do
    (state_1, s_1) <- exp (state, e_1)
    (state_2, s_2) <- exp (state_1, e_2)
    case toOffset s_1 of
      SX.BinExpr (SX.PtrTo a) AST.Add i ->
        return (updAtAddr a [i] s_2 state_2, s_2)
      _ -> throw UnknownAlias
  U.Assign {} -> throw (Unsupported "assignment with complex left-hand sides")
  U.Deref e_1 -> do
    (state', s) <- exp (state, e_1)
    case s of
      SX.BinExpr (SX.PtrTo a) AST.Add s_1 ->
        if rho state' `E.binds` a then
          return (state', SX.Sel (valInRho a state') [s_1])
        else
          return (state', SX.Sel (valInMu a state') [s_1])
      _ -> throw UnknownAlias
  U.Var x -> return (state, valInRho x state)
  U.Index x es -> do
    (state_n1, is) <- many state exp es
    if rho state `E.binds` x then
      return (state, SX.Sel (valInRho x state_n1) is)
    else
      return (state, SX.Sel (valInMu x state_n1) is)
  U.Int i -> return (state, SX.I32 i)
  U.Char c -> return (state, SX.I8 (fromEnum c))
  U.FunCall f es -> do
    (state', args) <- many state exp es
    return (state', SX.FunCall f args)
