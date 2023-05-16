{-# LANGUAGE FlexibleContexts #-}

module SBVConvert where

import qualified AST
import qualified SymbolicExecution as E
import qualified SymbolicExpression as X
import Data.SBV
import Data.SBV.Tuple

import Data.Map (toList)

-- Array helpers
toArrN :: (SFiniteBits a, SDivisible (SBV a), SIntegral a, SymVal a, HasKind tup) => ([SBV Int32] -> SBV tup) -> X.Expr -> Symbolic (SArray tup a)
toArrN tupleN e = case e of
  X.NewArr {} -> newArray_ Nothing
  X.Upd e1 es e2 -> do
    arr <- toArrN tupleN e1
    is <- tupleN <$> mapM convertExp es
    val <- convertExp e2
    return $ writeArray arr is val
  _ -> error "tried to interpret non-array value as array"

tuple1 :: [SBV a] -> SBV a
tuple1 [a] = a
tuple1 _ = undefined

tuple2 :: (SymVal a) => [SBV a] -> SBV (a, a)
tuple2 [a, b] = tuple (a, b)
tuple2 _ = undefined

toArr1 :: (SFiniteBits b, SIntegral b, SymVal b, SDivisible (SBV b)) => X.Expr -> Symbolic (SArray Int32 b)
toArr1 = toArrN tuple1

toArr2 :: (SFiniteBits b, SIntegral b, SymVal b, SDivisible (SBV b)) => X.Expr -> Symbolic (SArray (Int32, Int32) b)
toArr2 = toArrN tuple2

convertExp :: (SFiniteBits a, SDivisible (SBV a), SIntegral a, SymVal a, Num a, Ord a) => X.Expr -> Symbolic (SBV a)
convertExp e = case e of
  X.Literal i -> free $ show i
  X.FromType tau e1 -> case tau of
    X.Generic (X.Ptr _) -> do
      s <- convertExp e1 :: Symbolic (SBV Word32)
      return $ sFromIntegral s
    X.Base X.Int32 -> do
      s <- convertExp e1 :: Symbolic (SBV Int32)
      return $ sFromIntegral s
    X.Base X.Int8 -> do
      s <- convertExp e1 :: Symbolic (SBV Int8)
      return $ sFromIntegral s
    _ -> error "tried to enforce non-integral type"
  X.Sel e1 es -> do
    x <- mapM convertExp es
    case es of
      [_] -> do
        arr <- toArr1 e1
        return $ readArray arr (tuple1 x)
      [_, _] -> do
        arr <- toArr2 e1
        return $ readArray arr (tuple2 x)
      _ -> error "higher dimensional arrays not supported"
  X.ArithExpr e1 binop e2 ->
    let
      convertArith op = case op of
        AST.Add -> (+)
        AST.Sub -> (-)
        AST.Mul -> (*)
        AST.Div -> sDiv
        AST.Mod -> sMod
    in
      do
        s1 <- convertExp e1
        s2 <- convertExp e2
        return $ convertArith binop s1 s2
  X.LogExpr e1 binop e2 ->
    let
      convertLog op = case op of
        AST.LAnd -> (.&&)
        AST.LOr -> (.||)
    in do
      s1 <- convertExp e1 :: Symbolic SInt32 -- according to C99
      let b1 = s1 ./= 0
      s2 <- convertExp e2 :: Symbolic SInt32
      let b2 = s2 ./= 0
      return $ oneIf (convertLog binop b1 b2)
  X.BitExpr e1 binop e2 ->
    let
      convertBit op = case op of
        AST.BAnd -> (.&.)
        AST.BOr -> (.|.)
        AST.Shl -> sShiftLeft
        AST.Shr -> sSignedShiftArithRight
    in do
      s1 <- convertExp e1
      s2 <- convertExp e2
      return $ convertBit binop s1 s2
  X.RelExpr e1 binop e2 ->
    let
      convertRel op = case op of
        AST.Eq -> (.==)
        AST.Leq -> (.<=)
        AST.Geq -> (.>=)
        AST.Lt -> (.<)
        AST.Gt -> (.>)
    in do
      s1 <- convertExp e1 :: Symbolic SInt32
      s2 <- convertExp e2 :: Symbolic SInt32
      return $ oneIf (convertRel binop s1 s2)
  X.UnExpr op e1 -> do
    s1 <- convertExp e1
    case op of
      AST.Neg ->
        return $ negate s1
      AST.LNot ->
        return $ oneIf (s1 .== 0)
      AST.BNot ->
        return $ complement s1
  X.PtrTo n -> return $ sym ("_ptrto_" ++ n)
  X.NewArr {} -> error "array encountered as value"
  X.Upd {} -> error "array encountered as value"
  _ -> error "TODO"

-- Each heap object becomes an uninterpreted function with a constraint
convertEnv :: E.VarEnv -> Symbolic [()]
convertEnv env =
  let
    convertBinding (n, (e, tau)) =
      case X.dim tau of
        1 -> do
          let f = uninterpret n :: SBV Int32 -> SWord32
          arr <- toArr1 e
          constrain $ \(Forall x) -> readArray arr x .== f x
        2 -> do
          let f = uninterpret n :: SBV (Int32, Int32) -> SWord32
          arr <- toArr2 e
          constrain $ \(Forall x) -> readArray arr x .== f x
        _ -> error ""
  in mapM convertBinding (toList env)

