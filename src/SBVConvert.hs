{-# LANGUAGE FlexibleContexts #-}
module SBVConvert where

import qualified SymbolicExpression as X
import Data.SBV
import Data.SBV.Tuple
import qualified AST

i8kind :: Kind
i8kind = KBounded True 8

i32kind :: Kind
i32kind = KBounded True 32

u32kind :: Kind
u32kind = KBounded False 32

toKind :: X.Base -> Kind
toKind X.Int32 = i32kind
toKind X.Int8 = i8kind
toKind X.U32 = u32kind

-- Array helpers
toArrN :: (SFiniteBits a, SDivisible (SBV a), SIntegral a, SymVal a, HasKind tup) => ([SBV Int32] -> SBV tup) -> X.Expr -> Symbolic (SArray tup a)
toArrN tupleN e = case e of
  X.NewArr {} -> newArray_ Nothing
  X.Upd e1 es e2 -> do
    arr <- toArrN tupleN e1
    is <- tupleN <$> mapM toSBV es
    val <- toSBV e2
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

toSBV :: (SFiniteBits a, SDivisible (SBV a), SIntegral a, SymVal a, Num a, Ord a) => X.Expr -> Symbolic (SBV a)
toSBV e = case e of
  X.Literal i -> free $ show i
  X.FromType tau e1 -> case tau of
    X.Generic (X.Ptr _) -> do
      s <- toSBV e1 :: Symbolic (SBV Word32)
      return $ sFromIntegral s
    X.Base X.Int32 -> do
      s <- toSBV e1 :: Symbolic (SBV Int32)
      return $ sFromIntegral s
    X.Base X.Int8 -> do
      s <- toSBV e1 :: Symbolic (SBV Int8)
      return $ sFromIntegral s
    _ -> error "tried to enforce non-integral type"
  X.Sel e1 es -> do
    x <- mapM toSBV es
    case es of
      [_] -> do
        arr <- toArr1 e1
        return $ readArray arr (tuple1 x)
      [_, _] -> do
        arr <- toArr2 e1
        return $ readArray arr (tuple2 x)
      _ -> error "higher dimensional arrays not supported"
  X.ArithExpr e1 op e2 ->
    let
      convertArith aop = case aop of
        AST.Add -> (+)
        AST.Sub -> (-)
        AST.Mul -> (*)
        AST.Div -> sDiv
        AST.Mod -> sMod
    in
      do
        s1 <- toSBV e1
        s2 <- toSBV e2
        return $ convertArith op s1 s2
  X.LogExpr e1 op e2 ->
    let
      convertLog lop = case lop of
        AST.LAnd -> (.&&)
        AST.LOr -> (.||)
    in do
      s1 <- toSBV e1 :: Symbolic SInt32 -- according to C99
      let b1 = s1 ./= 0
      s2 <- toSBV e2 :: Symbolic SInt32
      let b2 = s2 ./= 0
      return $ oneIf (convertLog op b1 b2)
  X.BitExpr e1 op e2 ->
    let
      convertBit bop = case bop of
        AST.BAnd -> (.&.)
        AST.BOr -> (.|.)
        AST.Shl -> sShiftLeft
        AST.Shr -> sSignedShiftArithRight
    in do
      s1 <- toSBV e1
      s2 <- toSBV e2
      return $ convertBit op s1 s2
  X.RelExpr e1 op e2 ->
    let
      convertRel rop = case rop of
        AST.Eq -> (.==)
        AST.Leq -> (.<=)
        AST.Geq -> (.>=)
        AST.Lt -> (.<)
        AST.Gt -> (.>)
    in do
      s1 <- toSBV e1 :: Symbolic SInt32
      s2 <- toSBV e2 :: Symbolic SInt32
      return $ oneIf (convertRel op s1 s2)
  X.UnExpr op e1 -> do
    s1 <- toSBV e1
    case op of
      AST.Neg ->
        return $ negate s1
      AST.LNot ->
        return $ oneIf (s1 .== 0)
      AST.BNot ->
        return $ complement s1
  X.NewArr {} -> error "array encountered as value"
  X.Upd {} -> error "array encountered as value"
  _ -> error "TODO"



-- convertEnv :: E.VarEnv -> Symbolic SVal
-- convertEnv = error "TODO"