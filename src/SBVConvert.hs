module SBVConvert where

import qualified SymbolicExpression as X
import Data.SBV
import Data.SBV.Tuple

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

toSBV :: (Integral a, SymVal a) => X.Expr -> Symbolic (SBV a)
toSBV e = case e of
  X.Literal i -> free $ show i
  X.Enforce tau e1 -> case tau of
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
  X.Sel e1 es@[_, _] -> do
    arr <- toArr2 e1 
    x <- mapM toSBV es
    return $ readArray arr (tuple2 x)
  X.NewArr {} -> error "array encountered as value"
  X.Upd {} -> error "array encountered as value"
  _ -> error "TODO"

toArrN :: (Integral a, SymVal a, HasKind tup) => ([SBV Int32] -> SBV tup) -> X.Expr -> Symbolic (SArray tup a)
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

toArr1 :: (Integral b, SymVal b) => X.Expr -> Symbolic (SArray Int32 b)
toArr1 = toArrN tuple1

toArr2 :: (Integral b, SymVal b) => X.Expr -> Symbolic (SArray (Int32, Int32) b)
toArr2 = toArrN tuple2

-- convertEnv :: E.VarEnv -> Symbolic SVal
-- convertEnv = error "TODO"