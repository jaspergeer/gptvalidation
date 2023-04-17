module Solve where

-- import Prelude hiding (exp)

-- import qualified SymbolicExpression as SX

-- import Data.SBV.Dynamic

-- i32 :: Kind
-- i32 = KBounded True 32

-- i8 :: Kind
-- i8 = KBounded True 8

-- u32 :: Kind
-- u32 = KBounded False 32

-- exp :: SX.Expr -> SVal
-- exp e = case e of
--   (SX.Cast t1 t2 e) ->
--     let
--       cast SX.Int8 SX.Int32 = id
--     in cast t1 t2 (exp e)
--   _ -> undefined