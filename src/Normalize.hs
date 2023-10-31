{-# LANGUAGE LambdaCase #-}

module Normalize where

import qualified AST
import Control.Monad.Trans.State (State, get, modify)
import Data.Kind (Type)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified NormalizedAST as N
import qualified Types as T

{-
 - distinguish between heap and stack declarations, generate unique names for
 - every instantiated heap object
 -}

expression :: M.Map AST.Name AST.Name -> AST.Expr -> State (M.Map T.Type Int) N.Expr
expression sub = \case
    AST.BinExpr e1 op e2 -> N.BinExpr <$> expression sub e1 <*> pure op <*> expression sub e2
    AST.LogExpr e1 op e2 -> N.LogExpr <$> expression sub e1 <*> pure op <*> expression sub e2
    AST.ShiftExpr e1 op e2 -> N.ShiftExpr <$> expression sub e1 <*> pure op <*> expression sub e2
    AST.RelExpr e1 op e2 -> N.RelExpr <$> expression sub e1 <*> pure op <*> expression sub e2
    AST.UnExpr op e -> N.UnExpr op <$> expression sub e

    -- TODO
    AST.Var n -> pure (N.Var $ fromMaybe n $ M.lookup n sub)

freshObjName :: T.Type -> State (M.Map T.Type Int) N.Name
freshObjName tau = do
    counts <- get

    let index = fromMaybe 0 $ M.lookup tau counts
    modify (M.insert tau $ index + 1)

    pure $ mconcat [prefix tau, "_", show index]
  where
    prefix = \case
        T.Integral i -> case i of
            T.Ptr t -> mconcat ["PtrTo<", prefix t, ">"]
            _ -> show i
        T.Complex (T.Arr i n) -> mconcat ["ArrOf<", prefix $ T.Integral i, show n, ">"]
