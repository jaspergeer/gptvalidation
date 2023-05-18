module Types where
import Prelude hiding (Integral)

data Integral = Int32 | Int8 | U32 | Ptr Type deriving Show
data Complex = Arr Int Integral | Fun [Integral] Integral deriving Show

data Type = Integral Integral | Complex Complex deriving Show

dim :: Type -> Int
dim tau = case tau of
  Integral _ -> 0
  Complex (Arr i _) -> i
  Complex (Fun {}) -> error "IMPOSSIBLE: compute dim of function"

base :: Type -> Integral
base tau = case tau of
  Integral t -> t
  Complex (Arr _ t) -> t
  _ -> error "IMPOSSIBLE: compute base of function"
