module Types where
import Prelude hiding (Integral)

data Integral = Int32 | Int8 | U32 | Ptr Type deriving Show
data Complex = Arr Int Integral deriving Show

data Type = Integral Integral | Complex Complex deriving Show

dim :: Type -> Int
dim tau = case tau of
  Integral (Ptr tau') -> 1 + dim tau'
  Integral _ -> 0
  Complex (Arr i _) -> i

base :: Type -> Integral
base tau = case tau of
  Integral (Ptr tau') -> base tau'
  Integral t -> t
  Complex (Arr _ t) -> t

int32 :: Type
int32 = Integral Int32

int8 :: Type
int8 = Integral Int8

u32 :: Type
u32 = Integral U32

ptrto :: Type -> Type
ptrto = Integral . Ptr

arr :: Int -> Integral -> Type
arr i tau = Complex (Arr i tau)