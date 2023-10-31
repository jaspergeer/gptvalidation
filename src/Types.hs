module Types where
import Prelude hiding (Integral)

data Integral = Int32 | Int8 | U32 | Ptr Type
  deriving (Eq, Ord, Show)
data Complex = Arr Integral Int
  deriving (Eq, Ord, Show)

data Type = Integral Integral | Complex Complex
  deriving (Eq, Ord, Show)

dim :: Type -> Int
dim tau = case tau of
  Integral (Ptr tau') -> 1 + dim tau'
  Integral _ -> 0
  Complex (Arr _ i) -> i

base :: Type -> Integral
base tau = case tau of
  Integral (Ptr tau') -> base tau'
  Integral t -> t
  Complex (Arr t _) -> t

int32 :: Type
int32 = Integral Int32

int8 :: Type
int8 = Integral Int8

u32 :: Type
u32 = Integral U32

ptrto :: Type -> Type
ptrto = Integral . Ptr

arr :: Integral -> Int -> Type
arr tau i = Complex (Arr tau i)
