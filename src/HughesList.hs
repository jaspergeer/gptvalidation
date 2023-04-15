module HughesList where

data List a = List ([a] -> [a])

(@) :: List a -> List a -> List a
(List f1) @ (List f2) = List (f1 . f2)

(&) :: a -> List a -> List a
a & (List f) = List (\ys -> a : (f ys))

empty :: List a
empty = List (\xs -> xs)

singleton :: a -> List a
singleton x = List (\xs -> x : xs)

asList :: List a -> [a]
asList (List f) = f []

asHughesList :: [a] -> List a
asHughesList xs = List (\ys -> xs ++ ys)

instance Foldable List where
  foldr f z xs = foldr f z (asList xs)
