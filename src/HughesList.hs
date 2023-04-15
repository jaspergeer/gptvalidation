{-# LANGUAGE InstanceSigs #-}
module HughesList where

data List a = List ([a] -> [a])

(@) :: List a -> List a -> List a
(List f1) @ (List f2) = List (f1 . f2)

(&) :: a -> List a -> List a
a & (List f) = List (\ys -> a : (f ys))

empty :: List a
empty = List id

singleton :: a -> List a
singleton x = List (x :)

asList :: List a -> [a]
asList (List f) = f []

asHughesList :: [a] -> List a
asHughesList xs = List (xs ++)

instance Foldable List where
  foldr :: (a -> b -> b) -> b -> List a -> b
  foldr f z xs = foldr f z (asList xs)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f l = List (\ys -> map f (asList l) ++ ys)