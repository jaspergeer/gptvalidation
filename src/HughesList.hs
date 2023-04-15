module HughesList where

type HughesList a = [a] -> [a]

empty :: HughesList a
empty tail = tail

singleton :: a -> HughesList a
singleton e tail = e : tail

asList :: [a] -> HughesList a
asList es tail = es ++ tail