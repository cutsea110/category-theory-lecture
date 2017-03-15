{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Cata2 where

import Prelude hiding (sum, product)

data Mu f = In { out :: f (Mu f) }

cata :: Functor f => (f a -> a) -> Mu f -> a
cata phi = phi . fmap (cata phi) . out

data List1F a x = Wrap a | Cons a x deriving (Functor, Show)

--          In
-- Mu f <---------- f (Mu f)
--   |                |
--   | (|phi|)        | f (|phi|)
--   v                v
--   a  <----------  f a
--          phi
--  f = List1F a
type List1 a = Mu (List1F a)
instance Show a => Show (List1 a) where
  show x = show $ out x

wrap :: a -> List1 a
wrap x = In (Wrap x)
cons :: a -> List1 a -> List1 a
cons x xs = In (Cons x xs)

sum = cata phi
  where
    phi (Wrap a) = a
    phi (Cons a x) = a + x

product = cata phi
  where
    phi (Wrap a) = a
    phi (Cons a x) = a * x
