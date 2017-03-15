{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Cata where

import Prelude hiding (sum, succ)

-- Fix f = f (Fix f)
--    T    = f     T
data Fix f = In { out :: f (Fix f) }

data ListF a x = Nil | Cons a x deriving (Functor, Show)

-- fmap (+1) Nil
-- fmap (+1) (Cons 0 1)
-- fmap length Nil
-- fmap length (Cons 0 "hello")

type List a = Fix (ListF a)

instance Show a => Show (List a) where
  show x = show $ out x

nil :: List a
nil = In Nil
cons :: a -> List a -> List a
cons a as = In (Cons a as)


--
--          in
-- Fix f <------ f (Fix f)
--   |              |
--   |(|phi|)       | f (|phi|)
--   v              v
--   a   <------   f a
--         phi
--
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out

sum = cata phi
  where
    phi Nil = 0
    phi (Cons a x) = a + x

-- sum $ cons 1 (cons 2 (cons 3 nil)    


data NatF x = Zero | Succ x deriving (Functor, Show)
type Nat = Fix NatF
zero :: Nat
zero = In Zero
succ :: Nat -> Nat
succ n = In (Succ n)
instance Show Nat where
  show n = show $ out n

nat = cata phi
  where
    phi Zero = 1
    phi (Succ n) = 2 * n

-- nat $ succ (succ (succ zero))
