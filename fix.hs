{-# LANGUAGE DeriveFunctor, TypeSynonymInstances, FlexibleInstances #-}
module Fix where

import Prelude hiding (Maybe(..))

newtype Fix f = In { out :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out

data ListF a x = Nil | Cons a x deriving (Show, Functor)
-- fmap (+1) Nil
-- fmap (+1) (Cons 0 1)

type List a = Fix (ListF a)

nil ::  List a
nil = In Nil

cons :: a -> List a -> List a
cons a as = In (Cons a as)

instance Show a => Show (List a) where
  show x = "(" ++ show (out x) ++ ")"

sum' = cata phi
  where
    phi Nil = 0
    phi (Cons a x) = a + x

-- sum' (cons 1 (cons 2 (cons 3 nil)))
-- => 6

data NonEmptyListF a x = Single a | Plus a x deriving (Show , Functor)
type NonEmptyList a = Fix (NonEmptyListF a)
single :: a -> NonEmptyList a
single x = In (Single x)

plus :: a -> NonEmptyList a -> NonEmptyList a
plus a x = In (Plus a x)

instance Show a => Show (NonEmptyList a) where
  show x = "{" ++ show (out x) ++ "}"

sum'' = cata phi
  where
    phi (Single a) = a
    phi (Plus a x) = a + x

safeHead = cata phi
  where
    phi (Single a) = a
    phi (Plus a _) = a

data NatF x = Zero | Suc x deriving (Show, Functor)
type Nat = Fix NatF
zero :: Nat
zero = In Zero
suc :: Nat -> Nat
suc x = In (Suc x)
instance Show Nat where
  show = show . out

num' = cata phi
  where
    phi Zero = 0
    phi (Suc x) = 1 + x

data Maybe a = Nothing | Just a deriving (Show, Functor)
nothing :: Maybe a
nothing = Nothing
just :: a -> Maybe a
just x = Just x



-- X ^ A
data ExpF a x = Hom { runExp :: a -> x } deriving Functor

type Exp a = Fix (ExpF a)

cont :: (a -> Exp a) -> Exp a
cont f = In (Hom f)
