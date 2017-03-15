{-# LANGUAGE NoMonomorphismRestriction #-}
module Catamorphism where

import Prelude hiding (filter, foldr, sum, product, maybe)

import Data.Either hiding (either)

maybe :: (t, a -> t) -> Maybe a -> t
maybe (c, f) Nothing = c
maybe (c, f) (Just x) = f x
unmaybe :: (t -> Maybe a) -> t -> Maybe a
unmaybe = id
either :: (a -> t, b -> t) -> Either a b -> t
either (f, g) (Left x) = f x
either (f, g) (Right y) = g y
uneither :: (t -> Either a b) -> t -> Either a b
uneither = id

fold1 :: (a -> t, (a, t) -> t) -> List1 a -> t
fold1 = undefined
unfold1 :: (t -> Either a (a,t)) -> t -> List1 a
unfold1 = undefined

foldtree :: (a -> t, (a, t, t) -> t) -> Tree a -> t
foldtree (c, f) (Leaf x) = c x
foldtree (c, f) (Tree x l r) = f (x, foldtree (c, f) l, foldtree (c, f) r)
unfoldtree :: (t -> Either a (a, t, t)) -> t -> Tree a
unfoldtree = undefined

-- Type Functor
-- T(f) = (| in_B . F(f, 1_B)|)
--      = (|[Leaf, Tree] . (f + f x 1 x 1)|)
--      = (|[Leaf . f, Tree . (f x 1 x 1)|)
maptree :: (a -> b) -> Tree a -> Tree b
maptree f = foldtree (Leaf . f, (\(a, l, r) -> Tree (f a) l r))

foldtree' :: (t, (a, t, t) -> t) -> Tree a -> t
foldtree' = undefined
unfoldtree' :: (t -> Maybe (a, t, t)) -> t -> Tree a
unfoldtree' = undefined

--        in
--   X <------ A + B
--   |           |
--   |(|phi|)    | f(|phi|)
--   v           v
--   T <------ A + B
--       phi

--        in
--   X <------ 1 + A * X
--   |             |
--   |(|phi|)      | f(|phi|)
--   v             v
--   T <------ 1 + A * T
--       phi

data Nat = Zero | Succ Nat deriving (Show)

cataNat :: (t -> t, t) -> Nat -> t
cataNat (f, c) Zero = c
cataNat (f, c) (Succ n) = f (cataNat (f, c) n)
anaNat :: (t -> Maybe t) -> t -> Nat
anaNat f t = case f t of
  Nothing -> Zero
  Just s  -> Succ (anaNat f s)

foldr :: (a -> t -> t, t) -> [a] -> t
foldr (f, c) [] = c
foldr (f, c) (a:as) = f a (foldr (f, c) (as))
unfoldr :: (t -> Maybe (a, t)) -> t -> [a]
unfoldr f t = case f t of
  Nothing -> []
  Just (a, s) -> a : (unfoldr f s)

filter p = foldr (f, c)
  where
    c = []
    f a as = if p a then a:as else as

sum = foldr ((+), 0)

product = foldr ((*), 1)

sumFilterEven = foldr (f, c)
  where
    c = 0
    f a x = if even a then a + x else x

data List1 a = Unit a
             | Cons a (List1 a)
             deriving (Show)


cataList1 :: (a -> b -> b, a -> b) -> List1 a -> b
cataList1 (f, c) (Unit x) = c x
cataList1 (f, c) (Cons y ys) = f y (cataList1 (f, c) ys)

data Tree a = Leaf a
            | Tree a (Tree a) (Tree a)
            deriving (Show)

tree :: Integral a => Tree a
tree = Tree 12 (Leaf 6) (Tree 27 (Leaf 24) (Leaf 9))

cataTree :: (a -> b -> b -> b) -> (a -> b) -> Tree a -> b
cataTree g c (Leaf x) = c x
cataTree g c (Tree x l r) = g x (cataTree g c l) (cataTree g c r)

minTree :: Ord a => Tree a -> a
minTree = cataTree g c
  where
    c = id
    g x y z = min x (min y z)

gcdTree :: Integral a => Tree a -> a
gcdTree = cataTree g c
  where
    c = id
    g x y z = gcd x (gcd y z)

divideTree :: (Integral a) => Tree a -> a -> Tree a
divideTree = cataTree g c
  where
    c x = \m -> Leaf (x `div` m)
    g y l r = \m -> Tree (y `div` m) (l m) (r m)

normalize :: Integral a => Tree a -> Tree a
normalize = uncurry ($) . cataTree g c
  where
    c x = (\m -> Leaf (x `div` m), x)
    g y (l, l') (r, r') = (\m -> Tree (y `div` m) (l m) (r m), gcd y (gcd l' r'))

{--
normalize' :: Integral a => Tree a -> Tree a
normalize' t = cataTree g c t
  where
    m = cataTree (\x y z -> gcd x (gcd y z)) id t
    c x = Leaf (x `div` m)
    g y l r = Tree (y `div` m) l r
--}

gcd3 :: Integral a => a -> a -> a -> a
gcd3 x y z = gcd x (gcd y z)

normalize' :: Integral a => Tree a -> Tree a
normalize' t = cataTree g c t
  where
    m = cataTree gcd3 id t
    c x = Leaf (x `div` m)
    g y l r = Tree (y `div` m) l r

--        in = const g = \a -> g
--   X <------ X ^ A
--   |           |
--   | u         | (u.)
--   v           v
--   T <------ T ^ A
--       f
--
--  u = (|f|)

-- CPS
newtype Exp a = In { out :: a -> Exp a }
cata :: ((a -> c) -> c) -> Exp a -> c
cata f (In g) = f (cata f . g)
