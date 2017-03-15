module Magma where

import Data.Monoid

class Magma a where
  magappend :: a -> a -> a

data FreeMagma a = Leaf a | Node (FreeMagma a) (FreeMagma a) deriving Show

type BinaryTree = FreeMagma


-- "Hello" `magappend` "World" == "(Hello World)"
newtype Kakko = Kakko String deriving Show

instance Magma Kakko where
  Kakko x `magappend` Kakko y = Kakko ("(" ++ x ++ " " ++ y ++ ")")

foldMapMagma :: Magma b => (a -> b) -> FreeMagma a -> b
foldMapMagma f (Leaf x) = f x
foldMapMagma f (Node l r) = foldMapMagma f l `magappend` foldMapMagma f r

tree = Node (Leaf 0) (Node (Leaf 1) (Leaf 2))

newtype WrappedMonoid a = WrappedMonoid a deriving Show

instance Monoid a => Magma (WrappedMonoid a) where
  WrappedMonoid x `magappend` WrappedMonoid y = WrappedMonoid (x `mappend` y)

-- foldMapMagma (WrappedMonoid . Any . even) tree
-- foldMapMagma (WrappedMonoid .  Sum) tree
-- foldMapMagma (WrappedMonoid . Sum) tree
-- foldMapMagma (WrappedMonoid . Sum . (+3)) tree
