{-# LANGUAGE RankNTypes #-}
module Yoneda where

newtype CoYoneda f x = CoYoneda (forall a. (x -> a) -> f a)

liftCoYoneda :: Functor f => f x -> CoYoneda f x
liftCoYoneda x = CoYoneda (\f -> fmap f x)

lowerCoYoneda :: CoYoneda f x -> f x
lowerCoYoneda (CoYoneda y) = y id

-- lowerCoYoneda $ liftCoYoneda [1,2,3] => [1,2,3]
