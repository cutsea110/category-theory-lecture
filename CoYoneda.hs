{-# LANGUAGE RankNTypes #-}
module CoYoneda where

newtype CoYoneda f x = CoYoneda (forall a. (x -> a) -> f a)

liftCoYoneda :: Functor f => f x -> CoYoneda f x
liftCoYoneda x = CoYoneda (\f -> fmap f x)

lowerCoYoneda :: CoYoneda f x -> f x
lowerCoYoneda y = y id
