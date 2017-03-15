{-# LANGUAGE DeriveFunctor #-}
module Lambek where

-- Lambek
-- ListF(X) = 1 + A * X
-- [A] ~= ListF([A])
-- [A] ~= 1 + A * [A]
-- [A] ~= Maybe (A, [A])

-- in
f :: Maybe (a, [a]) -> [a]
f Nothing = []
f (Just (a,as)) = a:as

-- in^(-1)
f' :: [a] -> Maybe (a, [a])
f' [] = Nothing
f' (a:as) = Just (a,as)
