module Anamorphism where

--       out
--   X ------> A + B
--   ^           ^
--   |[(psi)]    | f[(psi)]
--   |           |
--   T ------> A + B
--       psi

--       out
--   X ------> 1 + A * X
--   ^             ^
--   |[(psi)]      | f[(psi)]
--   |             |
--   T ------> 1 + A * T
--       psi

X -----> A + A * X * X

T -----> A + A * T * T


X ----> 1 + A * X * X

T ----> 1 + A * X * X
