module Fixpoint where

-- import Data.Function (fix)

factF f = \n -> if n == 0 then 1 else n * f(n-1)

fact0 = factF undefined
-- fact0 0 => 1
-- fact0 1 => undefined
fact1 = factF . factF $ undefined
-- fact1 0 => 1
-- fact1 1 => 1
-- fact1 2 => undefined
fact2 = factF . factF . factF $ undefined

fix f = f (fix f)
fact = fix factF
