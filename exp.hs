module Exp where

-- data Exp a t = In { out :: a -> t }
type Exp a = ((->) a)
