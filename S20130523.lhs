> module S20130523 where
> import Data.Monoid
> import Data.Foldable
> import Data.Tree

> data AddMul = AddMul Integer Integer deriving Show
> instance Monoid AddMul where
>   mempty  = AddMul 1 0
>   AddMul a b `mappend` AddMul c d = AddMul (a * c) (a * d + b)

> tree = unfoldTree (\x -> (x, [1..x-1])) 4
> x = appEndo (Data.Foldable.foldMap (\x -> if even x then Endo (x *) else Endo (+ x)) tree) 0
> y = foldMap (\x -> if even x then AddMul x 0 else AddMul 1 x) tree
> 

