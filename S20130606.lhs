> module S20130606 where

> import Prelude hiding (succ)

射で考える
圏論では射は最も重要
様々な概念を圏論の言葉だけで述べる

Hom集合
難解だけど重要
圏論に限らず数学のいたるところで出てくる

Hom集合に構造を入れる

> import Data.Monoid
> fib n = if n < 2 then n else fib(n-1)+fib(n-2)
> fact n = if n == 0 then 1 else n*fact(n-1)

(Sum . fact <> Sum . fib) 10
(Product . fact <> Product . fib) 10

point-freeのpointは集合の要素

> one = \() -> 1
> two = \() -> 2

one ()
=> 1

> zero = \() -> 0
> succ = \n -> n + 1

(succ . succ . succ . zero) ()
=> 3

Hom関手
