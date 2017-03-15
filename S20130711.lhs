> module S20130711 where

表示的意味論
=> 数学的
   究極的にはこのプログラムを完全にエミュレートする数学的なオブジェクトは何か?を考えたい
操作的意味論
公理的意味論
=> 論理学

今回は表示的意味論をやります.

表示的意味論の土台が領域理論

- 計算の領域は最小値を持つ完備半順序集合である
- 計算を表示する関数は連続関数である

最小不動点定理

> import Data.Function

> fact n = if n == 0 then 1 else n * fact (n - 1)
> fact' = \n -> if n == 0 then 1 else n * fact' (n - 1)
> fact'' = (\f -> (\n -> if n == 0 then 1 else n * f (n - 1))) fact''

> fact''' = fix (\f -> (\n -> if n == 0 then 1 else n * f (n - 1)))

最小不動点を求めてみよう
fixpoint.hsへ


