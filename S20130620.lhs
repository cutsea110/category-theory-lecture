> module S20130620 where

積・余積 => それらを一般化した概念としての極限

- 終対象
  これは同型を除いて一意
- 始対象

Well-defined性
- 圏論の文脈ではなくもっと昔からある

双対性原理: 任意の圏における正しい命題は双対圏においても正しい命題となる.

- global elementという考え方

積はペア
余積は場合分け

積
> import Control.Arrow

(&&&) :: Arrow a => a b c -> a b c' -> a b (c, c') <= これはX -> Pへの射uにあたる

余積
> import Data.Either

q1 : Left
q2 : Right

(|||) :: ArrowChoice a => a b d -> a c d -> a (Either b c) d <= これはP -> Xへの射uにあたる

証明の圏では「かつ」「または」がそれぞれ積・余積に対応する


(***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')

(+++) :: ArrowChoice a => a b c -> a b' c' -> a (Either b b') (Either c c')

図式の見せ方がすばらしい!!

○記号論理学講義 清水義夫

圏の上の代数計算

