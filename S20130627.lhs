> module S20130627 where

様々な極限

Jにおいて横を繋ぐ射があるとどうなるか？

Setsでのイコライザ
  方程式の解集合を表す

Setsでのコイコライザ
  f(a)とg(a)の商集合を取る

任意の圏において
  イコライザはモノ
  コイコライザはエピ

引き戻し(pullback)
押し出し(pushout)

ω余完備 - プログラムの意味論で重要になる

代数的データ型
F代数とF余代数の圏において代数的データ型を考えることができる

自然数を射で表す

seqは全てを台無しにする.

F始代数: catamorphism
  バナナ括弧
cata- 下の方へ，崩すとか...

catamorphism : 畳み込みを表現する(ある種の)再帰スキーム

プログラム運算

> filter p = foldr (\a x -> if p a then a:x else x)
> g a x = if even a then a + x else x
> f = foldr g 0 [1..20]

任意の関手から任意のcatamorphismを導出できる.

