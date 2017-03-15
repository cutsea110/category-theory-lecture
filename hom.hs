module Hom where

-- 綺麗な関数だけを考えます

-- Hom(Integer,())
-- こいつはひとつしかない.
-- 型が決まると実装のバリエーションが決まる.

f :: Integer -> ()
f x = ()

g :: Integer -> ()
g x = g x

{--
f :: () -> Integer
f x = constant integerの数だけありえる
--}

