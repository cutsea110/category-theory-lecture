import Data.List

average' = uncurry (/) . foldr g c
  where
    c = (0, 0)
    g x (s, l) = (x + s, 1 + l)
