module Mac where

m n = if n > 100 then n - 10 else m (m (n+11))
