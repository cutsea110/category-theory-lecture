{--
data Nat = Zero | Succ Nat deriving Show

-- foldNat
cata :: (a, a -> a) -> Nat -> a
cata (c, f) Zero = c
cata (c, f) (Succ n) = f (cata (c, f) n)

-- unfoldNat
ana :: (a -> Maybe a) -> a -> Nat
ana psi x = case psi x of
              Nothing -> Zero
              Just x' -> Succ (ana psi x')
--}

{--
data Bl = Tr | Fs deriving Show

-- like as `if'
cata :: (a, a) -> Bl -> a
cata (t, f) Tr = t
cata (t, f) Fs = f

ana :: (c -> Either a b) -> c -> Bl
ana psi b = case psi b of
              Left x -> Tr
              Right y -> Fs
--}

data List a = Nil | Cons a (List a) deriving Show

cata (c, f) Nil = c
cata (c, f) (Cons x xs) = f x (cata (c, f) xs)

ana :: (t -> Maybe (a, List a)) -> t -> List a
ana psi xss = case psi xss of
                Nothing -> Nil
                Just (x, xs) -> Cons x xs
