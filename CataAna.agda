
{--
record Enumeration A : Set where
  constructor enumeration
  field
    start : A
    forward : A → A
    backward : A → A

open Enumeration

3rd : {A : Set} → Enumeration A → A
3rd e = forward e (forward e (forward e (start e)))

backward-2 : {A : Set} → Enumeration A → A → A
backward-2 e a = backward e (backward e a)
  where open Enumeration

open import Data.Nat

enum-ℕ : Enumeration ℕ
enum-ℕ = enumeration 0 suc pred

open import Relation.Binary.PropositionalEquality

test₁ : 3rd enum-ℕ ≡ 3
test₁ = refl

test₂ : backward-2 enum-ℕ 5 ≡ 3
test₂ = refl

-- copattern
enum-ℕ' : Enumeration ℕ
start    enum-ℕ' = 0
forward  enum-ℕ' n = suc n
backward enum-ℕ' zero = zero
backward enum-ℕ' (suc n) = n

test₃ : 3rd enum-ℕ' ≡ 3
test₃ = refl

test₄ : backward-2 enum-ℕ' 5 ≡ 3
test₄ = refl
--}

open import Data.Nat

