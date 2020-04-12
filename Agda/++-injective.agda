{-# OPTIONS --safe #-}
module ++-Injective where

    open import Relation.Binary.PropositionalEquality using (cong; refl; _≡_; sym)
    open import Data.List
    -- you can import other functions from the stdlib here
    open Relation.Binary.PropositionalEquality.≡-Reasoning
    open import Data.List.Properties

    ∷-injective₀  : ∀ {ℓ} {A : Set ℓ} (z : A) (a b : List A) → z ∷ a ≡ z ∷ b → a ≡ b
    ∷-injective₀  _ _ _ refl = refl

    ∷-injective-elem : ∀ {ℓ} {A : Set ℓ} (z w : A) (a b : List A) → a ≡ b → z ∷ a ≡ w ∷ b → z ≡ w
    ∷-injective-elem _ _ _ _ refl refl = refl

    lem : ∀ {ℓ} {A : Set ℓ} (a b c : List A) (x : A) → (x ∷ a) ++ b ≡ (x ∷ a) ++ c → a ++ b ≡ a ++ c
    lem a b c x eq = ∷-injective₀ x (a ++ b) (a ++ c) eq

    ++-injectiveʳ : ∀ {ℓ} {A : Set ℓ} (a b c : List A) → a ++ b ≡ a ++ c → b ≡ c
    ++-injectiveʳ [] _ _ refl = refl
    ++-injectiveʳ (x ∷ axs) b c eq = ++-injectiveʳ axs b c (lem axs b c x eq)


    lem-l : ∀ {ℓ} {A : Set ℓ} (a : List A) → a ++ [] ≡ a
    lem-l [] = refl
    lem-l (x ∷ xs) = cong (x ∷_) (lem-l xs)

    -- pretty hard
    -- try to use cong to convert to an eazier problem
    ++-injectiveˡ : ∀ {ℓ} {A : Set ℓ} (a b c : List A) → a ++ c ≡ b ++ c → a ≡ b
    ++-injectiveˡ a b [] eq rewrite lem-l a | lem-l b with eq
    ... | refl = refl
    -- a ++ x :: c === b ++ x :: c
    -- 
    ++-injectiveˡ a b (x ∷ c) eq 
        rewrite sym (++-assoc a [ x ] c) 
            | sym (++-assoc b [ x ] c) = ∷ʳ-injectiveˡ a b (++-injectiveˡ (a ∷ʳ x) (b ∷ʳ x) c eq)