module List where    
    import Relation.Binary.PropositionalEquality as Eq
    open Eq using (_≡_; refl; sym; trans; cong)
    open import Data.Nat using (ℕ; _+_; _*_)
    open import Data.Nat.Properties using (+-comm; *-comm)

    data List (A : Set) : Set where
        []   : List A
        _∷_ :  A → List A → List A

    infixr 5 _∷_
    {-# BUILTIN LIST List #-}

    value : ℕ → List ℕ → ℕ
    value _ [] = 0
    value b (x ∷ xs) = x + b * (value b xs)


    postulate
        -- plus-comm : ∀ (a b : ℕ) → a + b ≡ b + a
        -- mult-comm : ∀ (a b : ℕ) → a * b ≡ b * a
        add  : ℕ → List ℕ → ℕ → List ℕ
        mult : ℕ → List ℕ → ℕ → List ℕ
        -- Fact A
        value∘add  : (b : ℕ) → ∀ (L : List ℕ) → (y : ℕ) → value b (add b L y) ≡ (value b L) + y
        -- Fact B
        value∘mult : (b : ℕ) → ∀ (L : List ℕ) → (r : ℕ) → value b (mult b L r) ≡ (value b L) * r

    convert : (b : ℕ) → (c : ℕ) → (L : List ℕ) → List ℕ
    convert _ _ [] = []
    convert b c (x ∷ xs) = add c (mult c (convert b c xs) b) x

    -- Prove the property of change base
    change-base-≡ : (b : ℕ) → (c : ℕ) → ∀ (L : List ℕ) → value c (convert b c L) ≡ value b L
    change-base-≡ _ _ [] = refl
    change-base-≡ b c (x ∷ xs) 
                            rewrite value∘add c (mult c (convert b c xs) b) x   -- Apply Fact A
                                | value∘mult c (convert b c xs) b               -- Apply Fact B
                                | change-base-≡ b c xs                          -- Apply Induction Hypothesis
                                | *-comm (value b xs) b                      -- Commutativity of multiplication
                                | +-comm (b * (value b xs)) x = refl         -- Commutativity of addition
