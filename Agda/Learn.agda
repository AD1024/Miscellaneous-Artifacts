module Learn where
    open import Data.Empty using (⊥; ⊥-elim)
    open import Data.Nat using (ℕ; suc; zero; _+_; _*_)
    open import Relation.Binary.PropositionalEquality as Eq using (_≡_; refl)
    -- open import Data.Bool using (not)

    data Bool : Set where
        True : Bool
        False : Bool

    data Vec (A : Set) : ℕ → Set where
        empty : Vec A zero
        cons  : {n : ℕ} → A → Vec A n → Vec A (suc n)

    data Maybe (A : Set) : Set where
        Just    : A → Maybe A
        Nothing : Maybe A

    data Σ (A : Set) (B : A → Set) : Set where
         ⟨_,_⟩ : (x : A) → B x → Σ A B

    Σ-syntax = Σ
    infix 2 Σ-syntax
    syntax Σ-syntax A (λ x → B) = Σ[ x ∈ A ] B

    ∃ : ∀ {A : Set} (B : A → Set) → Set
    ∃ {A} B = Σ A B

    ∃-syntax = ∃
    syntax ∃-syntax (λ x → B) = ∃[ x ] B

    not : Set → Set
    not A = A → ⊥

    obj : Maybe ⊥
    obj = Nothing

    J : (A : Set) → (C : (x y : A) → x ≡ y → Set)
                  → ((x : A) → C x x refl)
                  → (m n : A) → (P : m ≡ n) → C m n P
    J A C f m n refl = f m

    K : (A : Set) (M : A) (C : M ≡ M -> Set)
      -> C refl
      -> (loop : M ≡ M) -> C loop
    K A M C s refl = s

    cong : ∀ {A B : Set}{x y : A}(f : A → B) -> x ≡ y -> f x ≡ f y
    cong f refl = refl


    plus-trans : ∀ (x y z : ℕ) → x + y + z ≡ x + (y + z)
    plus-trans 0 y z = refl
    plus-trans (suc x) y z = cong suc (plus-trans x y z)

    plus-mult-distrib : ∀ (m n p : ℕ) → (m + n) * p ≡ m * p + n * p
    plus-mult-distrib zero n p = refl
    plus-mult-distrib (suc m) n p
      rewrite (plus-mult-distrib m n p) | (plus-trans p (m * p) (n * p)) = refl

    isZero : ℕ → Bool
    isZero 0 = True
    isZero (suc _) = False

    is-zero : ∀ (n : ℕ) → isZero n ≡ True → n ≡ 0
    is-zero 0 _ = refl
    is-zero (suc _) ()

    data Expr : Set where
      Const : ℕ → Expr
      Plus : Expr → Expr → Expr

    flip : Expr → Expr
    flip (Const x) = (Const x)
    flip (Plus e1 e2) = Plus (flip e2) (flip e1)

    double-flip : ∀ (e : Expr) → flip (flip e) ≡ e
    double-flip (Const x) = refl
    double-flip (Plus e1 e2) rewrite double-flip e1 | double-flip e2 = refl



    -- maybe-false : {A : Set} → (∃ [ x ] (Maybe A → A → ⊥))
    -- maybe-false ⟨ Nothing , f ⟩ = f Nothing
    -- -- maybe-false

    -- F : ℕ → Set
    -- -- F (suc (suc (suc n))) = F n
    -- F (suc (suc n)) = Bool
    -- F (suc n) = Bool
    -- F Z = ℕ

    -- f : (n : ℕ) → F n
    -- -- f (suc (suc (suc n))) = f n
    -- f (suc (suc n)) = True
    -- f (suc n) = False
    -- f zero = 32
