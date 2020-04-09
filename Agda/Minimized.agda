module Minimized where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open import Function using (_∘_)
-- open import Data.Product using (_×_; proj₁; proj₂ ) renaming (_,_ to ⟨_,_⟩)

data _×_ (A B : Set) : Set where
  ⟨_,_⟩ : A → B → A × B

proj₁ : ∀ {A B : Set} → A × B → A
proj₁ ⟨ x , y ⟩ = x

proj₂ : ∀ {A B : Set} → A × B → B
proj₂ ⟨ x , y ⟩ = y

postulate
  extensionality : ∀ {A B : Set} {f g : A → B}
               → (∀ (x : A) → f x ≡ g x)
               → f ≡ g

{-
With the following version of extensionality, the code compiles:
postulate
  extensionality : ∀ {A : Set} {B : A → Set} {f g : (x : A) → B x}
               → (∀ (x : A) → f x ≡ g x)
               → f ≡ g
-}

infix 0 _≃_
record _≃_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
open _≃_

η-× : ∀ {A B : Set} (w : A × B) → ⟨ proj₁ w , proj₂ w ⟩ ≡ w
η-× ⟨ x , y ⟩ = refl

-- Exercise
∀-distrib-× : ∀ {A : Set}{B C : Set} →
                (∀ (x : A) → B × C) ≃ (∀ (x : A) → B) × (∀ (x : A) → C)
∀-distrib-× =
    record
    {
        to = λ{f → ⟨ proj₁ ∘ f , proj₂ ∘ f ⟩} ;
        from = λ{⟨ A→Bx , A→Cx ⟩ → λ{x → ⟨ A→Bx x , A→Cx x ⟩}} ;
        from∘to = λ{f → extensionality (η-× ∘ f)} ;
        to∘from = λ{⟨ f , g ⟩ → refl}
    }

{-
from∘to fails to typecheck with this error message:
Cannot instantiate the metavariable _96 to solution .B x × .C x
since it contains the variable x which is not in scope of the
metavariable or irrelevant in the metavariable but relevant in the
solution
when checking that the inferred type of an application
  _f_97 f ≡ _g_98 f
matches the expected type
  (λ { ⟨ f , g ⟩ → λ { a → ⟨ f a , g a ⟩ } })
  ((λ { f → ⟨ (λ x → proj₁ (f x)) , (λ x → proj₂ (f x)) ⟩ }) f)
  ≡ f
-}
