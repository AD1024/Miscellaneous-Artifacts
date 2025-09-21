open Classical

variable (α : Type) (p q : α → Prop)
variable (r : Prop)

example : (∃ x : α, r) → r :=
    fun ⟨ _ , hr ⟩ => hr

example (a : α) : r → (∃ x : α, r) := fun hr => ⟨ a, hr ⟩
example : (∃ x, p x ∧ r) ↔ (∃ x, p x) ∧ r :=
    Iff.intro
        (fun ⟨ x, hp, hr ⟩ => ⟨ ⟨ x, hp ⟩, hr⟩ )
        (fun ⟨ ⟨ x, hp ⟩, hr ⟩ => ⟨ x, hp, hr ⟩)

example : (∃ x, p x ∨ q x) ↔ (∃ x, p x) ∨ (∃ x, q x) :=
    have c1 : (∃ x, p x ∨ q x) → (∃ x, p x) ∨ (∃ x, q x) :=
        fun ⟨ x, hpq ⟩ => Or.elim hpq (fun hp => Or.inl ⟨ x, hp ⟩)
                                      (fun hq => Or.inr ⟨ x, hq ⟩)
    have c2 : (∃ x, p x) ∨ (∃ x, q x) → (∃ x, p x ∨ q x) :=
        fun h => Or.elim h (fun ⟨ x, hp ⟩ => ⟨ x, Or.inl hp ⟩)
                                      (fun ⟨ x, hq ⟩ => ⟨ x, Or.inr hq ⟩)
    Iff.intro c1 c2


example : (∀ x, p x) ↔ ¬ (∃ x, ¬ p x) :=
    have c1 : (∀ x, p x) → ¬ (∃ x, ¬ p x) :=
        fun f₁ => byContradiction
                   (fun h =>
                        have h₁: ∃ x, ¬ p x := by rw [not_not] at h; assumption
                        match h₁ with
                        | ⟨ x, hnpx ⟩ => absurd (f₁ x) hnpx)
    have c2 : ¬ (∃ x, ¬ p x) → (∀ x, p x) :=
        fun f₂ => byContradiction
                    (fun h => have h₁: ∃ x, ¬ p x := Classical.not_forall.mp h
                                match h₁ with
                                | ⟨ x, hnpx ⟩ => (f₂ ⟨ x, hnpx ⟩))
    Iff.intro c1 c2

example : (∃ x, p x) ↔ ¬ (∀ x, ¬ p x) := by
    have c1 : (∃ x, p x) → ¬ (∀ x, ¬ p x) :=
        fun ⟨ x, px ⟩ => byContradiction
            (fun n =>
                have f := not_not.mp n
                absurd px (f x))
    have c2 : ¬ (∀ x, ¬ p x) → (∃ x, p x) := by
        intro h
        rw [not_forall] at h
        cases h with
        | intro w hw =>
            rw [not_not] at hw
            exact ⟨ w, hw ⟩
    exact ⟨ c1, c2 ⟩

example : (¬ ∃ x, p x) ↔ (∀ x, ¬ p x) := by
    constructor
    . intro h
      rw [not_exists] at h
      exact h
    . intro h h1
      cases h1 with
      | intro w hw =>
        specialize (h w)
        contradiction

example : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) := by
    constructor
    · intro h
      rw [not_forall] at h
      apply h
    · intro h h1
      cases h with
      | intro w hw =>
        apply hw
        apply h1

example : (∀ x, p x → r) ↔ (∃ x, p x) → r := by
    constructor
    . intro h h1
      cases h1 with
      | intro w hw =>
        apply (h w)
        assumption
    . intro h h1 ph1
      apply h
      exact ⟨ h1, ph1 ⟩

theorem if_or_equiv (P Q: Prop): P → Q ↔ ¬ P ∨ Q := by grind
open Nat

def div (x y : Nat) : Nat :=
  if h : 0 < y ∧ y ≤ x then
    have : x - y < x := Nat.sub_lt (Nat.lt_of_lt_of_le h.1 h.2) h.1
    div (x - y) y + 1
  else
    0

#reduce div 8 4

section Hidden
open List

def reverse_lst {α}: List α → List α
| nil => nil
| cons x xs => List.append (reverse_lst xs) [x]

theorem rev_app (x: α) (xs: List α):
    reverse_lst (xs ++ [x]) = x :: reverse_lst xs := by
    revert x
    induction xs with
    | nil => intro x; simp [reverse_lst]
    | cons y ys ih =>
        intro x
        simp [reverse_lst]
        rw [ih x]
        rfl

theorem rev_rev_id (xs: List α):
    reverse_lst (reverse_lst xs) = xs := by
    induction xs with
        | nil =>
            simp [reverse_lst]
        | cons x xs ih =>
            simp [reverse_lst]
            rw [rev_app, ih]

def rev' {α} (xs acc: List α): List α :=
    match xs with
    | nil => acc
    | cons x xs => rev' xs (x :: acc)

theorem rev'_app (xs acc: List α):
    rev' xs acc = (rev' xs []) ++ acc := by
    revert acc
    induction xs with
        | nil =>
            simp [rev']
        | cons hd tl ih =>
            intro acc
            simp [rev']
            rw [ih]
            rw [ih [hd]]; simp

theorem rev'_app2 (xs acc: List α):
    rev' (rev' xs acc) [] = (rev' acc []) ++ xs := by
    revert acc
    induction xs with
        | nil => simp [rev']
        | cons hd tl ih =>
            intro acc
            simp [rev']
            rw [ih]; simp[rev']
            rw [rev'_app]; simp

theorem rev_rev_id' (xs: List α):
    rev' (rev' xs []) [] = xs := by
    induction xs with
        | nil => simp [rev']
        | cons x xs ih =>
            simp [rev']
            rw [rev'_app α xs [x]]
            rw [←rev'_app2]
            simp [rev']
            rw[rev'_app2]
            simp [rev']
end Hidden
