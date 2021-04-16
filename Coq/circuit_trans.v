Require Import Nat Lia String.

Inductive gate : Type :=
| Not : gate -> gate
| Or  : gate -> gate -> gate
| And : gate -> gate -> gate
| Const : string -> gate.

Fixpoint size (g : gate) : nat :=
    match g with
    | Const _ => 1
    | Not g   => size g
    | Or l r
    | And l r => size l + size r
    end.

Declare Scope gate_scope.
Delimit Scope gate_scope with gate.

Notation "A 'and' B" := (And A%gate B%gate) (at level 80, right associativity) : gate_scope.
Notation "A 'or' B" := (Or A%gate B%gate) (at level 85, right associativity) : gate_scope.
Arguments Const x%string_scope.

Open Scope gate_scope.

Example example_circ :=
    (Const "a" or Const "c") and ((Const "a" or Const "b") and (Const "b" or (Const "c"))) or (Const "a" and Const "b") and ((Const "a" or Const "e") and (Const "d" or (Const "c"))) and (Const "c" and Const "d") and ((Const "b" or Const "e") and (Const "a" or (Const "c" and Const "d" and Const "e" and Const "f" and Const "g" and Const "h" and Const "i" and Const "j" and Const "k" and Const "l"))).

Close Scope gate_scope.

Print example_circ.

Definition combine (g O0 O1 : gate) : gate :=
    And (Or (Not g) O1) (Or g O0).

Check ltb.

Fixpoint extract (d : nat) (g : gate) : (gate -> gate) * gate :=
    match g with
    | Const _ => (id, g)
    | Not g   => extract d g
    | Or l r  as dat
    | And l r as dat =>
        let (rem, t) := if ltb (size l) (size r) then (l, r) else (r, l) in
        if Nat.eqb ((size t) - (d / 3)) 0 then
            (And rem, t)
        else
            let (f, t') := extract d (if ltb (size l) (size r) then r else l) in
            match dat with
            | Not _
            | Const _ => (f, g)
            | Or _ _ => (fun x => Or rem (f x), t')
            | And _ _ => (fun x => And rem (f x), t')
            end
    end.

Definition result := (extract (size example_circ) example_circ).
Compute result.
Compute (fst result (Const "0")).

Fixpoint depth (g : gate) :=
    match g with
    | Const _ => 1
    | Not t   => S (depth t)
    | And l r
    | Or  l r => S (max (depth l) (depth r))
    end.

Fixpoint optimize (n : nat) (g : gate) : gate :=
    match n with
    | O => g
    | S n => 
        let (f, t) := extract (size g) g in
        let O0 := f (Const "0") in
        let O1 := f (Const "1") in
        let O0' := (optimize (pred n) O0) in
        let O1' := (optimize (pred n) O1) in
        let t'  := (optimize (pred n) t) in
        combine t' O0' O1'
    end.

Compute (optimize 3 example_circ).
Compute (depth example_circ).
Compute (depth (optimize 3 example_circ)).
    