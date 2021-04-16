From Coq Require Import Arith.
(* Preloaded:
*)

Fixpoint fib (n : nat) : nat :=
  match n with
  | 0 => 0
  | 1 => 1
  | S (S n as n') => fib n' + fib n
  end.

Fixpoint fib_aux (a b n : nat) : nat :=
  match n with
  | 0 => a
  | S n => fib_aux b (a + b) n
  end.
  
Definition fib2 (n : nat) : nat := fib_aux 0 1 n.

Lemma fib_ok:
forall n aux,
fib_aux (fib aux) (fib (S aux)) n = fib (n + aux).
Proof.
 intro n.
 induction n; intro aux.
 - cbn [fib_aux]. simpl. auto.
 - cbn [fib_aux]. rewrite (plus_comm (fib aux) _). change (fib (S aux) + (fib aux)) with (fib (S (S aux))). rewrite IHn. rewrite plus_comm. cbn -[fib]. rewrite plus_comm. auto.
Qed. 

Theorem fib_eq (n : nat) : fib2 n = fib n.
Proof.
unfold fib2.
pose proof (fib_ok n 0).
cbn -[fib_aux] in H.
rewrite plus_comm in H.
cbn -[fib_aux] in H.
exact H.
Qed.

Record IsMagic (A : Set) (f : A -> A -> A) : Prop :=
  is_magic {
    left  : forall x y, f (f x y) y = x;
    right : forall x y, f y (f y x) = x
  }.
Arguments IsMagic {_} f.

Record IsComm (A : Set) (f : A -> A -> A) : Prop :=
  is_comm {
    comm : forall x y, f x y = f y x
  }.
Arguments IsComm {_} f.


Theorem magic_is_comm : forall (A : Set) (f : A -> A -> A), IsMagic f -> IsComm f.
Proof.
  intros.
  pose proof (left _ _ H) as left.
  pose proof (right _ _ H) as right.
  econstructor.
  intros.
  assert (forall x y, f (f x y) y = f (f y x) y -> f x y = f y x).
  - congruence.
  - apply H0.
    rewrite left.
    pattern x at 1.
    rewrite <- right with (x := x) (y := y).
    
Admitted.