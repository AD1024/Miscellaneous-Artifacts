Require Import Coq.Arith.Plus.

Theorem plus_plus_eq:
forall x y,
x + x = y + y -> x = y.
Proof.
induction x as [| x' IHx].
- simpl.
  destruct y; simpl; auto.
  + discriminate.
- intro y.
  intro H.
  destruct y.
  + discriminate.
  + inversion H.
    rewrite plus_comm in H1.
    rewrite (plus_comm y (S y)) in H1. 
    simpl in H1. inversion H1.
    pose proof (IHx y H2) as ourGoal. f_equal. auto.
Qed.

Print plus_plus_eq.
Print nat_ind.