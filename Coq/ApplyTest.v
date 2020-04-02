Theorem comm : forall x y : nat, x + y = y + x.
Admitted.

Theorem test1 : forall x y : nat, x + y = y + x -> x = y.
Admitted.

Theorem test2 : forall x y : nat, x = y.
Proof.
  intro x.
  intro y.
  apply (test1 x y (comm x y)).
Qed.

Inductive Test : Prop :=
  | Oof : Prop -> Test
  | OofOof : Test.
  
Inductive Guanxuan : Set :=
  | Wu : Guanxuan -> Guanxuan.
  
Theorem www : Guanxuan -> False.
Proof.
  intros.
  induction H.
  contradiction.
Qed.