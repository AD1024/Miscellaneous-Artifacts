Require Import Arith.

Theorem lem :
forall a b,
a + b = 0 -> a = 0 /\ b = 0.
Proof.
  intros.
  induction a.
  - simpl in H. split; auto.
  - split.
    * discriminate.
    * discriminate.
Qed.

Theorem and_l :
forall A B,
  A /\ B -> A.
Proof.
  intros. elim H. intros. auto.
Qed.

Theorem oof :
  forall a, a + a = a * 2.
Proof.
  intros.
  induction a.
  - simpl. auto.
  - simpl. rewrite addS. rewrite IHa. auto.
Qed.

Theorem times_get_zero :
  forall x y,
  x * y = 0 -> x = 0 \/ y = 0.
Proof.
  intros.
  induction x.
  - simpl. left. auto.
  - simpl. right. simpl in H. induction y.
    * auto.
    * auto. discriminate.
Qed.

Theorem or_false_elim :
  forall A B,
  A \/ B -> B = False -> A.
Proof.
  intros.
  elim H.
  - auto.
  - rewrite H0. intros. destruct H1.
Qed.

Theorem times_const_zero :
  forall x,
  x * 2 = 0 -> x = 0.
Proof.
  intros.
  induction x.
  - auto.
  - discriminate.
Qed.


Theorem addS :
  forall x y,
  x + S y = S (x + y).
Proof.
  intros.
  induction x.
  - auto.
  - simpl. rewrite IHx. auto.
Qed.

Theorem double_add :
forall a b,
a + a = b + b -> a = b.
Proof.
  induction a.
  - intros. simpl in H. destruct b.
    * auto.
    * discriminate.
  - destruct b.
    * intros. discriminate.
    * intros. simpl in H.
      rewrite addS in H. rewrite (addS b) in H.
      apply (eq_add_S (S (a + a)) (S(b + b))) in H.
      apply eq_add_S in H.
      rewrite (IHa b H).
      reflexivity.
Qed.