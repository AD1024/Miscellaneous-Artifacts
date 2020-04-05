Set Warnings "-notation-overridden,-parsing".
Inductive exp : Type :=
  | Int (n : nat) : exp
  | Bool (b : bool) : exp
  | Add : exp -> exp -> exp
  | And : exp -> exp -> exp. 

Reserved Notation " t '--2>' t' " (at level 40).

Inductive step_2 : exp -> exp -> Prop :=
  | ST_AddConst2 
  : forall n1 n2, Add (Int n1) (Int n2) --2> Int (n1 + n2)
  | ST_AndBool2 
  : forall b1 b2, And (Bool b1) (Bool b2) --2> Bool (andb b1 b2)
  | ST_AddLeft2 
  : forall t1 t1' t2, t1 --2> t1' -> Add t1 t2 --2> Add t1' t2
  | ST_AddRight2
  : forall n1 t2 t2', t2 --2> t2' -> Add (Int n1) t2 --2> Add (Int n1) t2'
  | ST_AndLeft2
  : forall e1 e1' e2, e1 --2> e1' -> And e1 e2 --2> And e1' e2
  | ST_AndRight2
  : forall b1 e2 e2', e2 --2> e2' -> And (Bool b1) e2 --2> And (Bool b1) e2'
  where " t '--2>' t' " := (step_2 t t').

Reserved Notation " t '--2>*' t' " (at level 40).

Inductive step_2_multi : exp -> exp -> Prop :=
  | refl_2 : forall e, e --2>* e
  | multi_step_2 : forall e1 e1' e2, e1 --2> e1' -> e1' --2>* e2 -> e1 --2>* e2
  where " t '--2>*' t' " := (step_2_multi t t').
  
Reserved Notation " t '--3>' t' " (at level 40).

Inductive step_3 : exp -> exp -> Prop :=
  | ST_AddConst3 
  : forall n1 n2, Add (Int n1) (Int n2) --3> Int (n1 + n2)
  | ST_AndBool3 
  : forall b1 b2, And (Bool b1) (Bool b2) --3> Bool (andb b1 b2)
  | ST_AddLeft3 
  : forall t1 t1' t2, t1 --3> t1' -> Add t1 t2 --3> Add t1' t2
  | ST_AddRight3
  : forall n1 t2 t2', t2 --3> t2' -> Add (Int n1) t2 --3> Add (Int n1) t2'
  | ST_AndLeft3
  : forall e1 e1' e2, e1 --3> e1' -> And e1 e2 --3> And e1' e2
  | ST_AndFalse
  : forall e, And (Bool false) e --3> Bool (false)
  | ST_AndTrue :
  forall e e', e --3> e' -> And (Bool true) e --3> And (Bool true) e'
  where " t '--3>' t' " := (step_3 t t').

Reserved Notation " t '--3>*' t' " (at level 40).

Inductive step_3_multi : exp -> exp -> Prop :=
  | refl_3 : forall e, e --3>* e
  | multi_step_3 : forall e1 e1' e2, e1 --3> e1' -> e1' --3>* e2 -> e1 --3>* e2
  where " t '--3>*' t' " := (step_3_multi t t').
  
Lemma step_cont :
forall e e' v, e --3> e' -> e --3>* v -> e --3>* v.
Proof.
  intros.
  induction H0.
  - apply refl_3.
  - apply (multi_step_3 e1 e1' e2 H0 H1).
Qed.

Lemma Add3_Proceed :
  forall n1 n2,
  Add (Int n1) (Int n2) --3> Int (n1 + n2).
Proof.
  intros.
  apply ST_AddConst3.
Qed.

Theorem same :
  forall e v,
  e --2>* v -> e --3>* v.
Proof.
  intros.
  induction H.
  - apply refl_3.
  - induction H.
    * apply (step_cont (Add (Int n1) (Int n2)) (Int (n1 + n2)) e2 (ST_AddConst3 n1 n2) (multi_step_3 (Add (Int n1) (Int n2)) (Int (n1 + n2)) e2 (ST_AddConst3 n1 n2) IHstep_2_multi)).
    * apply (step_cont (And (Bool b1) (Bool b2)) (Bool (b1 && b2)) e2 (ST_AndBool3 b1 b2) (multi_step_3 (And (Bool b1) (Bool b2)) (Bool (b1 && b2)) e2 (ST_AndBool3 b1 b2) IHstep_2_multi)).
    * 
    