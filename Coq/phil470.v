Inductive prop_var : Set :=
  | A | B | C.

Inductive formulae : Prop :=
  | Var (x : nat) : formulae
  | Implies : formulae -> formulae -> formulae
  | Neg : formulae -> formulae.
  
Inductive theorems : formulae -> Prop :=
  | Thm : forall x, theorems (Var x)
  | MP : forall (x y : formulae), theorems x -> theorems (Implies x y) -> theorems y
  | EImplication : forall x y, theorems (Implies x (Implies y x))
  | NegImplication : forall x y, theorems (Implies (Neg x) (Implies x y))
  | Inference1 : forall x y z, theorems (Implies (Implies x (Implies y z)) (Implies (Implies x y) (Implies x z)))
  | Inference2 : forall x y, theorems (Implies (Implies x y) (Implies (Implies (Neg x) y) y))
  | NotImplication : forall x y, theorems (Implies x (Implies (Neg y) (Neg (Implies x y)))).
  
Theorem test :
forall x, theorems (Implies x (Neg (Neg x))).
Proof.
  intros.
  econstructor.
Qed.