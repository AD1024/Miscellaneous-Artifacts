Inductive Expr : Set := 
  | Lit (b : bool) : Expr
  | And : Expr -> Expr -> Expr.

Fixpoint evalExpr (e : Expr) : bool :=
  match e with
  | Lit x => x
  | And e1 e2 => (evalExpr e1) && (evalExpr e2)
  end.
  
Theorem and_comm :
  forall (x y : bool),
  andb x y = andb y x.
Proof.
  intros.
  destruct x; destruct y; simpl; auto.
Qed.
  
Theorem same :
  forall e1 e2 e3,
  evalExpr (And e1 (And e2 e3)) =
  evalExpr (And (And e1 e2) e3).
Proof.
  intros.
  induction e1.
  - simpl. destruct b; simpl; auto.
  - simpl. simpl in IHe1_2. rewrite andb_assoc.