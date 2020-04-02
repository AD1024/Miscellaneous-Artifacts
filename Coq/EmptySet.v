Require Import Frap.
Require Import Coq.Lists.List.

Module TestEmptySet.
    Inductive Bot : Set :=.

    Theorem false_implies : forall (X : Type), Bot -> X.
    Proof.
        intro X.
        intro F.
        destruct F.
    Qed.

    Inductive Product : Type :=
        | Pair : Type -> Type -> Product.

    Definition proj_1 (pi : Product) : Type :=
        match pi with
            | Pair x _ => x
        end.
    
    Definition proj_2 (pi : Product) : Type :=
        match pi with
            | Pair _ y => y
        end.
    
    Record Isomorphism (A B : Type) : Type := iso {
        to : A -> B;
        from : B -> A;
        from_to : forall x, (from (to x)) = x;
        to_from : forall y, (to (from y)) = y;
    }.

    Theorem cong : 
        forall (x y : Type) (f : Type -> Type),
        x = y -> f x = f y.
    Proof.
        intros.
        rewrite H.
        reflexivity.
    Qed.

    Definition extentionality : 
        forall (f g : Type -> Type),
        forall x, f x = g x -> f = g.
    Admitted.
    
    Lemma add_nSm : 
        forall n m, n + S m = S (n + m).
    Proof.
        intros.
        induct n.
        - simpl. reflexivity.
        - simpl. rewrite IHn. reflexivity.
    Qed.