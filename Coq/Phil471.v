Definition f0 (m x : nat) := S m.

Fixpoint f1 (m x : nat) : nat :=
match m with
| O => x
| S m => f0 (f1 m x) x
end.

Fixpoint f2 (m x : nat) : nat :=
match m with
| O => 0
| S m => f1 (f2 m x) x
end.

Fixpoint f3 (m x : nat) : nat :=
match m with
| O => 1
| S m => f2 (f3 m x) x
end.

Fixpoint f4 (m x : nat) : nat :=
match m with
| O => 1
| S m => f3 (f4 m x) x
end.

Fixpoint f5 (m x : nat) : nat :=
match m with
| O => 1
| S m => f4 (f5 m x) x
end.

Compute (f4 0 2).
Compute (f4 1 2).
Compute (f4 2 2).
Compute (f4 3 2).
Compute (f4 4 2).
(* x ^ ((x ^ m) *)
(* Compute (f5 3 2). *)
(* Compute (f4 4 2). *)