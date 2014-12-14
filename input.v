Extraction Language Rust.

Definition n : unit := tt.

Definition m : bool := true.

Inductive emp : Set := .

Inductive single : Set :=
| s : unit -> single
.

Definition o : single := s tt.

Recursive Extraction n m o emp single.
