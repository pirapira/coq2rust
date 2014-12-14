Extraction Language Rust.

Definition n : unit := tt.

Definition m : bool := true.

Inductive emp : Set := .

Inductive single : Set :=
| s : unit -> single
.

Definition o : single := s tt.

Inductive double : Set :=
| d0 : unit -> double
| d1 : double
.

Definition d : double := d0 tt.

Recursive Extraction n m o emp single double d.
