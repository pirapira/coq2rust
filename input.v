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
Definition e : double := d1.

Inductive two_arg : Set :=
| ta : unit -> unit -> two_arg
.

Definition tv : two_arg := ta tt tt.

Definition num : nat := 2.

Definition f (d :double) :=
  match d with
  | d0 _ => tt
  | d1   => tt
  end.

Definition g (d :double) :=
  match d with
  | d0 _ => d1
  | d1   => d0 tt
  end.

Recursive Extraction n m o emp single double d e two_arg tv num f g plus xorb app Empty_set fst.
