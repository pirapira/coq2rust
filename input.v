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

Inductive even : Set :=
| O0 : even
| eo : odd -> even
with odd : Set :=
| oe : even -> odd.

Fixpoint even_to_nat e :=
  match e with
    | O0 => O
    | eo o => S (odd_to_nat o)
  end
with odd_to_nat o :=
  match o with
    | oe e => S (even_to_nat e)
  end.

Record point := mkPoint {
  x : nat ;
  y : nat
}.

Recursive Extraction n m o emp single double d e two_arg tv num f g plus xorb app Empty_set fst even even_to_nat point.
