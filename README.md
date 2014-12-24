coq2rust

See input.v for examples of extracted Coq terms.

try
$ ./configure -local
$ ./compile.sh
at the directory containing this file.  After this, test.rs will
contain the extracted code like this:


```
enum Empty_set<> {
 
}


enum Unit<> {
 Tt
}


enum Bool<> {
 True,
 False
}


fn xorb(b1: Bool, b2: Bool) -> Bool {
  match b1 {
  Bool::True =>
    (match b2 {
     Bool::True => Bool::False,
     Bool::False => Bool::True
     }),
  Bool::False => b2
  }
}

enum Nat<> {
 O,
 S(Box<Nat>)
}


enum Prod< a, b> {
 Pair(Box<a>, Box<b>)
}


fn fst<p,a2>(p: Prod<p,a2>) -> p { match p {
Prod::Pair(box x,box y) => x
} }

enum List< a> {
 Nil,
 Cons(Box<a>, Box<(List<a>)>)
}


fn app<m0>(l: List<m0>, m0: List<m0>) -> List<m0> {
  match l {
  List::Nil => m0,
  List::Cons(box a,box l1) => List::Cons((box () a), (box () (app (l1,m0))))
  }
}

fn add(n0: Nat, m0: Nat) -> Nat {
  match n0 {
  Nat::O => m0,
  Nat::S(box p) => Nat::S((box () (add (p,m0))))
  }
}

fn n() -> Unit {
  Unit::Tt
}

fn m() -> Bool {
  Bool::True
}

enum Emp<> {
 
}


type Single =
  Unit;
  // singleton inductive, whose constructor was s
  
fn o() -> Single {
  Unit::Tt
}

enum Double<> {
 D0(Box<Unit>),
 D1
}


fn d() -> Double {
  Double::D0((box () Unit::Tt))
}

fn e() -> Double {
  Double::D1
}

enum Two_arg<> {
 Ta(Box<Unit>, Box<Unit>)
}


fn tv() -> Two_arg {
  Two_arg::Ta((box () Unit::Tt), (box () Unit::Tt))
}

fn num() -> Nat {
  Nat::S((box () (Nat::S((box () Nat::O)))))
}

fn f(d0: Double) -> Unit {
  Unit::Tt
}

fn g(d0: Double) -> Double { match d0 {
Double::D0(box u) => Double::D1,
Double::D1 => Double::D0((box () Unit::Tt))
} }

enum Even<> {
 O',
 Eo(Box<Odd>)
}

enum Odd<> {
 Oe(Box<Even>)
}
```


Below is the original Coq README


               	         THE COQ V8 SYSTEM
            	         =================

INSTALLATION.
=============

   See the file INSTALL for installation procedure.


DOCUMENTATION.
==============

   The documentation is part of the archive in directory doc. The
   documentation of the last released version is available on the Coq
   web site at http://coq.inria.fr/doc.


CHANGES.
========

   There is a file named CHANGES that explains the differences and the
   incompatibilities since last versions. If you upgrade Coq, please read
   it carefully.


AVAILABILITY.
=============

   Coq is available from http://coq.inria.fr.


THE COQ CLUB.
=============

   The Coq Club moderated mailing list is meant to be a standard way
   to discuss questions about the Coq system and related topics. The
   subscription link can be found at http://coq.inria.fr/community.

   The topics to be discussed in the club should include:

     * technical problems;

     * questions about proof developments;

     * suggestions and questions about the implementation;

     * announcements of proofs;

     * theoretical questions about typed lambda-calculi which are
       closely related to Coq.

   For any questions/suggestions about the Coq Club, please write to
   coq-club-request@inria.fr.


BUGS REPORT.
============

   Send your bug reports by filling a form at

        http://coq.inria.fr/bugs

   To be effective, bug reports should mention the Caml version used
   to compile and run Coq, the Coq version (coqtop -v), the configuration
   used, and include a complete source example leading to the bug.
