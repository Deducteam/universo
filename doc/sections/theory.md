Theory file
===========

The theory file used by Universo should have the format described below. It is already the case if you your files come from Matita using Krajono or Coq using CoqInE:

## The public signature

The public signature should include these constants

```dedukti
(;-----------------------------------------------------------------;)
(;----------------------  PUBLIC DEFINITIONS  ---------------------;)
(;-----------------------------------------------------------------;)

(;---------------------------  Sorts  -----------------------------;)

Sort : Type.
sinf : Sort.


(;---------------------  Types and terms  -------------------------;)

(; Type for encoded type codes ;)
Univ : s : Sort -> Type.

(; Type decoding function ;)
def Term : s : Sort -> a : Univ s -> Type.


(;------------------------  CTS predicates  -----------------------;)

Bool : Type.
eps : Bool -> Type.
true : Bool.
I : eps true.

def Axiom : Sort -> Sort -> Bool.
def Rule  : Sort -> Sort -> Sort -> Bool.
def Cumul : Sort -> Sort -> Bool.

def sup   : Sort -> Sort -> Sort.


(;--------------------  Term public constructors  -----------------;)

def univ : s : Sort -> s' : Sort ->
           p : eps (Axiom s s') -> Univ s'.

def prod : s1 : Sort -> s2 : Sort -> s3 : Sort ->
           p : eps (Rule s1 s2 s3) ->
           a : Univ s1 -> b : (Term s1 a -> Univ s2) -> Univ s3.

def SubType : s : Sort -> s' : Sort -> Univ s -> Univ s' -> Bool.

def cast : s : Sort -> s' : Sort ->
           a : Univ s -> b  : Univ s' ->
           p : eps (SubType s s' a b) ->
           Term s a -> Term s' b.
```

## The private signature

The private signature should include these rewrite rules (notice that subtyping cannot be contravariant on product, otherwise Universo might crash on inductive types):

``` dedukti
(;-----------------------------------------------------------------;)
(;---------------------  PRIVATE DEFINITIONS  ---------------------;)
(;-----------------------------------------------------------------;)

(;----------------------------  Axiom  ----------------------------;)

univ' (s : Sort) (s' : Sort) : Univ s'.

[s] Term _ (univ' s _) --> Univ s.

[s,s',p] univ s s' p --> univ' s s'.


(;----------------------------  Rule  -----------------------------;)

def prod' : s1 : Sort -> s2 : Sort -> s3 : Sort ->
            a : Univ s1 -> b : (Term s1 a -> Univ s2) -> Univ s3.

[s1, s2, a, b] Term _ (prod' s1 s2 _ a b) --> x : Term s1 a -> Term s2 (b x).

[s1, s2,s3,p] prod s1 s2 s3 p --> prod' s1 s2 s3.


(;-------------------------  Subtyping ----------------------------;)

[s1, s2]           SubType _  _  (univ' s1 _)     (univ' s2 _ )        --> Cumul s1 s2
[s1,s2,s2',a,b,b'] SubType _  _ (prod' s1 s2 _ a b) (prod' _ s2' _ a b') -->
                  forall s1 a (x => SubType s2 s2' (b x) (b' x)).
[a]          SubType _ _ a a --> true.

(;---------------------------  Casts  -----------------------------;)

def cast' : s : Sort -> s' : Sort ->
            a : Univ s -> b : Univ s' ->
            Term s a -> Term s' b.

[s, a] Term _ (cast' _ _ (univ' s _) _ a) --> Term s a.

[s1,s2,a,b,t] cast s1 s2 a b _ t --> cast' s1 s2 a b t.

(;---------------------  Canonicity rules -------------------------;)

[A,t] cast' _ _ A A t --> t.

[s, s', a, c, t]
  cast' _ s' _ c (cast' s _ a _ t) -->
  cast' s s' a c t.

[s1,s2,s3, a, b]
  prod' _ s2 s3 (cast' _ _ (univ' s1 _) (univ' _ _) a) b -->
  prod' s1 s2 s3 a b.

[s1, s2, s3, a, b]
  prod' s1 _  s3 a (x => cast' _ _ (univ' s2 _) (univ' _ _) (b x)) -->
  prod' s1 s2 s3 a (x => b x).

[s1,s2,s3,s4,a,b]
  cast' _ _ (univ' _ _) (univ' s4 _) (prod' s1 s2 s3 a b) --> prod' s1 s2 s4 a b.

[s1,s2,s3,A,B,C,b]
  cast' _ _ (prod' s1 s2 _ A B) (prod' {s1} s3 _ {A} C) (x => b x) -->
  x : Term s1 A => cast' s2 s3 (B x) (C x) (b x).

[s1,s2,s3,A,B,C,b,a]
  cast' _ _ (prod' s1 s2 _ A B) (prod' {s1} s3 _ {A} C) b a -->
  cast' s2 s3 (B a) (C a) (b a).

[s1,s2,A,B,a] cast' _ s2 (cast' _ _ (univ' s1 _) _ A) B a --> cast' s1 s2 A B a.

[s1,s2,A,B,a] cast' s1 _ A (cast' _ _ (univ' s2 _) _ B) a --> cast' s1 s2 A B a.
```
