---
title: Prefix-Free Program Codes
---

```coq
(*
In this post I'll look at programming languages from a
formal point of view. I want to highlight the connection
between strings of symbols and tree structures.

The post is written in the Coq programming/proof language,
so you can follow along at home by stepping through the
code in a Coq IDE like Proof General or Proof Web
*)

(* First let's import some useful libraries *)
Require Import Program. (* Dependent induction tactics   *)
Require Import Fin.     (* Finite sets                   *)
Require Import Mult.    (* Theorems about multiplication *)
Require Import Compare. (* Comparison of natural numbers *)
Require Import Lt.      (* Theorems about less-than      *)
Require Import Minus.   (* Theorems about subtraction    *)
Require Import NPeano.  (* Natural numbers               *)
Require Import List.    (* Tail-recursive lists          *)

(*
Languages require alphabets of distinct symbols. We can use
finite sets to represent these.
*)
Definition alphabet := t. (* The finite type from Fin *)

(*
The empty set "alphabet 0" contains no symbols.
*)
Theorem alphabet_0_empty : forall s : alphabet 0,
                                      False.
Proof.
  intros; dependent destruction s.
Qed.

(*
The symbol "F1" is a member of every non-empty alphabet.
*)
Definition alphabet_F1 (n :  nat)
                          :  alphabet (S n)
                          := F1.

(*
The function "FS" can coerce every member of "alphabet N"
to be a member of "alphabet N+1".
*)
Definition alphabet_FS (n :  nat)
                       (s :  alphabet    n)
                          :  alphabet (S n)
                          := FS s.

(*
Hence our alphabets look like this:

alphabet 0 = {                     }
alphabet 1 = {       F1            }
alphabet 2 = {    FS F1,     F1    }
alphabet 3 = {FS (FS F1), FS F1, F1}
...
*)

(*
When we're proving things about languages, we'll often have
to prove the case for alphabet 0. Coq isn't smart enough to
notice that it's an empty type, so we define this absurdity
to fulfil whatever impossible goal Coq has set us.
*)
Definition absurd : forall (a : Set)
                           (s : alphabet 0),
                            a.
  intros; dependent destruction s.
Defined.
Arguments absurd [a] s.

(*
A code is a list of symbols in some alphabet. We use
tail-recursive (singly-linked) lists which can be 'nil'
(empty) or 'cons x y' (element x followed by list y).
*)
Definition code (n : nat) := list (alphabet n).

(*
We can represent symbols from an alphabet 'a' as codes in
an alphabet 'b'. We call such representations
'Encoding a b e d', where 'e' and 'd' are the encoding and
decoding functions respectively. To construct an
'Encoding a b e d' we must also supply a proof that
encoding then decoding a symbol gives us back the same
symbol.
*)
Inductive Encoding (a b :  nat)
                   (  e :  alphabet a -> code     b)
                   (  d :  code     b -> alphabet a)
                        :  Set
       := encoding      : (forall s : alphabet a,
                                  s = d (e s))
                        -> Encoding a b e d.
Arguments encoding [a b] e d [_].

(*
If we have an 'Encoding a b e d' then we can turn any
'code a' into a 'code b' by encoding each symbol then
concatenating the results.
*)
Fixpoint encode (a b : nat                     )
                (  e : alphabet a -> code     b)
                (  d : code     b -> alphabet a)
                (  f : Encoding a b e d        )
                (  l : code     a              )
                     : code     b              :=
         match l with
             | nil       => nil
             | cons x l' => e x ++ encode a b e d f l'
         end.
Arguments encode [a b e d] f l.

(*
Concatenating encoded symbols may introduce ambiguity; we
can't, in general, work out where to split a code to get
back the original parts.

We can demonstrate this with a simple unary encoding which
just counts the number of 'FS' wrappers and makes a code
containing that many 'F1' symbols:

F1         <-> []
FS F1      <-> [F1]
FS (FS F1) <-> [F1; F1]
etc.

This encoding can handle input alphabets of arbitrary size
and outputs codes in any non-empty alphabet (ie. it must
contain F1).
*)

(*
First we define the full encoding and decoding functions.
*)
Fixpoint unary_enc' (a b : nat)
                    (  s : alphabet a)
                         : code (S b)
      := match s with
             | F1 _     => []
             | FS a' s' => cons F1 (unary_enc' a' b s')
         end.

Fixpoint unary_dec' (a b : nat   )
                    (  c : code b)
                         : alphabet (S a)
      := match c with
             | nil       => F1
             | cons x xs =>
         match a with
             | 0    => F1 (* Invalid input *)
             | S a' => FS (unary_dec' a' b xs)
         end end.

(*
Now we define wrappers which have a nicer interface.
*)
Definition unary_enc (a b : nat) := unary_enc' a b.
Definition unary_dec (a b : nat) := unary_dec' a b.
Arguments  unary_enc [a b] s.
Arguments  unary_dec [a b] c.

(*
Next we construct a proof that unary_dec will decode
anything that unary_enc gives out.
*)
Lemma unary_nil : forall a b :  nat,

                     let   c :  code a
                             := []

                  in let   s :  alphabet (S b)
                             := F1

                      in   s  = unary_dec c.
Proof.
  intros; dependent induction a;
  [ dependent induction b;
    [ compute; trivial
    | compute; trivial ]
  | dependent destruction b;
    [ trivial
    | trivial ]].
Qed.

Theorem unary_inv : forall a b :  nat,

                    forall   s :  alphabet (S a),

                       let   c :  code (S b)
                               := unary_enc s

                        in   s  = unary_dec c.
Proof.
  intros; unfold c; dependent induction s;
  [ unfold unary_enc; rewrite <- unary_nil; trivial
  | destruct a;
    [ apply (absurd s)
    | simpl; rewrite <- (IHs a s); tauto; tauto; tauto ]].
Qed.

(*
With our proof in hand, we can construct a unary encoding.
Note the 'S a' and 'S b', since we can't create codes or
symbols from 'alphabet 0'.
*)
Definition unary_encoding (a b : nat)
                               : let e := unary_enc' (S a)
                                                        b

                              in let d := unary_dec'    a
                                                     (S b)

                                  in Encoding (S a)
                                              (S b)
                                                 e
                                                 d

        := encoding (unary_enc' (S a)    b)
                    (unary_dec'    a  (S b))
                    (unary_inv     a     b).

(*
The encodings we care about are 'prefix-free'. A prefix-
free encoding function will never output a code which
begins with another code. For example if a prefix-free
encoding produces the code [F1; FS (FS F1); FS F1] for some
input x, it will never produce [], [F1] or [F1; FS (FS F1)]
for any input, since they're prefixes of the encoding of x.
*)

(*
To show that an encoding is prefix-free, we need a way to
find prefixes.
*)
Fixpoint prefixes' (a : Set)
                   (l : list a)
                      : list (list a)
      := match l with
             | nil       => [nil]
             | cons x xs => cons [x] (map (cons x)
                                          (prefixes' a xs))
         end.

Definition prefixes (a : Set)
                    (l : list a)
                       : list (list a)
        := cons [] (removelast
                   (removelast (prefixes' a l))).

Arguments  prefixes [a] l.

Theorem prefixes_cons : forall (a : Set)
                               (l : list a),
                                    prefixes l <> [].
Proof.
  intros; destruct l;
  [ compute; intuition; dependent destruction H
  | compute; intuition; dependent destruction H ].
Qed.

Theorem prefix_nil : forall (a : Set)
                            (l : list a),
                                 In [] (prefixes l).
Proof.
  intros; induction l;
    compute; tauto;
    unfold prefixes.
Qed.

(*
Now we can make a type for prefix-free encoding functions.
*)
Inductive PrefixFree (a b : nat)
                     (enc : alphabet a -> code b)
                          : Prop

          (* 'enc t' is not in 'prefixes (enc s)' *)
       := pf : forall p : (forall s t :  alphabet a,
                              let  et := enc t
                           in let  ps := prefixes (enc s)
                               in        not (In et ps)),
                           PrefixFree a b enc.

(*
A prefix-free code is an 'Encoding a b e d' where e is also
PrefixFree.
*)
Inductive PrefixFreeCode (a b : nat)
                         (e   : alphabet a -> code b)
                         (d   : code b -> alphabet a)
                              : Set
       := pfcode : Encoding       a b e d ->
                   PrefixFree     a b e   ->
                   PrefixFreeCode a b e d.

(*
Now we can show that our unary encoding is not prefix-free.
We use the following counter-example to show that unary
encodings are ambiguous:

(unary_enc F1) ++ (unary_enc F1) =
           []  ++            []  =
                             []  =
                   unary_enc F1  =
*)
Theorem unary_not_pf : forall a b : nat,

                          let e := unary_enc' (S a)
                                                 b

                           in not (PrefixFree (S a)
                                              (S b)
                                                 e).
Proof.
  unfold not; intros; destruct H;
  assert (p' := p F1 F1); simpl;
  assert (unary_enc' (S a) b F1 = []); trivial;
  assert (In (unary_enc' (S a)
                            b
                           F1)
             (prefixes (unary_enc' (S a)
                                      b
                                     F1)));
  compute; auto; auto.
Qed.

(*
Now we'll introduce trees. 'Tree n' is the most direct
representationan of an n-ary tree; it has empty leaves and
nodes containing a vector of n other n-ary trees. For
example, consider the following 3-ary tree:

  <----------Trees--------->

                    +-- leaf    ^
                    |           |
                    |    ::     |
                    |           |
         +-- node --+-- leaf    |
         |          |           |
         |          |    ::     |
         |    ::    |           V
         |          +-- leaf    e
         |                      c
  node --+-- leaf        ::     t
         |                      o
         |               []     r
         |    ::                s
         |                      |
         |                      |
         +-- leaf               |
                                |
              ::                |
                                |
              []                v
*)

Require Import Vector.
Definition Vec := Vector.t. (* Vector types *)

Inductive Tree (n : nat) :=
        | leaf    :                   Tree n
        | node    : Vec (Tree n) n -> Tree n.

(*
We can also represent trees is a less direct way by storing
paths from the root to each leaf. The leaves, nodes and
branches are all implicit: the elements in a path are the
choices we make about which branch to follow next. A choice
implies the existence of a node; no choice (the end of the
path) implies the existence of a leaf at that point.

Choices are represented using a finite set with the same
arity as the tree. Paths are plain lists.
*)
Definition choice := Fin.t.
Definition path (n : nat) := list (choice n).

(*
To form a tree we need a complete list of mutually-
compatible paths. Paths are compatible iff they agree on
the kind of data (node or leaf) in their common sections.

Since a tree's branches never merge, once two paths have
diverged they will never re-converge. This means that any
sections they have in common must start at the root.

In the 'divergeNow' case the two paths fork away from each
other immediately. Since they have no further nodes in
common, they can't disagree about anything and are
therefore compatible. For example:

                                       A
    A         B                        B
    +--                                +--
    |                                  |
  --+   and --+   represent the tree --+
              |                        |
              +--                      +--

In the 'divergeLater' case, we can prepend anything to a
pair of compatible paths knowing that they'll still diverge
eventually. For example:

                                             C++A
                                             C++B
                                                +--
                                                |
    A         B                  C           +--+
    +--                          +--         |  |
    |                            |           |  +--
  --+   and --+   prepended by --+   gives --+
              |
              +--

As their names suggest, divergeNow and divergeLater force
compatible paths to diverge at some point. This means that
no path is compatible with itself or with a section of
itself. This prevents compatible paths from defining a leaf
(nil) and a node (choice) at the same point in the tree.

For example:

    A  B   A  B
    +--    +--+
    |      |  |
    |      |  +--
  --+    --+

The first path implies a leaf at B, but the second path
implies a node. We can't make these paths compatible using
divergeNow or divergeLater, since they don't diverge!
*)
Inductive Compatible (n : nat)
                        : forall a b : path n,
                                 Set :=

        | divergeNow    : forall (a b : choice n)
                                 (x y : path   n)
                                 (p   : a  <>  b),
                                 Compatible    n (a :: x)
                                                 (b :: y)

        | divergeLater  : forall (a   : choice     n)
                                 (x y : path       n)
                                 (c   : Compatible n x
                                                     y),
                                  Compatible n (a :: x)
                                               (a :: y).

(*
If a list of paths are mutually-compatible we should be
able to show that any two are compatible.
*)
Inductive MutuallyCompatible (n m : nat)
                             (v   : Vec (path n) m)
                          : Prop

       := vecMC : forall (p : forall (i j : Fin.t m)
                                     (ne  : i <> j),
                                     Compatible  n
                                                (nth v i)
                                                (nth v j)),
                         MutuallyCompatible n m v.

(*
A list of paths is complete when every choice eventually
leads to a leaf.
*)

Inductive Complete (n : nat)
                      : forall (m : nat)
                               (v : Vec (path n) m),
                               Prop

          (* A leaf is trivially a complete tree *)
       := cNil  : Complete n
                           1
                           (Vector.cons (path n)
                                        []
                                        0
                                        (nil (path n)))

        | cCons : forall (* A Vec of paths is complete   *)
                         (m : nat)
                         (v : Vec (path n) m)

                         (* if every branch              *)
                         (b : (forall (c : choice n),
                                      Vec (path n) _))

                         (* in the Vec                   *)
                         (_ : forall (c : choice n)
                                     (p : path   n)
                                     (_ : In p (b c)),
                                     In (c :: p) v)

                         (* is complete,                 *)
                         (_ : (forall (c : choice n),
                                      Complete n m (b c)))

                         (* and that v is not empty      *)
                         (_ : m <> 0),

                              Complete n m v.

(*
Now we'll prove that complete lists of mutually-compatible
paths are isomorphic to trees.
*)
Theorem path_nil : forall (n m : nat)
                          (v : Vec (path n) m)
                          (_ : MutuallyCompatible n m v)
                          (_ : Complete n m v),
                          m <> 0.
Proof.
  intros; intuition; dependent destruction H0;
  [ dependent destruction H1
  | tauto ].
Qed.

Fixpoint choices (n : nat) : Vec (choice n) n.
Proof.
  destruct n;
  [ exact (nil (choice 0))
  | exact (cons (choice (S n)) F1 n
                (map FS (choices n))) ].
Defined.

(*
Theorem map_f (a b : Type)
              (x   : a)
              (n   : nat)
              (l   : Vec a n)
              (f   : a -> b)
              (i   : In x l)
                   : In (f x) (map f l).
Proof.
  destruct i. simpl. constructor.
  *)

Theorem find_choice (n : nat)
                    (c : choice (S n))
                       : In c (choices (S n)).
Proof.
  induction c;
  [ simpl; constructor
  | simpl; constructor; induction IHc;
    [ simpl; constructor
    | simpl; constructor; trivial ]].
Qed.

Theorem complete_nil : forall (n m : nat)
                              (v : Vec (path n) m)
                              (_ : Complete n m v),
                              m <> 0.
Proof.
  intros.
  dependent destruction H.
  intuition. dependent destruction H.
  trivial.
Qed.

Definition empty_paths (n : nat)
                       (p : list (path n))
                       (_ : p = [[]])
                          : (Tree n)
                          := leaf n.

Fixpoint paths_to_tree (n m : nat)
                       (v   : Vec (path n) m)
                            : (MutuallyCompatible n m v)
                              -> (Complete n m v)
                              -> (Tree n).
Proof.
  (* Grab some variables *)
  intros. assert (c := choices n).

  (* Tree 0 must be a leaf *)
  destruct n. exact (leaf 0).

  (* Tree 1 must be a leaf *)
  destruct n. exact (leaf 1).

  (* v can't be empty so discharge that case with ne *)
  destruct m;
  [ destruct (complete_nil (S (S n)) 0 v H0); trivial
  | ].

  (* If v is [[]] then we have a leaf *)
  dependent destruction v.
  destruct h. exact (leaf (S (S n))).

  (* Otherwise v is a node with S (S n) branches *)
  apply node. apply (cons).

paths_to_tree : forall (n m : nat) (v : Vec (path n) m),
                MutuallyCompatible n m v -> Complete n m v -> Tree n
n : nat
h : path (S (S n))
n0 : nat
v : t (path (S (S n))) n0
H : MutuallyCompatible (S (S n)) (S n0) (cons (path (S (S n))) h n0 v)
H0 : Complete (S (S n)) (S n0) (cons (path (S (S n))) h n0 v)
c : Vec (choice (S (S n))) (S (S n))
ne : S n0 <> 0



  (* Nothing is compatible with [] so ~ In [] p *)
  assert (~ List.In [] ([] :: p :: p0)).
    intuition. destruct H1.
  dependent destruction x.
          assert (a := p1 (F1 : Fin.t m) (FS F1)).
          replace ([] :: p :: p0) with (to_list v).
          rewrite <- x. leaf (S (S n))).

  (* The paths in p cannot be empty *)
  assert (forall q : path (S (S n)),
                 (List.In q ((a :: a0) :: p)) -> q <> []).
    intros. destruct H1.
      rewrite <- H1. intuition.
        dependent destruction H2.
      dependent destruction H0.
        assert (to_list v0 = p).
          dependent destruction v0.
            compute.


(*
The first result we'll show is that the binary alphabet is
isomorphic to the booleans.
*)
Definition bit : Set := alphabet 2.
Definition on  : bit := F1.     (* chosen      *)
Definition off : bit := FS F1.  (* arbitrarily *)

Definition bool_to_bit (b : bool) : bit :=
           match b with
               | true  => on
               | false => off
           end.

Definition bit_to_bool (b : bit) : bool :=
           match b with
               | F1 _   => true
               | FS _ _ => false
           end.

Theorem bool_bit_iso : forall b : bool,
                              bit_to_bool
                              (bool_to_bit b) = b.
  intros; compute; induction b;
  [ reflexivity
  | reflexivity ].
Qed.

Theorem bit_bool_iso : forall b : bit,
                              bool_to_bit
                              (bit_to_bool b) = b.
  cbv delta; intros; dependent induction b;
  [ trivial
  | dependent induction b;
    [ reflexivity
    | assert (H : t 0 -> FS F1 = FS (FS b));
      apply case0; exact b]].
Qed.
```
