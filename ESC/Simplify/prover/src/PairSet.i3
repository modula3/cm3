(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1996 by Digital Equipment Corp. *)

INTERFACE PairSet;

(* A "PairSet.T" represents a set of pairs of integers.  A "PairSet.Sparse"
   is a sparse representation of the same thing.  Both representations
   are approximate: a membership test will occasionally produce false 
   positives, but never a false negative. *)

(* This is an internal interface of the Simplify theorem-prover for
   the Extended Static Checker; to improve performance, some types
   that are logically opaque are not made actually opaque.  These
   logically opaque declarations are collected at the end of the
   interface; they include the declaration of the main type "T". *)

IMPORT IdSet;

PROCEDURE MakeEmpty(VAR t: T);
(* Equivalent to "t := Empty()". *)

PROCEDURE IsEmpty(READONLY t: T): BOOLEAN;
(* Return "t = Empty()". *)

PROCEDURE AddPairD(VAR ps: T; i,j: INTEGER);
(* "ps := Union(ps, {(i,j)})". *)

PROCEDURE Size(READONLY t: T): CARDINAL;
(* Return (an approximation of) the number of elements of "t". The answer
   can be too small, but for small sets, the answer is likely to be reasonably
   accurate.  No attempt has been made to make this procedure fast, since it
   is called from statistics code only. *)

PROCEDURE SparseSize(t: Sparse): CARDINAL;
(* Like "Size", only works for the sparse representation. *)

PROCEDURE UnionD(VAR ps1: T; READONLY ps2: T);
(* "ps1 := Union(ps1, ps2)". *)

PROCEDURE UnionSparseD(VAR ps1: T; ps2: Sparse);
(* Like "UnionD", only takes a "Sparse" as second argument. *)

PROCEDURE AddSetCrossSetD(VAR ps: T; s1, s2: IdSet.T);
(* "ps := Union(ps, CrossProduct(s1, s2))". *)

PROCEDURE AddSetCrossElemD(VAR ps: T; s: IdSet.T; id: INTEGER);
(* "ps := Union(ps, CrossProduct(s, {id}))". *)

PROCEDURE AddElemCrossSetD(VAR ps: T; id: INTEGER; s: IdSet.T);
(* "ps := Union(ps, CrossProduct({id}, s))". *)

PROCEDURE Overlap(s1: Sparse; READONLY s2: T): BOOLEAN;
(* Return whether "s1" and "s2" have any pairs in common.  False positives
   are possible.  *)

PROCEDURE SymClose(VAR ps: T);
(* "ps := Union(ps, {(i,j) | (j, i) IN ps})".  That is, "ps" becomes
   its symmetric closure. *)

PROCEDURE ToSparse(READONLY ps: T): Sparse;
(* Return "ps" in sparse form. *)

PROCEDURE Singleton(i: INTEGER): IdSet.T;
(* Return "{i MOD N}". *)

(* Please ignore the rest of the interface, which is logically a part of
   the implementation. *)

CONST 
  N = BITSIZE(INTEGER);

TYPE
  T = ARRAY [0..N-1] OF IdSet.T;
  Sparse = REF ARRAY OF SparseElem;
  SparseElem = RECORD i: INTEGER; bits: IdSet.T END;

(* For each "(i,j)" in the set represented by t:T, "(j MOD N)" is in
   "t[i MOD N]".  For each "(i,j)" in the set represented by
   "sp:Sparse", there is one "k" such that "sp[k].i = (i MOD N)" and
   "(j MOD N)" is in "sp[k].bits".  The elements of "sp" are sorted by
   their ".i" field. *)

CONST
  Empty = T{IdSet.Empty, ..};

VAR (*CONST*)
  EmptySparse: Sparse;

END PairSet.
