(* Copyright 1993 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Sep 22 19:43:58 PDT 1994 by heydon   *)
(*      modified on Fri Aug  5 13:38:52 PDT 1994 by detlefs  *)
(*      modified on Mon Oct 25 10:46:34 PDT 1993 by mcjones  *)
(*      modified on Thu Apr 29 15:57:27 PDT 1993 by gnelson  *)
<*PRAGMA SPEC*>

(* This interface defines the representation of generic
   sequences. *)

GENERIC INTERFACE SequenceRep(Elem, Seq);
(* Where "Seq = Sequence(Elem)". *)

REVEAL Seq.T <: Public;

TYPE
  RefArray = REF ARRAY OF Elem.T;
  Public = Seq.Public OBJECT
    elem: RefArray;
    st: CARDINAL := 0;
    sz: CARDINAL := 0
  END;

(* Element "i" of "s" is stored in

| s.elem[(s.st + i) MOD NUMBER(s.elem^)].

A sequence "s" satisfies the invariant that

| (s.elem # NIL) AND (s.sz <= NUMBER(s.elem^))
| AND (s.size() = s.sz) AND (NUMBER(s.elem^) > 0)
| AND (s.st < NUMBER(s.elem^))

*)

<*SPEC ABSTRACT Seq.Valid[t: Seq.T]:
                Seq.Valid[t] IFF 
                      t # NIL
                  AND t.elem # NIL
                  AND NUMBER(t.elem^) > 0
                  AND t.st < NUMBER(t.elem^)
                  AND t.sz <= NUMBER(t.elem^)
*>
<*SPEC DEPEND Seq.Valid[t: Seq.T]: t.st, t.sz, t.elem, RefArray *>

TYPE EArr = ARRAY OF Elem.T;

<*SPEC FUNC m(i: INTEGER, n: INTEGER): INTEGER*>
<*SPEC AXIOM (ALL [i: INTEGER, n: INTEGER] i >= n IMPLIES m(i, n) = i - n) *>
<*SPEC AXIOM (ALL [i: INTEGER, n: INTEGER] i < n IMPLIES m(i, n) = i) *>

<*SPEC FUNC Abs(elems: EArr, st: INTEGER, sz: INTEGER): SEQ[Elem.T] *>
<*SPEC AXIOM (ALL [elems: EArr, st: INTEGER, sz: INTEGER, i: INTEGER]
               Abs(elems, st, sz)[i] = elems[m((st + i), NUMBER(elems))]) *>
<*SPEC AXIOM (ALL [elems: EArr, st: INTEGER, sz: INTEGER]
               NUMBER(Abs(elems, st, sz)) = sz) *>

<*SPEC ABSTRACT Seq.Data[t: Seq.T]: Seq.Data[t] = Abs(t.elem^, t.st, t.sz) *>
<*SPEC DEPEND Seq.Data[t: Seq.T]: t.st, t.sz, t.elem, RefArray *>


END SequenceRep.





