(* Copyright 1993 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Sep 22 19:43:58 PDT 1994 by heydon   *)
(*      modified on Fri Aug  5 13:38:52 PDT 1994 by detlefs  *)
(*      modified on Mon Oct 25 10:46:34 PDT 1993 by mcjones  *)
(*      modified on Thu Apr 29 15:57:27 PDT 1993 by gnelson  *)

(* This interface defines the representation of generic
   sequences. *)

GENERIC INTERFACE ShSequenceRep(Elem, Seq);
(* Where "Seq = ShSequence(Elem)". *)

REVEAL Seq.S <: Public;

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

TYPE EArr = ARRAY OF Elem.T;

END ShSequenceRep.
