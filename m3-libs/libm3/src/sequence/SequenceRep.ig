(* Copyright 1993 Digital Equipment Corporation.             *)
(* Distributed only by permission.                           *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Fri Jan 26 13:37:56 PST 1996 by detlefs  *)
(*      modified on Thu Sep 22 19:43:58 PDT 1994 by heydon   *)
(*      modified on Mon Oct 25 10:46:34 PDT 1993 by mcjones  *)
(*      modified on Thu Apr 29 15:57:27 PDT 1993 by gnelson  *)

(* This interface defines the representation of generic
   sequences. *)

GENERIC INTERFACE SequenceRep(Elem, Seq);
(* Where "Seq = Sequence(Elem)". *)

REVEAL Seq.T <: Public;

TYPE
  RefArray = BRANDED REF ARRAY OF Elem.T;
  Public = Seq.Public BRANDED OBJECT
    elem: RefArray := NIL;
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

END SequenceRep.





