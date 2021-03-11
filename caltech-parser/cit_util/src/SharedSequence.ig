(* $Id$ *)

GENERIC INTERFACE SharedSequence(Elem, ElemSeq);

(* a memory-efficient representation of sequences that permit
   sharing of subsequences *)

TYPE
  T <: Public;

  Public = ElemSeq.T OBJECT METHODS
    init() : T;

    addSeqHi(s : ElemSeq.T);
    (* if s is changed after being added, the change will be reflected 
       in (the middle of) the new object.

       It is a checked runtime error to add a NIL s. *)

    (* all methods of the supertype are supported except fromArray *)
  END;

CONST Brand = "Shared " & ElemSeq.Brand & " of " & Elem.Brand;

END SharedSequence.
