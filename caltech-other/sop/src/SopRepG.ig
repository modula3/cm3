(* $Id$ *)
GENERIC INTERFACE SopRepG(Sop, SopLiteral);

TYPE
  Conjunct = REF ARRAY OF SopLiteral.T;
  Rep = REF ARRAY OF Conjunct;

REVEAL Sop.T <: Private;

(* importers outside the "sop" package should treat the rep field as
   read-only. *)

TYPE Private = Sop.Public OBJECT rep : Rep; END;

END SopRepG.
