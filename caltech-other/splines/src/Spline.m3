(* $Id$ *)

MODULE Spline;
IMPORT DerivRidders;

REVEAL 
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    deriv := Deriv
  END;

  
PROCEDURE Deriv(self : T; at : LONGREAL) : LONGREAL =
  VAR
    dummyErr : LONGREAL;
  BEGIN
    (* 1.0d0 is just a guess. *)
    RETURN DerivRidders.Deriv(self, at, 1.0d0, dummyErr)
  END Deriv;
  
BEGIN END Spline.
