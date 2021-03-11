(* $Id$ *)

GENERIC MODULE SXRealOps(Elem);

PROCEDURE DivB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a/b END DivB;

BEGIN 
  Zero   := FLOAT( 0, Elem.Base); 
  One    := FLOAT( 1, Elem.Base); 
  NegOne := FLOAT(-1, Elem.Base)
END SXRealOps.
