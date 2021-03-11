(* $Id$ *)

GENERIC MODULE SXIntOps(Elem);

PROCEDURE DivB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a DIV b END DivB;

BEGIN 
  Zero   :=  0; 
  One    :=  1;
  NegOne := -1
END SXIntOps.
