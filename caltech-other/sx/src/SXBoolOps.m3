(* $Id$ *)

MODULE SXBoolOps;
IMPORT SXBool AS Bool;
IMPORT SXBool_BoolFuncOps;

PROCEDURE NotB(a : BOOLEAN) : BOOLEAN =
  BEGIN RETURN NOT a END NotB;

PROCEDURE AndB  (a, b : BOOLEAN) : BOOLEAN =
  BEGIN RETURN a AND b END AndB;

PROCEDURE OrB   (a, b : BOOLEAN) : BOOLEAN =
  BEGIN RETURN a OR b END OrB;

PROCEDURE XorB  (a, b : BOOLEAN) : BOOLEAN =
  BEGIN RETURN a # b END XorB;

PROCEDURE EqualB  (a, b : BOOLEAN) : BOOLEAN =
  BEGIN RETURN a = b END EqualB;

(**********************************************************************)

PROCEDURE Not(a : Bool.T) : Bool.T =
  BEGIN RETURN SXBool_BoolFuncOps.UnaryFunc(a, NotB,"Not") END Not;

PROCEDURE Equal(a, b : Bool.T) : Bool.T =
  BEGIN RETURN SXBool_BoolFuncOps.BinaryFunc(a, b, EqualB,"Equal") END Equal;

PROCEDURE And  (a, b : Bool.T; ss : BOOLEAN) : Bool.T =
  BEGIN 
    IF ss THEN
      RETURN SXBool_BoolFuncOps.BinarySymmetricShortCircuitFunc(a, b, AndB,
                                                                FALSE,
                                                                FALSE,
                                                                "And") 
    ELSE
      RETURN SXBool_BoolFuncOps.BinaryFunc(a, b, AndB,"And") 
    END
  END And;

PROCEDURE Or   (a, b : Bool.T; ss : BOOLEAN) : Bool.T =
  BEGIN 
    IF ss THEN
      RETURN SXBool_BoolFuncOps.BinarySymmetricShortCircuitFunc(a, b, OrB,
                                                                TRUE,
                                                                TRUE,
                                                                "Or") 
    ELSE
      RETURN SXBool_BoolFuncOps.BinaryFunc(a, b, OrB,"Or") 
    END
  END Or;

PROCEDURE Xor  (a, b : Bool.T) : Bool.T =
  BEGIN RETURN SXBool_BoolFuncOps.BinaryFunc(a, b, XorB,"Xor") END Xor;

BEGIN END SXBoolOps.
