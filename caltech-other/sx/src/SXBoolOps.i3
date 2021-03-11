(* $Id$ *)

INTERFACE SXBoolOps;
IMPORT SXBool AS Bool;

PROCEDURE Not(a : Bool.T) : Bool.T;

PROCEDURE Equal(a, b : Bool.T) : Bool.T;
PROCEDURE And  (a, b : Bool.T; shortCircuit := TRUE) : Bool.T;
PROCEDURE Or   (a, b : Bool.T; shortCircuit := TRUE) : Bool.T;
PROCEDURE Xor  (a, b : Bool.T) : Bool.T;

END SXBoolOps.
