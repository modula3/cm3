(* $Id$ *)

INTERFACE BoolRemapImpl;
IMPORT Bool, BoolBoolTbl;

PROCEDURE Remap(map : BoolBoolTbl.T; e : Bool.T; check := FALSE) : Bool.T;

END BoolRemapImpl.
