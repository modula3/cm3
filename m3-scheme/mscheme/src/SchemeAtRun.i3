(* $Id$ *)

INTERFACE SchemeAtRun;
IMPORT SchemePrimitive;

CONST Brand = "SchemeAtRun";

PROCEDURE Extend(definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner;

END SchemeAtRun.
