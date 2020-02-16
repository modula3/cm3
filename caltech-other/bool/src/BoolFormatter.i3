(* $Id$ *)

INTERFACE BoolFormatter;
IMPORT Bool;

TYPE 
  T = OBJECT METHODS
    fmt(b : Bool.T) : TEXT
  END;

CONST Brand = "BoolFormatter";

END BoolFormatter.

