(* $Id$ *)

INTERFACE SchemeJailBreak;
IMPORT SchemeObject;

TYPE
  Object = SchemeObject.T;

  T = OBJECT METHODS
    apply(args : Object) : Object;
  END;

CONST Brand = "SchemeJailBreak";

END SchemeJailBreak.
