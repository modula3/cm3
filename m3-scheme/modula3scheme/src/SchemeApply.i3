(* $Id$ *)

INTERFACE SchemeApply;
(* apply a Scheme procedure (lambda) in a given interpreter's global
   environment *)

IMPORT Scheme, SchemeObject;

PROCEDURE OneArg(interp : Scheme.T; 
                 closure : SchemeObject.T; 
                 arg : SchemeObject.T) : SchemeObject.T RAISES { Scheme.E };

CONST Brand = "SchemeApply";

END SchemeApply.
