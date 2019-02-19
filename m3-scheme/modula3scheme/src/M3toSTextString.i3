(* $Id$ *)

INTERFACE M3toSTextString;
IMPORT SchemeString, Scheme;

(* interface for M3 <--> Scheme converting generics *)

TYPE S = SchemeString.T;
TYPE T = TEXT;

CONST ToScheme = SchemeString.FromText;

PROCEDURE FromScheme(s : S) : T RAISES { Scheme.E };

CONST Brand = "M3toSTextString";

END M3toSTextString.
