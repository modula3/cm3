INTERFACE JsonKeyword;

IMPORT JsonScanner;

PROCEDURE Classify (READONLY x: ARRAY OF CHAR): JsonScanner.TK;
(* If "x" is a json keyword, return its token class,
   otherwise return Scanner.TK_Ident.  It is a checked runtime
   error if NUMBER(x) is less than one. *)

END JsonKeyword.
