INTERFACE Keyword;

IMPORT Scanner;

PROCEDURE Classify (READONLY x: ARRAY OF CHAR): Scanner.TK;
(* If "x" is a json keyword, return its token class,
   otherwise return Scanner.TK_Ident.  It is a checked runtime
   error if NUMBER(x) is less than one. *)

END Keyword.
