INTERFACE FmtLexSupport;
(*Copyright (c) 1996, m3na project

Abstract: Support for FmtLex type modules

*)

(*==========================*)

TYPE
  Precedence = {sum, product, power};

PROCEDURE Parenthesize (t : TEXT; inner, outer : Precedence) : TEXT;

(*==========================*)
END FmtLexSupport.
