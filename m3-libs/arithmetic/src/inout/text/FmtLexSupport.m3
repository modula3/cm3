MODULE FmtLexSupport;
(*Copyright (c) 1996, m3na project

Abstract: 

*)

<*UNUSED*> CONST Module = "FmtLexSupport.";
(*==========================*)

PROCEDURE Parenthesize (t : TEXT; inner, outer : Precedence) : TEXT =
  BEGIN
    IF inner < outer THEN
      RETURN "\\left(" & t & "\\right)";
    ELSE
      RETURN t;
    END;
  END Parenthesize;


(*==========================*)
BEGIN
END FmtLexSupport.
