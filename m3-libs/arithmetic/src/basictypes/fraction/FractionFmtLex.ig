GENERIC INTERFACE FractionFmtLex(C);
(*Copyright (c) 1996, m3na project
  
Abstract: Formatting and parsing fraction numbers

1/1/96  Harry George    Initial version
2/17/96 Harry George    Converted from Objects to ADT's
3/23/96 Harry George    Incorporated Warren Smith's implementations
*)

FROM xUtils IMPORT Error;
IMPORT Fmt AS F;

PROCEDURE Lex(str:TEXT):C.T RAISES {Error};
        (*reads after the "COMPLEX{" in COMPLEX{re:=<r>; im:=<r>},
        thru the "}"*)
PROCEDURE Fmt(x:C.T; 
        style:F.Style:=F.Style.Fix;
        prec:CARDINAL:=3):TEXT;
        (*outputs as "COMPLEX{re:=<r>; im:=<r>}"
        Uses simple F.Real if x.im=0.0.*)

(*==========================*)
END FractionFmtLex.
