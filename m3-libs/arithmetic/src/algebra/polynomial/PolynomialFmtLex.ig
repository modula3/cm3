GENERIC INTERFACE PolynomialFmtLex(P);
(*Copyright (c) 1996, m3na project
  
Abstract: Direct access to Polynomial functions

2/3/96   Harry George    Initial version
2/17/96  Harry George    Convert from OO to ADT
*)

FROM P IMPORT T;
IMPORT Wr,Thread,Fmt AS F;
(*==========================*)

<*UNUSED*>
PROCEDURE Lex(str:TEXT):T;
PROCEDURE Fmt(x:T;
              style:F.Style:=F.Style.Fix;
              prec:CARDINAL:=1):TEXT RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END PolynomialFmtLex.
