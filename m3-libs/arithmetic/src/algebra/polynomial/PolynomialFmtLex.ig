GENERIC INTERFACE PolynomialFmtLex(P,RF);
(*Copyright (c) 1996, m3na project
  
Abstract: Direct access to Polynomial functions

2/3/96   Harry George    Initial version
2/17/96  Harry George    Convert from OO to ADT
*)

IMPORT Wr,Thread;
(*==========================*)
TYPE
  T = P.T;
  FmtStyle = RECORD elemStyle := RF.FmtStyle{}; END;

<*UNUSED*>
PROCEDURE Lex(str:TEXT):T;
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END PolynomialFmtLex.
