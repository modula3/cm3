GENERIC INTERFACE VectorFmtLex(V,RF);
(*Copyright (c) 1996, m3na project

Abstract: vector formatting

2/17/96  Harry George    Convert from Objects to ADT's
*)
IMPORT Wr,Thread;
(*==========================*)
TYPE
  T = V.T;
  FmtStyle = RECORD elemStyle := RF.FmtStyle{}; END;

<*UNUSED*>
PROCEDURE Lex(str:TEXT):T;
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END VectorFmtLex.
