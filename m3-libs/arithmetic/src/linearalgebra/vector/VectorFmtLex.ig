GENERIC INTERFACE VectorFmtLex(V);
(*Copyright (c) 1996, m3na project

Abstract: vector formatting

2/17/96  Harry George    Convert from Objects to ADT's
*)
IMPORT Fmt AS F,Wr,Thread;
(*==========================*)
TYPE
  T = V.T;

<*UNUSED*>
PROCEDURE Lex(str:TEXT):T;
PROCEDURE Fmt(x:T;
            style:=F.Style.Fix;
            prec:=2):TEXT RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END VectorFmtLex.
