GENERIC INTERFACE IntegerFmtLex(I);
(*Copyright (c) 1996, m3na project

Abstract: Generic formatting and parsing of integer types

2/17/96  Harry George    Initial version
*)

IMPORT Fmt AS F, Thread, Wr;

(*==========================*)

(*============================*)
(* Functions                  *)
(*============================*)  
CONST Fmt = F.Int;

PROCEDURE FmtArray(READONLY a:ARRAY OF I.T;
                   base      :CARDINAL:=10;
                   cellwidth :CARDINAL:=4;
                   linewidth :CARDINAL:=60):TEXT RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END IntegerFmtLex.
