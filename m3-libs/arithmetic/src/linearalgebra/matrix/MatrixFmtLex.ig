GENERIC INTERFACE MatrixFmtLex(M,RF);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to matrix functions

2/17/96  Harry George    Convert from OO to ADT
*)
(*
FROM xUtils IMPORT Error;
*)
IMPORT Wr, Thread;
(*==========================*)
(*-----------------*)
TYPE
  T = M.T;
  FmtStyle = RECORD width : CARDINAL := 12; elemStyle := RF.FmtStyle{}; END;

(*
<*UNUSED*>
PROCEDURE Lex(str:TEXT):T RAISES {Error};
*)
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END MatrixFmtLex.
