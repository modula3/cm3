GENERIC INTERFACE MatrixFmtLex(RF,M);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to matrix functions

2/17/96  Harry George    Convert from OO to ADT
*)
(*
FROM NADefinitions IMPORT Error;
*)
IMPORT Wr, Thread;
(*==========================*)
(*-----------------*)
TYPE
  T = M.T;
  FmtStyle = RECORD width : CARDINAL := 12; elemStyle := RF.FmtStyle{}; END;
  TexStyle = RECORD elemStyle := RF.TexStyle{}; END;

(*
<*UNUSED*>
PROCEDURE Lex(str:TEXT):T RAISES {Error};
*)
PROCEDURE Fmt (x : T; READONLY style := FmtStyle{}) : TEXT RAISES {Thread.Alerted, Wr.Failure};
PROCEDURE Tex (x : T; READONLY style := TexStyle{}) : TEXT RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END MatrixFmtLex.
