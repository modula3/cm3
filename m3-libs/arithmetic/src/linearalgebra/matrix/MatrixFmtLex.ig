GENERIC INTERFACE MatrixFmtLex(M);
(*Copyright (c) 1996, m3na project

Abstract: Direct access to matrix functions

2/17/96  Harry George    Convert from OO to ADT
*)
FROM xUtils IMPORT Error;
IMPORT Fmt AS F, Wr, Thread;
(*==========================*)
(*-----------------*)
TYPE
  T = M.T;

<*UNUSED*>
PROCEDURE Lex(str:TEXT):T RAISES {Error};
PROCEDURE Fmt(mat:T;
              style:=F.Style.Fix;
              prec:=2):TEXT RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END MatrixFmtLex.
