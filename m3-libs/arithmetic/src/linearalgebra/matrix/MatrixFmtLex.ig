GENERIC INTERFACE MatrixFmtLex(RF, M);
(*Copyright (c) 1996, m3na project*)
(*
FROM NADefinitions IMPORT Error;
*)
IMPORT Wr, Thread;
(*==========================*)
TYPE T = M.T;

TYPE
  FmtStyle = RECORD
               width    : CARDINAL := 12;
               elemStyle           := RF.FmtStyle{};
             END;

PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}): TEXT
  RAISES {Thread.Alerted, Wr.Failure};

TYPE TexStyle = RECORD elemStyle := RF.TexStyle{};  END;

PROCEDURE Tex (x: T; READONLY style := TexStyle{}): TEXT
  RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END MatrixFmtLex.
