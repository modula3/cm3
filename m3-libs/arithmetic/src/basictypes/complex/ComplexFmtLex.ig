GENERIC INTERFACE ComplexFmtLex(RF, C);
(*Copyright (c) 1996, m3na project*)

(*Abstract: Formatting and parsing complex numbers*)

IMPORT Rd, Wr, Thread;
IMPORT Lex AS L;
IMPORT FloatMode;
(*FROM NADefinitions IMPORT Error;*)
FROM FmtLexSupport IMPORT Precedence;


TYPE T = C.T;

TYPE FmtStyle = RECORD elemStyle := RF.FmtStyle{};  END;

PROCEDURE Fmt (READONLY x: T; READONLY style := FmtStyle{}): TEXT;
(*outputs as "COMPLEX{re:=<r>; im:=<r>}" Uses simple F.Real if x.im=0.0.*)

TYPE TexStyle = RECORD elemStyle := RF.TexStyle{};  END;

PROCEDURE Tex (READONLY x     : T;
               READONLY style       := TexStyle{};
                        within      := Precedence.sum): TEXT;

TYPE
  LexStyle = RECORD
               sep       := '+';
               elemStyle := RF.LexStyle{};
             END;

PROCEDURE Lex (rd: Rd.T; READONLY style := LexStyle{}; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted};

END ComplexFmtLex.
