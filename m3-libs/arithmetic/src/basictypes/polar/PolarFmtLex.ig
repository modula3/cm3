GENERIC INTERFACE PolarFmtLex(P, RF);
(* Arithmetic for Modula-3, see doc for details *)

IMPORT Rd, Thread;
IMPORT Lex AS L;
IMPORT FloatMode;
FROM FmtLexSupport IMPORT Precedence;
(*
FROM Arithmetic IMPORT Error;
*)


TYPE T = P.T;

TYPE FmtStyle = RECORD elemStyle := RF.FmtStyle{};  END;

PROCEDURE Fmt (READONLY x: T; READONLY style := FmtStyle{}; ): TEXT;
(* as "POLAR{radius:=<r>; angle:=<r>}"*)

TYPE TexStyle = RECORD elemStyle := RF.TexStyle{};  END;

PROCEDURE Tex (READONLY x     : T;
               READONLY style       := TexStyle{};
                        within      := Precedence.Sum; ): TEXT;

TYPE
  LexStyle = RECORD
               sep       := ',';
               elemStyle := RF.LexStyle{};
             END;

PROCEDURE Lex (rd: Rd.T; READONLY style := LexStyle{}; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted};

END PolarFmtLex.
