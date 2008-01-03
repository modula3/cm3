GENERIC INTERFACE VectorFmtLex(RF, V);
(* Arithmetic for Modula-3, see doc for details *)

IMPORT Rd, Wr, Thread;
IMPORT Lex AS L;
IMPORT FloatMode;
FROM FmtLexSupport IMPORT Precedence;


TYPE T = V.T;

TYPE
  FmtStyle = RECORD
               width    : CARDINAL := 12;
               elemStyle           := RF.FmtStyle{};
             END;

PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}; ): TEXT
  RAISES {Thread.Alerted, Wr.Failure};

TYPE
  TexFlag = {Vertical            (* row or column vector? *)
            };
  TexFlagSet = SET OF TexFlag;
  TexStyle = RECORD
               flags     := TexFlagSet{};
               sep       := " \\quad ";
               elemStyle := RF.TexStyle{};
             END;

PROCEDURE Tex
  (x: T; READONLY style := TexStyle{}; within := Precedence.Sum; ): TEXT
  RAISES {Thread.Alerted, Wr.Failure};

TYPE
  LexStyle = RECORD
               sep       := ' ';
               term      := '\n';
               elemStyle := RF.LexStyle{};
             END;

PROCEDURE Lex (rd: Rd.T; READONLY style := LexStyle{}; ): T
  RAISES {L.Error, FloatMode.Trap, Rd.Failure, Thread.Alerted};

END VectorFmtLex.
