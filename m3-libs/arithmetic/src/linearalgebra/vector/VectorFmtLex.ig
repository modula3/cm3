GENERIC INTERFACE VectorFmtLex(RF, V);
(*Copyright (c) 1996, m3na project*)
IMPORT Wr, Thread;
FROM FmtLexSupport IMPORT Precedence;
(*==========================*)
TYPE T = V.T;

TYPE
  FmtStyle = RECORD
               width    : CARDINAL := 12;
               elemStyle           := RF.FmtStyle{};
             END;

PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}): TEXT
  RAISES {Thread.Alerted, Wr.Failure};

TYPE
  TexFlag = {vertical            (*row or column vector?*)
            };
  TexFlagSet = SET OF TexFlag;
  TexStyle = RECORD
               flags           := TexFlagSet{};
               sep      : TEXT := " \\quad ";
               elemStyle       := RF.TexStyle{};
             END;

PROCEDURE Tex (x: T; READONLY style := TexStyle{}; within := Precedence.sum):
  TEXT RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END VectorFmtLex.
