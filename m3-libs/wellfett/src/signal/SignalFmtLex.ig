GENERIC INTERFACE SignalFmtLex(RF, Signal);

IMPORT Wr, Thread;
(*==========================*)

REVEAL Signal.T <: T;

TYPE
  FmtStyle = RECORD elemStyle := RF.FmtStyle{};  END;
  T = Signal.TPublic OBJECT
      METHODS
        fmt (READONLY style := FmtStyle{}): TEXT
             RAISES {Thread.Alerted, Wr.Failure} := Fmt;
      END;

(*PROCEDURE Lex(str:TEXT):T;*)
PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}): TEXT
  RAISES {Thread.Alerted, Wr.Failure};

(*==========================*)
END SignalFmtLex.
