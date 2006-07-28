GENERIC MODULE SignalFmtLex(RF, SignalRep);

IMPORT Fmt AS F, Wr, TextWr, Thread;

PROCEDURE Fmt (x: T; READONLY style := FmtStyle{}): TEXT
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR
    SELF := NARROW(x, SignalRep.T);
    wr   := NEW(TextWr.T).init();
  BEGIN
    Wr.PutText(wr, "Signal[" & F.Int(SELF.getFirst()) & ".."
                     & F.Int(SELF.getLast()) & "]{");
    FOR i := FIRST(SELF.data^) TO LAST(SELF.data^) DO
      Wr.PutText(wr, RF.Fmt(SELF.data[i], style.elemStyle));
      IF i < LAST(SELF.data^) THEN Wr.PutText(wr, ", "); END;
    END;
    Wr.PutText(wr, "}");
    RETURN TextWr.ToText(wr);
  END Fmt;

BEGIN
END SignalFmtLex.
