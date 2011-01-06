MODULE Main;
IMPORT FileWr, Wr, Fmt;

TYPE
  PV = RECORD
    price : LONGREAL;
    volume : CARDINAL;
  END;

  HFData_S = RECORD
    time : LONGREAL;
    pv : PV;
  END;

PROCEDURE DumpMatching() =
  PROCEDURE DumpOne(trade : HFData_S) RAISES{Wr.Failure} =
    BEGIN
      Wr.PutText(wr, Fmt.LongReal(trade.time));
    END DumpOne;
  VAR
    wr := FileWr.Open("out");
  BEGIN
  END DumpMatching;

BEGIN
END Main.
