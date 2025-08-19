MODULE Main;
IMPORT TextRefTbl, Fmt;
IMPORT RTRefStats AS Stats;
TYPE
  T = OBJECT
    blah, blh, blaah: INTEGER;
  END;
VAR
  tbl := NEW(TextRefTbl.Default).init();
BEGIN
  FOR i := 0 TO 10000 DO
    EVAL tbl.put(Fmt.Int(i), NEW(T));
  END;
  Stats.ReportReachable();
END Main.
