UNSAFE MODULE RTRefStats;
(* $Id$ *)

FROM Debug IMPORT S;
IMPORT Fmt;
IMPORT RTHeapRep;
IMPORT RTType;
IMPORT RTName;
FROM RT0 IMPORT Typecode;
IMPORT IntPQ;
IMPORT Cprintf;
IMPORT M3toC;

TYPE
  Counts = REF ARRAY OF R;
  R = RECORD
    count, size := 0;
  END;
  T = RTHeapRep.RefVisitor OBJECT
    x: Counts;
    printTexts: BOOLEAN;
  OVERRIDES
    visit := Visit;
  END;

PROCEDURE Visit(self: T; tc: Typecode;
                r: REFANY; size: CARDINAL): BOOLEAN =
  BEGIN
    IF self.printTexts AND ISTYPE(r, TEXT) THEN
      VAR
        s := M3toC.CopyTtoS(r);
      BEGIN
        Cprintf.prints(s);
        M3toC.FreeCopiedS(s)
      END
    END;
    INC(self.x[tc].count);
    INC(self.x[tc].size, size);
    RETURN TRUE;
  END Visit;

TYPE
  E = IntPQ.Elt OBJECT tc, count: INTEGER; END;

PROCEDURE Line(tc: INTEGER; name: TEXT; size, count: INTEGER) =
  BEGIN
          S(Fmt.Pad(Fmt.Int(tc),5) & "  " &
            Fmt.Pad(name,40,' ',Fmt.Align.Left) &
            Fmt.Pad(Fmt.Int(size),14) &
            Fmt.Pad(Fmt.Int(count),12) &
            Fmt.Pad(Fmt.Int(size DIV count),9),
            0);
  END Line;

PROCEDURE ReportReachable(printTexts := FALSE) =
  <* FATAL IntPQ.Empty *>
  VAR
    n := RTType.MaxTypecode()+1;
    self := NEW(T, x := NEW(REF ARRAY OF R, n), printTexts:=printTexts);
  BEGIN
    S("visiting references...", 0);
    RTHeapRep.VisitAllRefs(self);
    S("sorting stats by size...", 0);
    VAR
      q := NEW(IntPQ.Default).init();
      e: E;
      totalCount, totalSize := 0;
    BEGIN
      FOR tc := 0 TO n-1 DO
        IF self.x[tc].count # 0 THEN
          q.insert(NEW(E,
                       priority := self.x[tc].size,
                       count := self.x[tc].count,
                       tc := tc));
        END;
      END;
      S("   TC  Name                                         totalSize       count  avgSize",0);
      WHILE q.size() > 0 DO
        e := q.deleteMin();
        Line(e.tc, RTName.GetByTC(e.tc), e.priority, e.count);
        INC(totalCount, e.count);
        INC(totalSize, e.priority);
      END;
      Line(n, "<- Number of typecodes         totals ->",
           totalSize,totalCount)
    END
  END ReportReachable;

BEGIN
END RTRefStats.
