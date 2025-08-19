UNSAFE MODULE RTRefStats;
(* $Id: RTRefStats.m3,v 1.8 2008/06/06 00:15:20 mika Exp $ *)

IMPORT Fmt;
IMPORT RTHeapRep;
IMPORT RTType;
IMPORT RTName;
FROM RT0 IMPORT Typecode;
IMPORT IntPQ;
IMPORT Cprintf;
IMPORT M3toC;
IMPORT Debug;
IMPORT Wr;
IMPORT Time;
IMPORT Thread;

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

PROCEDURE S(wr : Wr.T; t: TEXT; minLevel : CARDINAL := 5) =
  BEGIN
    IF wr = NIL THEN
      Debug.S(t, minLevel)
    ELSE
      TRY
        Wr.PutText(wr, t);
        Wr.PutChar(wr, '\n')
      EXCEPT
      ELSE
      END
    END
  END S;

PROCEDURE Visit(self : T;
                tc   : Typecode;
                r    : REFANY;
                size : CARDINAL): BOOLEAN =
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

PROCEDURE Line(wr : Wr.T; tc: INTEGER; name: TEXT; size, count: INTEGER) =
  BEGIN
          S(wr, Fmt.Pad(Fmt.Int(tc),5) & "  " &
            Fmt.Pad(name,40,' ',Fmt.Align.Left) &
            Fmt.Pad(Fmt.Int(size),14) &
            Fmt.Pad(Fmt.Int(count),12) &
            Fmt.Pad(Fmt.Int(size DIV count),9),
            0);
  END Line;

PROCEDURE ReportReachable(printTexts : BOOLEAN; wr : Wr.T) =
  <* FATAL IntPQ.Empty *>
  VAR
    n    := RTType.MaxTypecode()+1;
    self := NEW(T, x := NEW(REF ARRAY OF R, n), printTexts:=printTexts);
  BEGIN
    S(wr, "visiting references...", 0);
    RTHeapRep.VisitAllRefs(self);
    S(wr, "sorting stats by size...", 0);
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
      S(wr, "   TC  Name                                         totalSize       count  avgSize",0);
      WHILE q.size() > 0 DO
        e := q.deleteMin();
        Line(wr, e.tc, RTName.GetByTC(e.tc), e.priority, e.count);
        INC(totalCount, e.count);
        INC(totalSize, e.priority);
      END;
      Line(wr, n, "<- Number of typecodes         totals ->",
           totalSize,totalCount)
    END;
    TRY
      IF wr # NIL THEN Wr.Flush(wr) END
    EXCEPT
    ELSE
    END
  END ReportReachable;

REVEAL
  Reporter = PublicReporter BRANDED OBJECT
    mu         : MUTEX;
    enabled    : BOOLEAN;
    interval   : Time.T;
    wr         : Wr.T;
    printTexts : BOOLEAN;
  OVERRIDES
    init  := Init;
    start := Start;
    stop  := Stop;
  END;

TYPE
  Closure = Thread.Closure OBJECT
    reporter : Reporter;
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Init(r : Reporter;
               interval : Time.T;
               printTexts : BOOLEAN;
               wr : Wr.T) : Reporter =
  BEGIN
    r.mu := NEW(MUTEX);
    r.interval := interval;
    r.printTexts := printTexts;
    r.wr := wr;
    r.enabled := FALSE;
    EVAL Thread.Fork(NEW(Closure, reporter := r));
    RETURN r
  END Init;

PROCEDURE Start(r : Reporter) =
  BEGIN LOCK r.mu DO r.enabled := TRUE END END Start;
    
PROCEDURE Stop(r : Reporter) =
  BEGIN LOCK r.mu DO r.enabled := FALSE END END Stop;

PROCEDURE Apply(cl : Closure) : REFANY =
  VAR
    en : BOOLEAN;
  BEGIN
    LOOP
      Thread.Pause(cl.reporter.interval);
      LOCK cl.reporter.mu DO
        en := cl.reporter.enabled
      END;
      IF en THEN
        ReportReachable(cl.reporter.printTexts, cl.reporter.wr)
      END
    END
  END Apply;
    
BEGIN
END RTRefStats.
