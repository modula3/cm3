MODULE UnitTestList;

IMPORT UnitTest, UnitTestTerminal, RefSeq;

REVEAL
  T = Public BRANDED OBJECT
        children: RefSeq.T;
        name    : TEXT;
      OVERRIDES
        init     := Init;
        addChild := AddChild;
        run      := Run;
      END;

PROCEDURE Init
  (SELF: T; name: TEXT; READONLY children: ARRAY OF UnitTest.T; ):
  UnitTest.T =
  BEGIN
    SELF.name := name;
    WITH refs = NEW(REF ARRAY OF REFANY, NUMBER(children))^ DO
      FOR i := FIRST(children) TO LAST(children) DO
        refs[i] := children[i];
      END;
      SELF.children := NEW(RefSeq.T).fromArray(refs);
    END;
    RETURN SELF;
  END Init;

PROCEDURE AddChild (SELF: T; child: UnitTest.T; ) =
  BEGIN
    SELF.children.addhi(child);
  END AddChild;

PROCEDURE Run (SELF: T; terminal: UnitTestTerminal.T; ): UnitTest.Report =
  VAR
    report := UnitTest.Report{
                numTests := 0, numFailedTests := 0, numErrors := 0};
  BEGIN
    terminal.put("Test set: " & SELF.name & "\n");
    terminal.indent();
    FOR i := 0 TO SELF.children.size() - 1 DO
      WITH childReport = NARROW(SELF.children.get(i), UnitTest.T).run(
                           terminal) DO
        INC(report.numTests, childReport.numTests);
        INC(report.numFailedTests, childReport.numFailedTests);
        INC(report.numErrors, childReport.numErrors);
      END;
    END;
    terminal.deindent();
    RETURN report;
  END Run;


BEGIN
END UnitTestList.
