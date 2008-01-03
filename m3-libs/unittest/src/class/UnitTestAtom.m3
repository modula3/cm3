MODULE UnitTestAtom;

IMPORT UnitTest, UnitTestAtomRep, UnitTestTerminal, TextSeq;

REVEAL
  T = UnitTestAtomRep.Public BRANDED OBJECT
      OVERRIDES
        init    := Init;
        run     := Run;
        error   := Error;
        message := Message;
      END;

PROCEDURE Init (SELF: T; name: TEXT; ): T =
  BEGIN
    SELF.errors := NEW(TextSeq.T).init();
    SELF.name := name;
    RETURN SELF;
  END Init;

PROCEDURE Run (SELF: T; terminal: UnitTestTerminal.T; ): UnitTest.Report =
  BEGIN
    SELF.terminal := terminal;
    terminal.put("Check " & SELF.name & "\n");
    terminal.indent();
    SELF.test();
    terminal.deindent();
    SELF.terminal := NIL;
    WITH numErrors = SELF.errors.size() DO
      RETURN UnitTest.Report{numTests := 1, numFailedTests :=
                             ORD(numErrors # 0), numErrors := numErrors};
    END;
  END Run;

PROCEDURE Error (SELF: T; message: TEXT; ) =
  BEGIN
    SELF.errors.addhi(message);
    SELF.terminal.put("Test failed:\n" & message);
  END Error;

PROCEDURE Message (SELF: T; message: TEXT; ) =
  BEGIN
    SELF.terminal.put(message);
  END Message;


BEGIN
END UnitTestAtom.
