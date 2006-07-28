INTERFACE UnitTest;

IMPORT UnitTestTerminal;

TYPE
  T = OBJECT METHODS run (terminal: UnitTestTerminal.T; ): Report; END;

  Report = RECORD
             numTests      : CARDINAL;
             numFailedTests: CARDINAL;
             numErrors     : CARDINAL;
           END;

PROCEDURE Run (test: T; terminal: UnitTestTerminal.T; );

END UnitTest.
