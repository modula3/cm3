MODULE UnitTest;

IMPORT UnitTestTerminal;
IMPORT Fmt;

PROCEDURE Run (test: T; terminal: UnitTestTerminal.T; ) =
  VAR
    report := test.run(terminal);
    hr     := "------\n";
  BEGIN
    IF report.numErrors = 0 THEN
      terminal.put(hr & Fmt.F("%4s tests ran successfully.\n",
                              Fmt.Int(report.numTests)));
    ELSE
      terminal.put(
        hr & Fmt.F(
               "%4s executed tests\n" & "%4s failed tests\n"
                 & "%4s single errors\n", Fmt.Int(report.numTests),
               Fmt.Int(report.numFailedTests), Fmt.Int(report.numErrors)));
      <* ASSERT FALSE *>
    END;
  END Run;

BEGIN
END UnitTest.
