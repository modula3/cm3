MODULE Main;

IMPORT TestLapack;

IMPORT UnitTest, UnitTestTerminalText;

BEGIN
  UnitTest.Run(TestLapack.Test(),
    NEW(UnitTestTerminalText.T).init());
END Main.
