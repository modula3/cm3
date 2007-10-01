MODULE Main;

IMPORT UnitTest, UnitTestList, UnitTestTerminalText;

IMPORT TestSplit, TestControl, TestModulation;

BEGIN
  UnitTest.Run(
    NEW(UnitTestList.T).init(
      "Synthesizer", ARRAY OF
                       UnitTest.T{TestSplit.Test(), TestControl.Test(),
                                  TestModulation.Test()}),
    NEW(UnitTestTerminalText.T).init());
END Main.
