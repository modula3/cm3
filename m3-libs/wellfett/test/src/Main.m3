MODULE Main;

IMPORT UnitTest, UnitTestList, UnitTestTerminalText;

IMPORT TestSignal,
       TestPerfectReconstruction,
       TestWaveletMatch,
       TestRefinableFunction;

BEGIN
  UnitTest.Run(
    NEW(UnitTestList.T).init(
      "Wavelet transform",
      ARRAY OF
        UnitTest.T{TestSignal.Test(), TestPerfectReconstruction.Test(),
                   TestWaveletMatch.Test(), TestRefinableFunction.Test()}),
    NEW(UnitTestTerminalText.T).init());
END Main.
