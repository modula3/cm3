MODULE ProcessPlaySolaris EXPORTS ProcessPlay;

IMPORT Signal;

FROM ProcessInOut IMPORT WordSize, WordSizeToText;
IMPORT ProcessPipeOut;
IMPORT Fmt, Wr, Thread;

PROCEDURE MakeParams (rate: LONGREAL; wordSize: WordSize; ):
  REF ARRAY OF TEXT =
  VAR params := NEW(REF ARRAY OF TEXT, 8);
  BEGIN
    params^ :=
      ARRAY OF
        TEXT{"s03", "play", "-x", "-t", "s" & WordSizeToText[wordSize],
             "-r", Fmt.LongReal(rate), "-"};
    RETURN params;
  END MakeParams;

PROCEDURE Array
  (READONLY x: Signal.Array; rate: LONGREAL; wordSize: WordSize; )
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    ProcessPipeOut.Array(x, "ssh", MakeParams(rate, wordSize)^, wordSize);
  END Array;

PROCEDURE Stream (x: Signal.T; rate: LONGREAL; wordSize: WordSize; )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted} =
  BEGIN
    ProcessPipeOut.Stream(x, "ssh", MakeParams(rate, wordSize)^, wordSize);
  END Stream;

BEGIN
END ProcessPlaySolaris.
