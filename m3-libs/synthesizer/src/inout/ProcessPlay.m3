MODULE ProcessPlay;

IMPORT Signal;

FROM ProcessInOut IMPORT WordSize, WordSizeToText;
IMPORT ProcessPipeOut;
IMPORT Fmt, Wr, Thread;

PROCEDURE MakeParams
  (numChannels: CARDINAL; rate: LONGREAL; wordSize: WordSize; ):
  REF ARRAY OF TEXT =
  VAR
    params := ARRAY [0 .. 6] OF
                TEXT{"-t", "s" & WordSizeToText[wordSize], "-c",
                     Fmt.Int(numChannels), "-r", Fmt.LongReal(rate), "-"};
    refParams := NEW(REF ARRAY OF TEXT, NUMBER(params));
  BEGIN
    refParams^ := params;
    RETURN refParams;
  END MakeParams;

PROCEDURE Array
  (READONLY x: Signal.Array; rate: LONGREAL; wordSize: WordSize; )
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    ProcessPipeOut.Array(
      x, "play", MakeParams(1, rate, wordSize)^, wordSize);
  END Array;

PROCEDURE Stream (x: Signal.T; rate: LONGREAL; wordSize: WordSize; )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted} =
  BEGIN
    ProcessPipeOut.Stream(
      x, "play", MakeParams(1, rate, wordSize)^, wordSize);
  END Stream;

PROCEDURE MultiStream
  (READONLY x: ARRAY OF Signal.T; rate: LONGREAL; wordSize: WordSize; )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted} =
  BEGIN
    ProcessPipeOut.MultiStream(
      x, "play", MakeParams(NUMBER(x), rate, wordSize)^, wordSize);
  END MultiStream;

BEGIN
END ProcessPlay.
