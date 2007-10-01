MODULE ProcessSave;

FROM ProcessInOut IMPORT WordSize, WordSizeToText;

IMPORT Signal, ProcessPipeOut, ProcessInOut;
IMPORT Wr, Thread, Fmt, TextSeq;

PROCEDURE MakeParams (numChannels: CARDINAL;
                      rate       : LONGREAL;
                      name       : TEXT;
                      format     : TEXT;
                      wordSize   : WordSize; ): REF ARRAY OF TEXT =
  VAR params := NEW(TextSeq.T).init();
  BEGIN
    params.addhi("-t");
    params.addhi("s" & WordSizeToText[wordSize]);
    params.addhi("-c");
    params.addhi(Fmt.Int(numChannels));
    params.addhi("-r");
    params.addhi(Fmt.LongReal(rate));
    params.addhi("-");

    IF format # NIL THEN params.addhi("-t"); params.addhi(format); END;
    params.addhi(name);

    RETURN ProcessInOut.TextSeqToArr(params);
  END MakeParams;

PROCEDURE Array (READONLY x       : Signal.Array;
                          rate    : LONGREAL;
                          name    : TEXT;
                          format  : TEXT;
                          wordSize: WordSize;     )
  RAISES {Wr.Failure, Thread.Alerted} =

  BEGIN
    ProcessPipeOut.Array(
      x, "sox", MakeParams(1, rate, name, format, wordSize)^, wordSize);
  END Array;

PROCEDURE Stream (x       : Signal.T;
                  rate    : LONGREAL;
                  name    : TEXT;
                  format  : TEXT;
                  wordSize: WordSize; )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted} =

  BEGIN
    ProcessPipeOut.Stream(
      x, "sox", MakeParams(1, rate, name, format, wordSize)^, wordSize);
  END Stream;

PROCEDURE MultiStream (READONLY x       : ARRAY OF Signal.T;
                                rate    : LONGREAL;
                                name    : TEXT;
                                format  : TEXT;
                                wordSize: WordSize;          )
  RAISES {Signal.Error, Wr.Failure, Thread.Alerted} =

  BEGIN
    ProcessPipeOut.MultiStream(
      x, "sox", MakeParams(NUMBER(x), rate, name, format, wordSize)^,
      wordSize);
  END MultiStream;

BEGIN
END ProcessSave.
