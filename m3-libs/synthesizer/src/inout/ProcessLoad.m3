MODULE ProcessLoad;

FROM ProcessInOut IMPORT WordSize, WordSizeToText;

IMPORT Signal, ProcessPipeIn, ProcessInOut;
IMPORT TextSeq;
IMPORT Rd, Thread;

PROCEDURE MakeParams (name: TEXT; format: TEXT; wordSize: WordSize; ):
  REF ARRAY OF TEXT =
  VAR params := NEW(TextSeq.T).init();
  BEGIN
    IF format # NIL THEN params.addhi("-t"); params.addhi(format); END;
    params.addhi(name);

    params.addhi("-t");
    params.addhi("s" & WordSizeToText[wordSize]);
    params.addhi("-");

    RETURN ProcessInOut.TextSeqToArr(params);
  END MakeParams;

PROCEDURE Do (name: TEXT; format: TEXT; wordSize: WordSize; ):
  Signal.RefArray RAISES {Rd.Failure, Thread.Alerted} =
  BEGIN
    RETURN ProcessPipeIn.Do(
             "sox", MakeParams(name, format, wordSize)^, wordSize);
  END Do;


REVEAL T = Public BRANDED OBJECT OVERRIDES init := Init; END;

PROCEDURE Init (SELF: T; name: TEXT; format: TEXT; wordSize: WordSize; ):
  T =
  BEGIN
    RETURN ProcessPipeIn.T.init(
             SELF, "sox", MakeParams(name, format, wordSize)^, wordSize);
  END Init;


REVEAL Multi = MultiPublic BRANDED OBJECT OVERRIDES init := MultiInit; END;

PROCEDURE MultiInit (         SELF       : Multi;
                              numChannels: CARDINAL;
                     READONLY channels   : ARRAY OF CARDINAL;
                              name       : TEXT;
                              format     : TEXT;
                              wordSize   : WordSize;          ): Multi =
  BEGIN
    RETURN
      ProcessPipeIn.Multi.init(
        SELF, numChannels, channels, "sox",
        MakeParams( (*numChannels*)name, format, wordSize)^, wordSize);
  END MultiInit;

BEGIN
END ProcessLoad.
