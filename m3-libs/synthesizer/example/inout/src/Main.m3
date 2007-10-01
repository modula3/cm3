MODULE Main;

IMPORT ProcessPlay;
IMPORT ProcessLoad;
IMPORT IO, Fmt;
IMPORT Rd, Wr, Thread;

<* FATAL Rd.Failure, Wr.Failure, Thread.Alerted *>

CONST
  FileName = "SuperfrogMono.aiff";

BEGIN
  WITH x = ProcessLoad.Do(FileName)^ DO
    IO.Put(Fmt.F("File '%s' contains %s samples.\n", FileName,
                 Fmt.Int(NUMBER(x))));
    ProcessPlay.Array(x, 44100.0D0);
  END;
END Main.
