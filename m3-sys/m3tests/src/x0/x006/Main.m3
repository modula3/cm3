(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Tst EXPORTS Main;

IMPORT Wr, Stdio, Rd, Text, CoreOS, FileStream, Time, Fmt, Scan;
IMPORT ParseParams;

IMPORT Thread;
FROM Thread IMPORT Closure, SizedClosure;

PROCEDURE Reader(cl: Closure <* NOWARN *>): REFANY RAISES{} =
  VAR
     txt: TEXT;
  BEGIN
    LOOP
      txt := Rd.GetLine(Stdio.stdin);
      Wr.PutText(Stdio.stderr, Fmt.F("  *%s*\n", txt));
    END;
    RETURN(NIL);
  END Reader;

PROCEDURE Writer(cl: Thread.Closure  <* NOWARN *>): REFANY RAISES{} =
  CONST
    maxChar = 60000;
  VAR
    rd: Rd.T;
    wr: Wr.T;
    txt: TEXT;
    buf: REF ARRAY OF CHAR;
  BEGIN
    TRY
      rd := FileStream.OpenRead(file);
    EXCEPT
      Rd.Failure => Wr.PutText(Stdio.stderr, "Can\'t open sound file\n");
                    CoreOS.exit(-1);
    END;

    txt := Rd.GetText(rd, maxChar);
    buf := NEW(REF ARRAY OF CHAR, Text.Length(txt));
    Text.SetChars(buf^, txt);

    TRY
      (* wr := FileStream.OpenWrite("/dev/audio"); *)
      wr := FileStream.OpenWrite("/dev/null");
    EXCEPT
      Wr.Failure => Wr.PutText(Stdio.stderr, "Can\'t open /dev/audio\n");
                    CoreOS.exit(-1);
    END;

    LOOP
      Wr.PutText(Stdio.stderr, "About to PutString\n");
      Wr.PutString(wr, buf^);
      Wr.PutText(Stdio.stderr, "Did PutString\n");
      IF pause >= 0 THEN
        Time.Pause(pause);
      END;
    END;
    RETURN NIL;
  END Writer;

PROCEDURE Doit() =
  BEGIN
    EVAL Thread.Fork(NEW(SizedClosure, apply := Reader,
                         stackSize := 10*Thread.DefaultStackSize));
    EVAL Thread.Fork(NEW(SizedClosure, apply := Writer,
                         stackSize := 10*Thread.DefaultStackSize));
    Time.Pause(1000000000);
  END Doit;

VAR
  file: TEXT;
  pause:= -1;

BEGIN
  IF ParseParams.NumParameters > 1 THEN
    file := ParseParams.GetParameter(1);
  ELSE
    file := "/etc/termcap";
  END;
  IF  ParseParams.NumParameters > 2 THEN
    pause := Scan.Int(ParseParams.GetParameter(2));
  END;
  Doit();
END Tst.
