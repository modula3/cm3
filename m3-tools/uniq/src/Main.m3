MODULE Main;

IMPORT Rd, Stdio, TextRefTbl, Thread, Wr;
IMPORT FileRd, Params, OSError;

PROCEDURE DoIt () =
  <*FATAL Rd.Failure, Rd.EndOfFile, Wr.Failure, Thread.Alerted, OSError.E *>
  VAR
    txt: TEXT;
    seen := NEW (TextRefTbl.Default).init (200);
    rd: Rd.T := Stdio.stdin;
  BEGIN
    IF Params.Count = 2 THEN
      rd := FileRd.Open(Params.Get(1));
    END;
    WHILE NOT Rd.EOF (rd) DO
      txt := Rd.GetLine (rd);
      IF NOT seen.put (txt, NIL) THEN
        Wr.PutText (Stdio.stdout, txt);
        Wr.PutText (Stdio.stdout, Wr.EOL);
      END;
    END;
  END DoIt;

BEGIN
  DoIt ();
END Main.
