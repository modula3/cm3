MODULE Main;

IMPORT Rd, Stdio, TextRefTbl, Thread, Wr;

PROCEDURE DoIt () =
  <*FATAL Rd.Failure, Rd.EndOfFile, Wr.Failure, Thread.Alerted *>
  VAR
    txt: TEXT;
    seen := NEW (TextRefTbl.Default).init (200);
  BEGIN
    WHILE NOT Rd.EOF (Stdio.stdin) DO
      txt := Rd.GetLine (Stdio.stdin);
      IF NOT seen.put (txt, NIL) THEN
        Wr.PutText (Stdio.stdout, txt);
        Wr.PutText (Stdio.stdout, Wr.EOL);
      END;
    END;
  END DoIt;

BEGIN
  DoIt ();
END Main.
