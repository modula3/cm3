MODULE Msg;

IMPORT Wr, Stdio, MxConfig, Process;

PROCEDURE M(a, b, c, d, e, f : TEXT := NIL) =
  BEGIN
    TRY
      IF a # NIL THEN Wr.PutText(Stdio.stdout, a) END;
      IF b # NIL THEN Wr.PutText(Stdio.stdout, b) END;
      IF c # NIL THEN Wr.PutText(Stdio.stdout, c) END;
      IF d # NIL THEN Wr.PutText(Stdio.stdout, d) END;
      IF e # NIL THEN Wr.PutText(Stdio.stdout, e) END;
      IF f # NIL THEN Wr.PutText(Stdio.stdout, f) END;
      Wr.PutText(Stdio.stdout, EOL)
    EXCEPT ELSE
    END;
  END M;

PROCEDURE D(a, b, c, d, e, f : TEXT := NIL) =
  BEGIN
    IF debug THEN M(a, b, c, d, e, f) END;
  END D;

PROCEDURE V(a, b, c, d, e, f : TEXT := NIL) =
  BEGIN
    IF verbose THEN M(a, b, c, d, e, f) END;
  END V;

PROCEDURE F(a, b, c, d, e, f : TEXT := NIL) =
  BEGIN
    M(a, b, c, d, e, f);
    Process.Exit(1);
  END F;

VAR
  EOL : TEXT := "\n";
BEGIN
  IF MxConfig.HOST_OS_TYPE() = MxConfig.OS_TYPE.WIN32 THEN
    EOL := "\r\n";
  END;
END Msg.
