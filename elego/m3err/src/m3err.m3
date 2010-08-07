(*---------------------------------------------------------------------------*)
MODULE m3err EXPORTS Main;

IMPORT Process, Text, FileRd, Rd, Params, Wr, Stdio;
IMPORT TextUtils, Creation, SMsg AS Msg;

(*---------------------------------------------------------------------------*)
PROCEDURE M(msg : TEXT) =
  BEGIN
    TRY
      Wr.PutText(Stdio.stdout, msg & "\n");
    EXCEPT ELSE
      Msg.Fatal("cannot write to stdout", 1000);
    END;
  END M;

(*---------------------------------------------------------------------------*)
VAR
  rd :  FileRd.T;
  fn := ".errors";
  t  :  TEXT;
BEGIN
  IF Params.Count > 1 THEN
    fn := Params.Get(1);
    IF Text.Equal(fn, "-created") THEN
      M(Creation.Date & " on " & Creation.System);
      Process.Exit(0);
    END;
  END;
  TRY
    rd := FileRd.Open(fn);
    t := Rd.GetText(rd, LAST(CARDINAL));
    Rd.Close(rd);
  EXCEPT ELSE
    Process.Exit(4000);
  END;
  IF t = NIL OR Text.Length(t) = 0 THEN
    Process.Exit(0);
  END;
  IF TextUtils.Contains(t, "error", caseSensitive := FALSE) THEN
    Process.Exit(1);
  END;
  IF TextUtils.Contains(t, "failed", caseSensitive := FALSE) THEN
    Process.Exit(1);
  END;
  IF TextUtils.Contains(t, "fatal", caseSensitive := FALSE) THEN
    Process.Exit(1);
  END;
  Process.Exit(0);
END m3err.
