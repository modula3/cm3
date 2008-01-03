MODULE Main;

IMPORT IO, Params, Process, Stdio, Text, Wr;
IMPORT M3CG_Rd, M3CG_Wr, M3CG_BinRd, M3CG_BinWr, MxConfig, Target;

PROCEDURE DoIt () =
  VAR arg: TEXT;
  BEGIN
    IF Params.Count # 2 THEN Usage (); END;
    arg := Params.Get (1);
    IF Text.Equal (arg, "-ascii") THEN
      Init ();
      M3CG_Rd.Inhale (Stdio.stdin, M3CG_BinWr.New (Stdio.stdout));
    ELSIF Text.Equal (arg, "-binary") THEN
      Init ();
      M3CG_BinRd.Inhale (Stdio.stdin, M3CG_Wr.New (Stdio.stdout));
    ELSE
      Usage ();
    END;
  END DoIt;

PROCEDURE Usage () =
  BEGIN
    IO.Put ("usage: " & Params.Get(0) & " -ascii  < in.asc > out.bin" & Wr.EOL);
    IO.Put ("       " & Params.Get(0) & " -binary < in.bin > out.asc" & Wr.EOL);
    Process.Exit (1);
  END Usage;

PROCEDURE Init () =
  VAR machine: TEXT;
  BEGIN
    machine := MxConfig.Get ("TARGET");
    IF (machine = NIL) THEN
      IO.Put ("unable to find TARGET definition in configuration file" & Wr.EOL);
      Process.Exit (1);
    ELSIF NOT Target.Init (machine) THEN
      IO.Put ("unable to initialize Target: " & machine & Wr.EOL);
      Process.Exit (1);
    END;
  END Init;

BEGIN
  DoIt ();
END Main.
