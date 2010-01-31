(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Dec 14 14:43:35 PST 1994 by kalsow     *)
(*      modified on Mon Sep 26 14:47:43 PDT 1994 by isard      *)

MODULE Main;

IMPORT FileRd, FileWr, IO, M3CG_Rd, M3CG_Ops, M3x86, M3CG, OSError, Params;
IMPORT Rd, Stdio, Target, Text, Wr, MasmObjFile, NTObjFile, M3ObjFile;
<*FATAL ANY*>

PROCEDURE PrintErr(txt: TEXT) =
  BEGIN
    IO.Put (txt, Stdio.stderr);
    Wr.Close (out); Rd.Close (in);
    IF debug THEN Wr.Close (log) END;
    <* ASSERT FALSE *>
  END PrintErr;

VAR
  cg: M3CG.T;
  obj: M3ObjFile.T;
  gFlag := FALSE;
  debug := FALSE;
  objformat := FALSE;
  input, output, logfile: TEXT;
  i := 0;
  in: FileRd.T;
  out, log: FileWr.T;
  target_init := Target.Init ("NT386");

BEGIN
  <*ASSERT target_init*>

  WHILE i < Params.Count DO
    WITH opt = Params.Get (i) DO
      IF    Text.Equal (Text.Sub (opt, 0, 2), "-g") THEN
         gFlag := TRUE
      ELSIF Text.Equal (opt, "-o") AND i < Params.Count - 1 THEN
        INC (i); output := Params.Get (i)
      ELSIF Text.Equal (opt, "-v") THEN
        debug := TRUE
      ELSIF Text.Equal (opt, "-NTObj") THEN
        objformat := TRUE
      ELSIF Text.GetChar (opt, 0) # '-' THEN
        input := opt
      END
    END;
    INC (i)
  END;

  logfile := input & "log";

  TRY
    EVAL Target.Init ("-NT386");
    in  := FileRd.Open (input);
    out := FileWr.Open (output);

    IF objformat THEN
      obj := NTObjFile.New();
    ELSE
      obj := MasmObjFile.New();
    END;

    IF debug THEN
      log := FileWr.Open (logfile);
      cg := M3x86.New (log, obj);
    ELSE
      cg := M3x86.New (NIL, obj);
    END;

    cg.set_error_handler (PrintErr);
    M3CG_Rd.Inhale (in, cg);

    IF objformat THEN
      NTObjFile.Dump(obj, out);
    ELSE
      MasmObjFile.Dump(obj, out);
    END;

    IF debug THEN Wr.Close (log) END;
    Wr.Close (out); Rd.Close (in)
  EXCEPT
    OSError.E => IO.Put ("m3cg: IO error\n", Stdio.stderr)
  END;
END Main.


