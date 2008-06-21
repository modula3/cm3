
MODULE M3Compare;

IMPORT IO, Process, Fingerprint, Rd, Thread, Params;

PROCEDURE Compare (a, b: TEXT) =
(* Use "Fingerprint.FromText" to compare the two files. *)
  VAR aa, bb: TEXT;
  BEGIN
    aa := Inhale (a);
    bb := Inhale (b);
    IF (aa = NIL) OR (bb = NIL) THEN
      (* already reported an error *)
    ELSIF Fingerprint.FromText (aa) = Fingerprint.FromText (bb) THEN
      IO.Put ("The files are the same.\n");
    ELSE
      IO.Put ("The files are different.\n");
    END;
  END Compare;

PROCEDURE Inhale (file: TEXT): TEXT =
(* Read a file and return its contents as text. *)
VAR rd: Rd.T;  body: TEXT;
  BEGIN
    TRY
      rd := IO.OpenRead (file);
      IF (rd = NIL) THEN
        IO.Put ("\"" & file & "\" is not a file.\n");
        RETURN NIL;
      END;
      body := Rd.GetText (rd, LAST (CARDINAL));
      Rd.Close (rd);
    EXCEPT Rd.Failure, Thread.Alerted =>
      IO.Put ("Unable to read \"" & file & "\".\n");
      RETURN NIL;
    END;
    RETURN body;
  END Inhale;

BEGIN
  IF Params.Count # 3 THEN
    IO.Put ("syntax: m3compare <file1> <file2>\n");
    Process.Exit (2);
  END;
  Compare (Params.Get (1), Params.Get (2));
END M3Compare.
