MODULE Main;

IMPORT FileRd, OSError, Params, Pathname, Process, Rd;
IMPORT Stdio, TextRefTbl, Text, Thread, Wr;

CONST
  MaxInputs = 4;

VAR
  Slash := ARRAY BOOLEAN OF CHAR { '\134', '/' }
             [Text.Equal ("a/b", Pathname.Join ("a", "b", NIL))];

TYPE
  Note = REF RECORD
    file : TEXT;
    fp   : ARRAY [0..MaxInputs-1] OF TEXT;
  END;

VAR
  IsSpace: ARRAY CHAR OF BOOLEAN;

PROCEDURE DoIt () =
  VAR seen := NEW (TextRefTbl.Default).init (512);
  BEGIN
    IF Params.Count < 3 OR Params.Count > MaxInputs+1 THEN
      Die ("usage:  cmpfp  in0 in1 ...");
    END;

    FOR c := FIRST (IsSpace) TO LAST (IsSpace) DO
      IsSpace [c] := FALSE;
    END;
    IsSpace [' '] := TRUE;
    IsSpace ['\n'] := TRUE;
    IsSpace ['\r'] := TRUE;
    IsSpace ['\t'] := TRUE;

    FOR i := 1 TO Params.Count-1 DO
      LoadFile (seen, Params.Get (i), i - 1);
    END;

    DumpDiffs (seen, Params.Count-1);
  END DoIt;

PROCEDURE LoadFile (seen: TextRefTbl.T;  nm: TEXT;  index: CARDINAL) =
  VAR
    rd   : Rd.T;
    len  : INTEGER;
    nm_0 : INTEGER;
    nm_1 : INTEGER;
    fp_0 : INTEGER;
    fp_1 : INTEGER;
    key  : TEXT;
    val  : TEXT;
    ref  : REFANY;
    note : Note;
    buf  : ARRAY [0..511] OF CHAR;
  BEGIN
    TRY
      rd := FileRd.Open (nm);
    EXCEPT OSError.E =>
      Die ("unable to open ", nm);
    END;
    TRY
      WHILE NOT Rd.EOF (rd) DO
        len := Rd.GetSubLine (rd, buf);

        (* scan to find the field limits *)
        nm_0 := 0;
        WHILE (nm_0 < len) AND IsSpace[buf[nm_0]] DO INC (nm_0); END;
        nm_1 := nm_0;
        WHILE (nm_1 < len) AND NOT IsSpace[buf[nm_1]] DO INC (nm_1); END;
        fp_0 := nm_1;
        WHILE (fp_0 < len) AND IsSpace[buf[fp_0]] DO INC (fp_0); END;
        fp_1 := fp_0;
        WHILE (fp_1 < len) AND NOT IsSpace[buf[fp_1]] DO INC (fp_1); END;

        (* convert slashes in the file name to the host convention *)
        FOR i := nm_0 TO nm_1-1 DO
          IF buf[i] = '/' OR buf[i] = '\134' THEN buf[i] := Slash; END;
        END;

        IF nm_0 < nm_1 AND fp_0 < fp_1 THEN
          key := Text.FromChars (SUBARRAY (buf, nm_0, nm_1 - nm_0));
          val := Text.FromChars (SUBARRAY (buf, fp_0, fp_1 - fp_0));
          IF seen.get (key, ref)
            THEN note := ref;
            ELSE note := NEW (Note, file := key);  EVAL seen.put (key, note);
          END;
          note.fp[index] := val;
        END;
      END;
      Rd.Close (rd);
    EXCEPT Rd.Failure, Thread.Alerted =>
      Die ("trouble reading ", nm);
    END;
  END LoadFile;

PROCEDURE DumpDiffs (seen: TextRefTbl.T;  cnt: INTEGER) =
  VAR
    iter := seen.iterate ();
    key  : TEXT;
    val  : REFANY;
    note : Note;
    v0, v1 : TEXT;
  BEGIN
    WHILE iter.next (key, val) DO
      note := val;
      v0 := note.fp[0];
      FOR i := 1 TO cnt-1 DO
        v1 := note.fp[i];
        IF v0 = NIL OR v1 = NIL OR NOT Text.Equal (v0, v1) THEN
          DumpNote (note, cnt);
          EXIT;
        END;
      END;
    END;
  END DumpDiffs;

PROCEDURE DumpNote (note: Note;  cnt: INTEGER) =
  VAR v: TEXT;
  BEGIN
    Out (note.file);
    FOR i := 0 TO cnt-1 DO
      v := note.fp[i];
      IF (v = NIL) THEN v := "**missing**"; END;
      Out ("  ", v);
    END;
    Out (Wr.EOL);
  END DumpNote;

PROCEDURE Die (a, b: TEXT := NIL) =
  BEGIN
    Out (a, b, Wr.EOL);
    Process.Exit (1);
  END Die;

PROCEDURE Out (a, b, c: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR wr := Stdio.stdout;
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
  END Out;

BEGIN
  DoIt ();
END Main.
