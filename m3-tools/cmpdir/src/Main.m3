(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

MODULE Main;

IMPORT Atom, AtomList, File, FS, OSError, Params, Pathname;
IMPORT Stdio, Text, TextArraySort, Wr;

PROCEDURE Compare (p1, p2: TEXT) =
  VAR
    dir1 := IsDirectory (p1);
    dir2 := IsDirectory (p2);
  BEGIN
    IF (dir1 # dir2) THEN
      IF dir1
        THEN Out (p1, " is a directory, ", p2, " is not.");
        ELSE Out (p1, " is not a directory, ", p2, " is.");
      END;
      RETURN;
    END;
    IF dir1
      THEN CompareDir (p1, p2);
      ELSE CompareFile (p1, p2);
    END;
  END Compare;

(*--------------------------------------------------------- directories ---*)

VAR
  n_names : CARDINAL := 0;
  names   := NEW (REF ARRAY OF TEXT, 200);

PROCEDURE CompareDir (p1, p2: TEXT) =
  VAR s1, s2, n1, n2: CARDINAL;  save := n_names;  cmp: INTEGER;
  BEGIN
    PushNames (p1, s1, n1);
    PushNames (p2, s2, n2);
    TextArraySort.Sort (SUBARRAY (names^, s1, n1));
    TextArraySort.Sort (SUBARRAY (names^, s2, n2));
    INC (n1, s1);
    INC (n2, s2);
    LOOP
      IF (s1 < n1) AND (s2 < n2) THEN
        cmp := Text.Compare (names[s1], names[s2]);
        IF (cmp = 0) THEN
          Compare (Pathname.Join (p1, names[s1], NIL),
                   Pathname.Join (p2, names[s2], NIL));
          INC (s1); INC (s2);
        ELSIF (cmp < 0) THEN
          Out ("<< ", Pathname.Join (p1, names[s1], NIL));
          INC (s1);
        ELSIF (cmp > 0) THEN
          Out (">> ", Pathname.Join (p2, names[s2], NIL));
          INC (s2);
        END;
      ELSIF (s1 < n1) THEN
        Out ("<< ", Pathname.Join (p1, names[s1], NIL));
        INC (s1);
      ELSIF (s2 < n2) THEN
        Out (">> ", Pathname.Join (p2, names[s2], NIL));
        INC (s2);
     ELSE
        (* both lists are empty *)
        EXIT;
      END;
    END;
    n_names := save;  (* pop the name sets *)
  END CompareDir;

PROCEDURE PushNames (dir: TEXT;  VAR(*OUT*) start, len: CARDINAL) =
  VAR iter: FS.Iterator;  nm: TEXT;
  BEGIN
    start := n_names;  len := 0;
    TRY
      iter := FS.Iterate (dir);
      WHILE iter.next (nm) DO
        IF (n_names >= NUMBER (names^)) THEN ExpandNames(); END;
        names[n_names] := nm;  INC (n_names);
      END;
    EXCEPT OSError.E (ec) =>
      Out ("trouble enumerating contents of ", dir, OSErr (ec));
    END;
    len := n_names - start;
  END PushNames;

PROCEDURE ExpandNames () =
  VAR n := NUMBER (names^);  xx := NEW (REF ARRAY OF TEXT, n+n);
  BEGIN
    SUBARRAY (xx^, 0, n) := names^;
    names := xx;
  END ExpandNames;

(*--------------------------------------------------------------- files ---*)

TYPE
  Buffer = REF ARRAY OF File.Byte;

VAR
  buf1 := NEW (Buffer, 10000);
  buf2 := NEW (Buffer, 10000);
  WhiteSpace: ARRAY File.Byte OF BOOLEAN;

PROCEDURE CompareFile (p1, p2: TEXT) =
  VAR
    n1 := Inhale (p1, buf1);
    n2 := Inhale (p2, buf2);
  BEGIN
    IF (n1 # n2) OR (SUBARRAY (buf1^, 0, n1) # SUBARRAY (buf2^, 0, n2)) THEN
      Out ("## ", p1, "  ", p2);
    END;
  END CompareFile;

PROCEDURE Inhale (fn: TEXT;  VAR buf: Buffer): CARDINAL =
  VAR f: File.T;  len, next, got: INTEGER;
  BEGIN
    TRY
      f := FS.OpenFileReadonly (fn);
      len := f.status().size;
      IF len > NUMBER (buf^) THEN ExpandBuf (buf, len); END;

      next := 0;
      WHILE (len > 0) DO
        got := f.read (SUBARRAY (buf^, next, len), mayBlock := TRUE);
        INC (next, got);
        DEC (len, got);
      END;

      f.close ();
    EXCEPT OSError.E(ec) =>
      Out ("trouble reading ", fn, OSErr (ec));
      RETURN 0;
    END;

    RETURN RemoveBlanks (buf, next);
  END Inhale;

PROCEDURE RemoveBlanks (buf: Buffer;  len: CARDINAL) : CARDINAL =
  VAR s0: CARDINAL := 0;  s1: CARDINAL := 0;  c: File.Byte;
  BEGIN
    WHILE (s0 < len) DO
      c := buf[s0];
      IF WhiteSpace [c] THEN
        buf[s1] := ORD (' ');  INC (s1);  INC (s0);
        WHILE (s0 < len) AND WhiteSpace [buf[s0]] DO INC (s0); END;
      ELSE
        buf[s1] := c;  INC (s0);  INC (s1);
      END;
    END;
    RETURN s1;
  END RemoveBlanks;

PROCEDURE ExpandBuf (VAR buf: Buffer;  len: INTEGER) =
  VAR n := NUMBER (buf^);
  BEGIN
    WHILE (n < len) DO INC (n, n); END;
    buf := NEW (Buffer, n);
  END ExpandBuf;


(*---------------------------------------------------------------- misc ---*)

PROCEDURE IsDirectory (file: TEXT): BOOLEAN =
  BEGIN
    TRY
      WITH stat = FS.Status (file) DO
        RETURN stat.type = FS.DirectoryFileType;
      END
    EXCEPT
    | OSError.E => RETURN FALSE;
    END
  END IsDirectory;

PROCEDURE OSErr (args: AtomList.T): TEXT =
  VAR msg : TEXT := NIL;
  BEGIN
    WHILE (args # NIL) DO
      IF (msg = NIL) THEN  msg := ": ";  ELSE  msg := msg & "  ***  ";  END;
      msg  := msg & Atom.ToText (args.head);
      args := args.tail;
    END;
    IF (msg = NIL) THEN msg := ": ** NO INFO **"; END;
    RETURN msg;
  END OSErr;

PROCEDURE Out (a, b, c, d: TEXT := NIL) =
  <*FATAL ANY*>
  VAR wr := Stdio.stdout;
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    IF (d # NIL) THEN Wr.PutText (wr, d); END;
    Wr.PutText (wr, Wr.EOL);
    Wr.Flush (wr);
  END Out;

BEGIN
  FOR i := FIRST (WhiteSpace) TO LAST (WhiteSpace) DO
    WhiteSpace[i] := FALSE;
  END;
  WhiteSpace [ORD(' ')]  := TRUE;
  WhiteSpace [ORD('\t')] := TRUE;
  WhiteSpace [ORD('\r')] := TRUE;
  WhiteSpace [ORD('\n')] := TRUE;
  WhiteSpace [ORD('\f')] := TRUE;

  IF Params.Count # 3 THEN
    Out ("usage:  cmpdir <dir1> <dir2>");
  ELSE
    Compare (Params.Get (1), Params.Get (2));
  END;
END Main.
