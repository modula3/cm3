(* Copyright 1996, Critical Mass, Inc.  All rights reserved. *)

UNSAFE MODULE Main;

IMPORT Atom, AtomList, File, FileRd, Fingerprint, FS, OSError, Params, Pathname;
IMPORT Rd, Stdio, Text, Thread, Word, Wr;

PROCEDURE Gen (arg: TEXT) =
  BEGIN
    IF Text.GetChar (arg, 0) = '@' THEN
      GenFile (Text.Sub (arg, 1));
    ELSE
      Visit (arg);
    END;
  END Gen;

PROCEDURE GenFile (nm: TEXT) =
  VAR
    rd    : Rd.T;
    start : INTEGER;
    len   : INTEGER;
    buf   : ARRAY [0..255] OF CHAR;
  BEGIN
    TRY
      rd := FileRd.Open (nm);
      TRY
        WHILE NOT Rd.EOF (rd) DO
          len := Rd.GetSubLine (rd, buf);
          start := 0;
          WHILE (start < len) AND WhiteSpace [ORD (buf[start])] DO INC (start); END;
          WHILE (len > start) AND WhiteSpace [ORD (buf[len-1])] DO DEC (len); END;
          IF start < len THEN
            Visit (Text.FromChars (SUBARRAY (buf, start, len - start)));
          END;
        END;
      FINALLY
        Rd.Close (rd);
      END;
    EXCEPT
    | OSError.E (ec) =>
        Out ("** unable to open ", nm, OSErr (ec));
    | Rd.Failure (ec) =>
        Out ("** trouble reading ", nm, OSErr (ec));
    | Thread.Alerted =>
        Out ("** interrupted while reading ", nm);
    END;
  END GenFile;

PROCEDURE Visit (nm: TEXT) =
  BEGIN
    IF IsDirectory (nm)
      THEN VisitDir (nm);
      ELSE VisitFile (nm);
    END;
  END Visit;
  
(*--------------------------------------------------------- directories ---*)

PROCEDURE VisitDir (dir: TEXT) =
  VAR iter: FS.Iterator;  nm: TEXT;
  BEGIN
    TRY
      iter := FS.Iterate (dir);
      WHILE iter.next (nm) DO
        Visit (Pathname.Join (dir, nm, NIL));
      END;
    EXCEPT OSError.E (ec) =>
      Out ("trouble enumerating contents of ", dir, OSErr (ec));
    END;
    iter.close ();
  END VisitDir;

(*--------------------------------------------------------------- files ---*)

TYPE
  Buffer = REF ARRAY OF File.Byte;

VAR
  buf := NEW (Buffer, 10000);
  WhiteSpace: ARRAY File.Byte OF BOOLEAN;

PROCEDURE VisitFile (nm: TEXT) =
  VAR
    len := Inhale (nm, buf);
    ptr := LOOPHOLE (buf, REF ARRAY OF CHAR);
    fp  := Fingerprint.FromChars (SUBARRAY (ptr^, 0, len), Fingerprint.OfEmpty);
  BEGIN
    <*ASSERT BYTESIZE(CHAR) = BYTESIZE(File.Byte)*>
    Out (nm, " ", FPText (fp));
  END VisitFile;

PROCEDURE FPText (READONLY fp: Fingerprint.T): TEXT =
  CONST Digit = ARRAY [0..15] OF CHAR { '0','1','2','3','4','5','6','7',
                                        '8','9','a','b','c','d','e','f' };
  VAR buf: ARRAY [0..15] OF CHAR;  len := 0;
  BEGIN
    FOR i := 0 TO 7 DO
      buf [len] := Digit [Word.Extract (fp.byte[i], 4, 4)];  INC (len);
      buf [len] := Digit [Word.Extract (fp.byte[i], 0, 4)];  INC (len);
    END;
    RETURN Text.FromChars (buf);
  END FPText;

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

  FOR i := 1 TO Params.Count-1 DO
    Gen (Params.Get (i));
  END;
END Main.
