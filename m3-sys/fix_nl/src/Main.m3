(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE Main;

IMPORT Atom, AtomList, File, Fmt, FS, OSError, OS, Params, Pathname;
IMPORT Stdio, Text, Thread, Wr;

CONST
  KnownSourceSuffixes = ARRAY OF TEXT {
    ".i3", ".m3", ".ig", ".mg", "m3makefile", "m3overrides", "COPYRIGHT",
    ".c", ".h", ".bat", ".html", ".htm", ".el", ".s", ".asm", ".txt",
    ".tmpl", "README", ".mx", ".m3x", ".M3WEB", ".M3EXPORTS", ".M3SHIP",
    ".tex", ".lsl", ".lm3"
  };

CONST
  KnownBinarySuffixes = ARRAY OF TEXT {
    ".mo", ".io", "_i.o", "_m.o", ".o", ".obj", ".exe",
    ".a", ".sa", ".lib", ".so", ".dll", ".", "..", ".ps",
    ".gif", ".dvi", ".lect", ".pdf"
  };

VAR
  on_unix : BOOLEAN;
  Map     : ARRAY CHAR OF CHAR; (* for filename comparisons *)
  verbose : BOOLEAN := FALSE;

PROCEDURE Fix (path: TEXT;  top_level: BOOLEAN) =
  BEGIN
    IF top_level AND Text.Equal (path, "-verbose") THEN
      verbose := TRUE;
    ELSIF OS.IsDirectory (path) THEN
      FixDir (path);
    ELSIF (top_level) OR AnyMatch (path, KnownSourceSuffixes) THEN
      IF verbose THEN Err ("fixing: ", path); END;
      FixFile (path);
    ELSIF AnyMatch (path, KnownBinarySuffixes) THEN
      (* silently skip over known binary files *)
      IF verbose THEN Err ("skipping: ", path); END;
    ELSE
      Err ("unrecognized file: ", path, ", ignored.");
    END;
  END Fix;

PROCEDURE FixDir (dir: TEXT) =
  VAR iter: FS.Iterator;  nm: TEXT;
  BEGIN
    TRY
      iter := FS.Iterate (dir);
      WHILE iter.next (nm) DO
        Fix (Pathname.Join (dir, nm, NIL), top_level := FALSE);
      END;
    EXCEPT OSError.E (ec) =>
      Err ("trouble scanning: ", dir, OSErr (ec));
    END;
  END FixDir;

CONST
  CR = ORD ('\r');
  LF = ORD ('\n');
  NL = ORD ('\n');

TYPE
  Buffer = REF ARRAY OF File.Byte;

VAR
  inbuf  := NEW (Buffer, 40000);
  outbuf := NEW (Buffer, 60000);

PROCEDURE FixFile (path: TEXT) =
  VAR
    f: File.T;
    stat: File.Status;
    in_len: INTEGER;
    out_len: INTEGER;
    saw_return: BOOLEAN;
  BEGIN
    (* inhale the file *)
    TRY
      f := FS.OpenFileReadonly (path);
      TRY
        stat := f.status ();
        IF (stat.size <= 0) THEN RETURN; END;
        MakeRoom (stat.size);
        in_len := f.read (SUBARRAY (inbuf^, 0, stat.size), mayBlock := TRUE);
        IF (in_len # stat.size) THEN
          Err ("unable to read: ", path, ": expected " & Fmt.Int (stat.size)
               & " bytes, but got " & Fmt.Int (in_len));
          RETURN;
        END;
      FINALLY
        f.close ();
      END;
    EXCEPT OSError.E (ec) =>
      Err ("trouble reading: ", path, OSErr (ec));
      RETURN;
    END;

    (* process the bytes *)
    IF on_unix THEN (* => convert cr-lf pairs to newlines *)
      out_len := 0;
      FOR i := 0 TO in_len-1 DO
        IF (inbuf[i] = CR) AND (i < in_len-1) AND (inbuf[i+1] = LF) THEN
          (* skip this carriage return *)
        ELSE
          outbuf[out_len] := inbuf[i];  INC (out_len);
        END;
      END;
    ELSE (* windows => convert singleton newlines to cr-lf pairs *)
      out_len := 0;  saw_return := FALSE;
      FOR i := 0 TO in_len-1 DO
        IF (inbuf[i] = CR) THEN
          outbuf[out_len] := inbuf[i];  INC (out_len);
          saw_return := TRUE;
        ELSIF (inbuf[i] = NL) THEN
          IF NOT saw_return THEN  outbuf[out_len] := CR;  INC (out_len);  END;
          outbuf[out_len] := inbuf[i];  INC (out_len);
          saw_return := FALSE;
        ELSE
          outbuf[out_len] := inbuf[i];  INC (out_len);
          saw_return := FALSE;
        END;
      END;
    END;

    IF (in_len = out_len) THEN
      (* no changes! *)
      IF verbose THEN Err (" => no changes"); END;
      RETURN;
    ELSE
      IF verbose THEN
        Err (" => mapping from ", Fmt.Int (in_len),
             " to " & Fmt.Int (out_len) & " bytes");
      END;
    END;

    (* write the file and fix-up the time stamps *)
    TRY
      f := FS.OpenFile (path);
      TRY
        IF (out_len > 0) THEN
          f.write (SUBARRAY (outbuf^, 0, out_len));
        END;
      FINALLY
        OS.Close (f, stat.modificationTime, path);
      END;
    EXCEPT OSError.E (ec) =>
      Err ("trouble writing: ", path, OSErr (ec));
    END;
  END FixFile;

PROCEDURE MakeRoom (file_size: INTEGER) =
  VAR len := NUMBER (inbuf^);
  BEGIN
    WHILE (file_size > len) DO INC (len, len); END;
    IF len # NUMBER (inbuf^) THEN
      inbuf  := NEW (Buffer, len);
      outbuf := NEW (Buffer, (3 * len) DIV 2);
    END;
  END MakeRoom;

PROCEDURE AnyMatch (path: TEXT;  READONLY suffixes: ARRAY OF TEXT): BOOLEAN =
  BEGIN
    FOR i := FIRST (suffixes) TO LAST (suffixes) DO
      IF SuffixMatch (path, suffixes[i]) THEN RETURN TRUE; END;
    END;
    RETURN FALSE;
  END AnyMatch;
  
PROCEDURE SuffixMatch (path, suffix: TEXT): BOOLEAN =
  VAR
    len    := Text.Length (path);
    slen   := Text.Length (suffix);
    offset : INTEGER;
  BEGIN
    IF len >= slen THEN
      offset := len - slen;
      FOR i := 0 TO slen-1 DO
        IF Map [Text.GetChar(path, i+offset)] # 
	   Map [Text.GetChar(suffix, i)] THEN
          RETURN FALSE;
        END;
      END;
    END;
    RETURN TRUE;
  END SuffixMatch;

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

PROCEDURE Err (a, b, c: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted *>
  VAR wr := Stdio.stdout;
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    Wr.PutText (wr, Wr.EOL);
    Wr.Flush (wr);
  END Err;

BEGIN
  on_unix  := Text.Equal ("a/b", Pathname.Join ("a", "b", NIL));

  FOR c := FIRST (Map) TO LAST (Map) DO  Map [c] := c;  END;
  IF NOT on_unix THEN (* windows has case insensitive file names *)
    FOR c := 'a' TO 'z' DO
      Map [c] := VAL (ORD (c) - ORD ('a') + ORD ('A'), CHAR);
    END;
  END;

  FOR i := 1 TO Params.Count-1 DO
    Fix (Params.Get (i), top_level := TRUE);
  END;
END Main.
