UNSAFE MODULE Main;

IMPORT IO, Params, Rd, Wr, FileRd, Process, WinNT, WinDef, Stdio;
IMPORT CoffTime, Text, Fmt, Cstring, Thread, Word, OSError;
IMPORT Date, Convert;


TYPE
  Header = WinNT.IMAGE_ARCHIVE_MEMBER_HEADER;
  BYTE   = WinDef.BYTE;
  Buffer = REF ARRAY OF CHAR;

VAR
  exports1  : Buffer := NIL;
  exports2  : Buffer := NIL;
  longnames : Buffer := NIL;

PROCEDURE DumpLib (lib: TEXT) =
  VAR rd: Rd.T;
  BEGIN
    TRY
      rd := FileRd.Open (lib);
    EXCEPT OSError.E =>
      Die ("unable to open \"" & lib & "\".");
    END;

    TRY
      CheckMagic (rd, lib);
      FOR i := 0 TO LAST(INTEGER) DO
        IF Rd.EOF (rd) THEN EXIT; END;
        DumpMember (rd, i);
      END;
      DumpExports1 ();
      DumpExports2 ();
      Wr.Flush (Stdio.stdout);
      Rd.Close (rd);
    EXCEPT
    | Rd.Failure => Die ("I/O read failure.");
    | Wr.Failure => Die ("I/O write failure.");
    | Thread.Alerted => Die ("interrupted.");
    END;
  END DumpLib;

PROCEDURE CheckMagic (rd: Rd.T;  lib: TEXT)
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR buf: ARRAY [0..WinNT.IMAGE_ARCHIVE_START_SIZE-1] OF CHAR;
  BEGIN
    Read (rd, buf);
    IF Cstring.strncmp (ADR (buf), WinNT.IMAGE_ARCHIVE_START,
                        WinNT.IMAGE_ARCHIVE_START_SIZE) = 0 THEN
      IO.Put (Wr.EOL & lib & ": magic ok." & Wr.EOL & Wr.EOL);
      IO.Put (" offset  UserID  GrpID   Mode           Date            Size    Name" & Wr.EOL);
      IO.Put ("-------- ------ ------ -------- -------------------- ---------- ----------------" & Wr.EOL);
    ELSE
      Die ("\"" & lib & "\" is not an archive (wrong magic number).");
    END;
  END CheckMagic;

PROCEDURE DumpMember (rd: Rd.T;  index: INTEGER)
  RAISES {Rd.Failure, Thread.Alerted, Wr.Failure} =
  VAR
    hdr: Header;
    here := Rd.Index (rd);
    nm_offs : INTEGER;
    size    : INTEGER;
  BEGIN
    Read (rd, LOOPHOLE (hdr, ARRAY [0..BYTESIZE(hdr)-1] OF CHAR));

    OutX (here, 8);      OutC (' ');
    OutS (hdr.UserID);   OutC (' ');
    OutS (hdr.GroupID);  OutC (' ');
    OutS (hdr.Mode);     OutC (' ');
    OutD (hdr.Date);     OutC (' ');
    OutS (hdr.Size);     OutC (' ');
    IF (hdr.Name[0] = ORD ('/'))
      AND (ORD ('0') <= hdr.Name[1]) AND (hdr.Name[1] <= ORD ('9'))
      AND (longnames # NIL) THEN
      nm_offs := ToInt (SUBARRAY (hdr.Name, 1, NUMBER(hdr.Name)-1));
      IF (0 <= nm_offs) AND (nm_offs < NUMBER (longnames^))
        THEN OutSP (ADR (longnames[nm_offs]));
        ELSE OutSL (hdr.Name);
      END;
    ELSE
      OutSL (hdr.Name);
    END;
    OutT (Wr.EOL);

    (* check for special members and skip to the next header *)
    INC (here, BYTESIZE (hdr));
    size := ToInt (hdr.Size);
    size := Word.And (size + 1, Word.Not (1)); (* round up to an even size *)
    IF (index > 2) THEN
      (* skip this file *)
      Rd.Seek (rd, here + size);
    ELSIF (index = 0) AND NameMatch (hdr.Name, "/") THEN
      exports1 := NEW (Buffer, size);
      Read (rd, exports1^);
    ELSIF (index = 1) AND NameMatch (hdr.Name, "/") THEN
      exports2 := NEW (Buffer, size);
      Read (rd, exports2^);
    ELSIF (index <= 2) AND NameMatch (hdr.Name, "//") THEN
      longnames := NEW (Buffer, size);
      Read (rd, longnames^);
    ELSE
      (* skip this file *)
      Rd.Seek (rd, here + size);
    END;
  END DumpMember;

PROCEDURE DumpExports1 ()
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    n_syms : INTEGER;
    offs   : REF ARRAY OF INTEGER;
    next_c : INTEGER := 0;
    ch     : CHAR;
  BEGIN
    IF (exports1 = NIL) THEN RETURN; END;

    OutT (Wr.EOL);
    OutT ("Export table #1");  OutT (Wr.EOL);

    n_syms := get_be_int (exports1, next_c);
    OutT ("# symbols = ");  OutI (n_syms, 0);  OutT (Wr.EOL);
    OutT ("---------------------------");  OutT (Wr.EOL);

    (* read the file offsets *)
    offs := NEW (REF ARRAY OF INTEGER, n_syms);
    FOR i := 0 TO n_syms-1 DO
      offs[i] := get_be_int (exports1, next_c);
    END;
    
    (* finally, dump the exports *)
    FOR i := 0 TO n_syms-1 DO
      OutI (i, 4);        OutC (' ');
      OutX (offs[i], 8);  OutC (' ');
      WHILE (next_c < NUMBER (exports1^)) DO
        ch := exports1[next_c];  INC (next_c);
        IF (ch = '\000') THEN EXIT; END;
        OutC (ch);
      END;
      OutT (Wr.EOL);
    END;
  END DumpExports1;

PROCEDURE get_be_int (buf: Buffer;  VAR cur: INTEGER): INTEGER =
  VAR n := 0;  x := NUMBER (buf^);
  BEGIN
    IF (cur < x) THEN
      n := ORD (buf[cur]);  INC (cur);
    END;
    IF (cur < x) THEN
      n := Word.Or (Word.LeftShift (n, 8), ORD (buf[cur]));
      INC (cur);
    END;
    IF (cur < x) THEN
      n := Word.Or (Word.LeftShift (n, 8), ORD (buf[cur]));
      INC (cur);
    END;
    IF (cur < x) THEN
      n := Word.Or (Word.LeftShift (n, 8), ORD (buf[cur]));
      INC (cur);
    END;
    RETURN n;
  END get_be_int;

PROCEDURE DumpExports2 ()
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    n_files : INTEGER;
    offs    : REF ARRAY OF INTEGER;
    n_syms  : INTEGER;
    symfile : REF ARRAY OF INTEGER;
    next_c  : INTEGER := 0;
    ch      : CHAR;
  BEGIN
    IF (exports2 = NIL) THEN RETURN; END;

    OutT (Wr.EOL);
    OutT ("Export table #2");  OutT (Wr.EOL);

    n_files := get_le_int (exports2, next_c);
    OutT ("# files = ");  OutI (n_files, 0);  OutT (Wr.EOL);

    (* read the file offsets *)
    offs := NEW (REF ARRAY OF INTEGER, n_files);
    FOR i := 0 TO n_files-1 DO
      offs[i] := get_le_int (exports2, next_c);
    END;

    n_syms := get_le_int (exports2, next_c);
    OutT ("# symbols = ");  OutI (n_syms, 0);  OutT (Wr.EOL);
    OutT ("---------------------------");  OutT (Wr.EOL);

    (* read the symbol indicies *)
    symfile := NEW (REF ARRAY OF INTEGER, n_syms);
    FOR i := 0 TO n_syms-1 DO
      symfile[i] := get_le_short (exports2, next_c);
    END;
    
    (* finally, dump the exports *)
    FOR i := 0 TO n_syms-1 DO
      OutI (i, 4);           OutC (' ');
      OutI (symfile[i], 4);  OutC (' ');
      IF (1 <= symfile[i]) AND (symfile[i] <= NUMBER (offs^))
        THEN OutX (offs[symfile[i] - 1], 8);  OutC (' ');
        ELSE OutT ("   ***** ");
      END;
      WHILE (next_c < NUMBER (exports2^)) DO
        ch := exports2[next_c];  INC (next_c);
        IF (ch = '\000') THEN EXIT; END;
        OutC (ch);
      END;
      OutT (Wr.EOL);
    END;
  END DumpExports2;

PROCEDURE get_le_int (buf: Buffer;  VAR cur: INTEGER): INTEGER =
  VAR n := 0;  x := NUMBER (buf^);
  BEGIN
    IF (cur < x) THEN
      n := ORD (buf[cur]);  INC (cur);
    END;
    IF (cur < x) THEN
      n := Word.Or (n, Word.LeftShift (ORD (buf[cur]), 8));
      INC (cur);
    END;
    IF (cur < x) THEN
      n := Word.Or (n, Word.LeftShift (ORD (buf[cur]), 16));
      INC (cur);
    END;
    IF (cur < x) THEN
      n := Word.Or (n, Word.LeftShift (ORD (buf[cur]), 24));
      INC (cur);
    END;
    RETURN n;
  END get_le_int;

PROCEDURE get_le_short (buf: Buffer;  VAR cur: INTEGER): INTEGER =
  VAR n := 0;  x := NUMBER (buf^);
  BEGIN
    IF (cur < x) THEN
      n := ORD (buf[cur]);  INC (cur);
    END;
    IF (cur < x) THEN
      n := Word.Or (n, Word.LeftShift (ORD (buf[cur]), 8));
      INC (cur);
    END;
    RETURN n;
  END get_le_short;

PROCEDURE Read (rd: Rd.T;  VAR buf: ARRAY OF CHAR)
  RAISES {Rd.Failure, Thread.Alerted} =
  VAR len: INTEGER;
  BEGIN
    len := Rd.GetSub (rd, buf);
    IF (len # BYTESIZE (buf)) THEN Die ("incomplete read."); END;
  END Read;

PROCEDURE OutS (READONLY s: ARRAY OF BYTE)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR n := NUMBER (s);
  BEGIN
    WHILE (n > 0) AND (s[n-1] = ORD (' ')) DO OutC (' ');  DEC (n); END;
    FOR i := 0 TO n-1 DO OutC (VAL (s[i], CHAR)); END;
  END OutS;

PROCEDURE OutSL (READONLY s: ARRAY OF BYTE)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    FOR i := 0 TO LAST (s) DO
      IF (s[i] = ORD ('/')) THEN EXIT; END;
      OutC (VAL (s[i], CHAR));
    END;
  END OutSL;

PROCEDURE OutX (n: INTEGER;  width: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Wr.PutText (Stdio.stdout, Fmt.Pad (Fmt.Unsigned (n), width, '0'));
  END OutX;

PROCEDURE OutI (n: INTEGER;  width: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Wr.PutText (Stdio.stdout, Fmt.Pad (Fmt.Int (n), width));
  END OutI;

PROCEDURE OutD (READONLY s: ARRAY OF BYTE)
  RAISES {Wr.Failure, Thread.Alerted} =
  (* Archive dates are represented as seconds since Jan 1, 1970. *)
  TYPE Buffer = RECORD len: INTEGER;  buf: ARRAY [0..31] OF CHAR END;
  VAR
    secs := ToInt (s);
    time := CoffTime.EpochAdjust + FLOAT (secs, LONGREAL);
    date := Date.FromTime (time);
    b    : Buffer;

  PROCEDURE AddInt (VAR b: Buffer;  value, width: INTEGER;  pre, post: CHAR) =
    <*FATAL Convert.Failed*>
    VAR
      buf : ARRAY [0..BITSIZE(INTEGER)] OF CHAR;
      len := Convert.FromInt (buf, value);
    BEGIN
      WHILE (width > len) DO
        b.buf[b.len] := pre;  INC (b.len);
        DEC (width);
      END;
      FOR i := 0 TO len-1 DO
        b.buf[b.len] := buf[i];  INC (b.len);
      END;
      b.buf[b.len] := post;  INC (b.len);
    END AddInt;

  BEGIN (* OutD *)
    b.len := 0;
    AddInt  (b, date.hour,   2, '0', ':');
    AddInt  (b, date.minute, 2, '0', ':');
    AddInt  (b, date.second, 2, '0', ' ');
    AddInt  (b, ORD (date.month) + 1, 2, '0', '/');
    AddInt  (b, date.day,    2, '0', '/');
    AddInt  (b, date.year,   4, ' ', ' ');
    Wr.PutString (Stdio.stdout, SUBARRAY (b.buf, 0, b.len-1));
  END OutD;

PROCEDURE OutSP (cp: UNTRACED REF CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (cp # NIL) THEN
      WHILE (cp^ # '\000') DO
        OutC (cp^);
        INC (cp, ADRSIZE (cp^));
      END;
    END;
  END OutSP;

PROCEDURE OutT (txt: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Wr.PutText (Stdio.stdout, txt);
  END OutT;

PROCEDURE OutC (ch: CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    Wr.PutChar (Stdio.stdout, ch);
  END OutC;

PROCEDURE NameMatch (READONLY nm: ARRAY OF BYTE;  txt: TEXT): BOOLEAN =
  VAR len := Text.Length (txt);
  BEGIN
    IF len > NUMBER (nm) THEN RETURN FALSE; END;
    FOR i := 0 TO len-1 DO
      IF nm[i] # ORD (Text.GetChar (txt, i)) THEN RETURN FALSE; END;
    END;
    FOR i := len TO LAST (nm) DO
      IF nm[i] # ORD (' ') THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END NameMatch;

PROCEDURE ToInt (READONLY s: ARRAY OF BYTE): INTEGER =
  VAR n := 0;
  BEGIN
    FOR i := FIRST (s) TO LAST (s) DO
      IF (ORD ('0') <= s[i]) AND (s[i] <= ORD ('9')) THEN
        n := n * 10 + (s[i] - ORD ('0'));
      END;
    END;
    RETURN n;
  END ToInt;

PROCEDURE Die (msg: TEXT) =
  BEGIN
    IO.Put (msg);
    IO.Put (Wr.EOL);
    Process.Exit (1);
  END Die;

BEGIN
  <*ASSERT BYTESIZE (Header) = WinNT.IMAGE_SIZEOF_ARCHIVE_MEMBER_HDR *>
  IF Params.Count # 2 THEN Die ("usage:  libdump <foo.lib>"); END;
  DumpLib (Params.Get (1));
END Main.
