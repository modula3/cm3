(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE LibFile;

IMPORT Process, IO, Wr, FileWr, Thread, OSError, TextRefTbl;
IMPORT WinNT, Convert, FS, Text, Word, TextWr;
IMPORT Fmt, IntArraySort, WinDef;

CONST
  ArchiveMagic = "!<arch>\n";
  EndHeader    = "`\n";
  PadChar      = '\n';

TYPE
  Header = WinNT.IMAGE_ARCHIVE_MEMBER_HEADER;
  BYTE   = WinDef.BYTE;

TYPE
  FileDesc = REF RECORD
    next     : FileDesc := NIL;
    obj      : Obj      := NIL;
    tag      : TEXT     := NIL;   (* short name that fits in a header *)
    size     : INTEGER  := 0;     (* file length *)
    index    : INTEGER  := 0;     (* ordinal index of file in the global list *)
    offset   : INTEGER  := 0;     (* final offset of the file in the lib *)
  END;

TYPE
  ExportDesc = REF RECORD
    next   : ExportDesc;
    symbol : TEXT;
    file   : FileDesc;
  END;

TYPE
  State = RECORD
    lib_wr      : Wr.T         := NIL;
    lib_name    : TEXT         := NIL;
    lib_time    : INTEGER      := 0;
    keep_size   : CARDINAL     := 0;
    n_files     : CARDINAL     := 0;
    files       : FileDesc     := NIL;
    n_exports   : CARDINAL     := 0;
    exports     : ExportDesc   := NIL;
    export_vec  : REF ARRAY OF ExportDesc := NIL;
    export_map  : REF ARRAY OF INTEGER := NIL;
    export_len  : CARDINAL     := 0;
    export_tbl  : TextRefTbl.T := NIL;
    long_nms    : TextWr.T     := NIL;
  END;

PROCEDURE Gen (nm: TEXT;  contents: Obj) =
  VAR s: State;
  BEGIN
    s.lib_name := nm;
    ScanFiles (s, contents);
    AssignFileOffsets (s);
    SortExports (s);
    WriteLib (s);
  END Gen;

(*------------------------------------------------------ file scanning ---*)

PROCEDURE ScanFiles (VAR s: State;  obj: Obj) =
  VAR f, last_f: FileDesc;
  BEGIN
    (* scan through the files & their exports *)
    WHILE (obj # NIL) DO
      f := NEW (FileDesc, obj := obj);
      f.size := Text.Length (f.obj.body);
      GenTag (s, f);

      IF (s.files = NIL) THEN s.files := f; ELSE last_f.next := f; END;
      last_f := f;
      INC (s.n_files);

      IF (obj.symbols # NIL) THEN
        FOR i := 0 TO LAST (obj.symbols^) DO
          AddExport (s, obj.symbols[i], f);
        END;
      END;

      s.lib_time := MAX (s.lib_time, obj.time);

      obj := obj.next;
    END;
  END ScanFiles;

PROCEDURE GenTag (VAR s: State;  f: FileDesc) =
  VAR hdr: Header;  nm := f.obj.filename;
  BEGIN
    (* make sure the name fits... *)
    IF Text.Length (nm) + 1 <= NUMBER (hdr.Name) THEN
      f.tag := nm & "/";
    ELSE
      IF (s.long_nms = NIL) THEN s.long_nms := TextWr.New (); END;
      <*FATAL Wr.Failure, Thread.Alerted*>
      VAR offs := Wr.Index (s.long_nms);
      BEGIN
        Wr.PutText (s.long_nms, nm);
        Wr.PutChar (s.long_nms, '\000');
        f.tag := "/" & Fmt.Int (offs);
      END;
    END;
  END GenTag;

PROCEDURE AddExport (VAR s: State;  sym: TEXT;  f: FileDesc) =
  VAR ref: REFANY; f2: FileDesc;
  BEGIN
    IF (s.export_tbl = NIL) THEN
      s.export_tbl := NEW (TextRefTbl.Default).init ();
    END;
    IF s.export_tbl.get (sym, ref) THEN
      f2 := ref;
      Warn ("symbol \"", sym, "\" is exported twice:" & Wr.EOL,
              "  " & f2.obj.filename & "  (using this instance)" & Wr.EOL,
              "  " & f.obj.filename  & "  (ignoring this instance)");
    ELSE
      (* a new symbol *)
      EVAL s.export_tbl.put (sym, f);
      s.exports := NEW (ExportDesc, next := s.exports, symbol := sym, file := f);
      INC (s.n_exports);
      INC (s.export_len, Text.Length (sym) + 1);
    END;
  END AddExport;

(*---------------------------------------------------- LIB file layout ---*)

PROCEDURE AssignFileOffsets (VAR s: State) =
  CONST Hdr = BYTESIZE (Header);
  VAR f := s.files;  offs := 0;  index := 1;
  BEGIN
    INC (offs, Text.Length (ArchiveMagic));

    IF (s.n_exports > 0) THEN
      (* export table 1 *)
      INC (offs, Hdr + 4 * (s.n_exports + 1) + s.export_len);
      IF (offs MOD 2 # 0) THEN INC (offs); END;
    END;

    IF (s.n_exports > 0) THEN
      (* export table 2 *)
      INC (offs, Hdr + 4 * (s.n_files + 2) + 2 * s.n_exports + s.export_len);
      IF (offs MOD 2 # 0) THEN INC (offs); END;
    END;

    IF (s.long_nms # NIL) THEN
      (* long filename table *)
      INC (offs, Hdr + Wr.Index (s.long_nms));
      IF (offs MOD 2 # 0) THEN INC (offs); END;
    END;

    (* record the final offset for each "real" file *)
    WHILE (f # NIL) DO
      f.index  := index;  INC (index);
      f.offset := offs;   INC (offs, Hdr + f.size);
      IF (offs MOD 2 # 0) THEN INC (offs); END;
      f := f.next;
    END;
  END AssignFileOffsets;

(*------------------------------------------------------------ sorting ---*)

PROCEDURE SortExports (VAR s: State) =
  VAR e := s.exports;

  PROCEDURE CmpExport (a, b: INTEGER): [-1..+1] =
    BEGIN
      RETURN Text.Compare (s.export_vec [a].symbol, s.export_vec[b].symbol);
    END CmpExport;

  BEGIN
    IF (e = NIL) THEN RETURN; END;
    s.export_vec := NEW (REF ARRAY OF ExportDesc, s.n_exports);
    s.export_map := NEW (REF ARRAY OF INTEGER, s.n_exports);
    FOR i := 0 TO s.n_exports-1 DO
      s.export_vec[i] := e;  e := e.next;
      s.export_map[i] := i;
    END;
    IntArraySort.Sort (s.export_map^, CmpExport);
  END SortExports;

(*--------------------------------------------------------- LIB writer ---*)

PROCEDURE WriteLib (VAR s: State) =
  BEGIN
    (* open the output file *)
    TRY
      s.lib_wr := FileWr.Open (s.lib_name);
    EXCEPT OSError.E =>
      Die (s, "unable to open \"", s.lib_name, "\" for writing.");
    END;

    (* write the archive... *)
    TRY
      Wr.PutText (s.lib_wr, ArchiveMagic);
      DumpExports1 (s);
      DumpExports2 (s);
      DumpLongNames (s);
      DumpFiles (s);
    EXCEPT
    | Wr.Failure =>
        Die (s, "I/O failure while writing \"", s.lib_name, "\".");
    | Thread.Alerted =>
        Die (s, "interrupted while writing \"", s.lib_name, "\".");
    END;

    TRY
      Wr.Close (s.lib_wr);
    EXCEPT Wr.Failure, Thread.Alerted =>
      Die (s, "unable to close \"", s.lib_name, "\".");
    END;
  END WriteLib;

PROCEDURE DumpExports1 (VAR s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR len, odd: INTEGER;  e: ExportDesc;
  BEGIN
    IF (s.n_exports <= 0) THEN RETURN; END;
    len := 4 * (s.n_exports + 1) + s.export_len;
    odd := s.export_len MOD 2;
    WriteHeader (s, "/", "0", s.lib_time, len);
    WriteBE4 (s, s.n_exports);
    FOR i := 0 TO s.n_exports-1 DO
      e := s.export_vec [s.export_map [i]];
      WriteBE4 (s, e.file.offset);
    END;
    FOR i := 0 TO s.n_exports-1 DO
      e := s.export_vec [s.export_map [i]];
      Wr.PutText (s.lib_wr, e.symbol);
      Wr.PutChar (s.lib_wr, '\000');
    END;
    IF (odd # 0) THEN Wr.PutChar (s.lib_wr, PadChar); END;
  END DumpExports1;

PROCEDURE WriteBE4 (VAR s: State;  n: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  (* write the little-endian 4-byte value 'n' *)
  BEGIN
    Wr.PutChar (s.lib_wr, VAL (Word.And (Word.RightShift (n, 24), 16_ff), CHAR));
    Wr.PutChar (s.lib_wr, VAL (Word.And (Word.RightShift (n, 16), 16_ff), CHAR));
    Wr.PutChar (s.lib_wr, VAL (Word.And (Word.RightShift (n, 8), 16_ff), CHAR));
    Wr.PutChar (s.lib_wr, VAL (Word.And (n, 16_ff), CHAR));
  END WriteBE4;

PROCEDURE DumpExports2 (VAR s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR len, odd: INTEGER;  e: ExportDesc;  f := s.files;
  BEGIN
    IF (s.n_exports <= 0) THEN RETURN; END;
    len := 4 * (s.n_files + 2) + 2 * (s.n_exports) + s.export_len;
    odd := s.export_len MOD 2;
    WriteHeader (s, "/", "0", s.lib_time, len);
    WriteLE4 (s, s.n_files);
    FOR i := 0 TO s.n_files-1 DO
      WriteLE4 (s, f.offset);
      f := f.next;
    END;
    WriteLE4 (s, s.n_exports);
    FOR i := 0 TO s.n_exports-1 DO
      e := s.export_vec [s.export_map [i]];
      WriteLE2 (s, e.file.index);
    END;
    FOR i := 0 TO s.n_exports-1 DO
      e := s.export_vec [s.export_map [i]];
      Wr.PutText (s.lib_wr, e.symbol);
      Wr.PutChar (s.lib_wr, '\000');
    END;
    IF (odd # 0) THEN Wr.PutChar (s.lib_wr, PadChar); END;
  END DumpExports2;

PROCEDURE WriteLE4 (VAR s: State;  n: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  (* write the little-endian 4-byte value 'n' *)
  BEGIN
    Wr.PutChar (s.lib_wr, VAL (Word.And (n, 16_ff), CHAR));
    Wr.PutChar (s.lib_wr, VAL (Word.And (Word.RightShift (n, 8), 16_ff), CHAR));
    Wr.PutChar (s.lib_wr, VAL (Word.And (Word.RightShift (n, 16), 16_ff), CHAR));
    Wr.PutChar (s.lib_wr, VAL (Word.And (Word.RightShift (n, 24), 16_ff), CHAR));
  END WriteLE4;

PROCEDURE WriteLE2 (VAR s: State;  n: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  (* write the little-endian 2-byte value 'n' *)
  BEGIN
    Wr.PutChar (s.lib_wr, VAL (Word.And (n, 16_ff), CHAR));
    Wr.PutChar (s.lib_wr, VAL (Word.And (Word.RightShift (n, 8), 16_ff), CHAR));
  END WriteLE2;

PROCEDURE DumpLongNames (VAR s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR len, odd: INTEGER;
  BEGIN
    IF (s.long_nms = NIL) THEN RETURN; END;
    len := Wr.Index (s.long_nms);
    odd := len MOD 2;
    WriteHeader (s, "//", "0", s.lib_time, len);
    Wr.PutText (s.lib_wr, TextWr.ToText (s.long_nms));
    IF (odd # 0) THEN Wr.PutChar (s.lib_wr, PadChar); END;
  END DumpLongNames;

PROCEDURE DumpFiles (VAR s: State)
  RAISES {Wr.Failure, Thread.Alerted} =
  (* dump the "real" files *)
  VAR f := s.files;
  BEGIN
    WHILE (f # NIL) DO
      DumpFile (s, f);
      f := f.next;
    END;
  END DumpFiles;

PROCEDURE DumpFile (VAR s: State;  f: FileDesc)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    WriteHeader (s, f.tag, "0" (**"100666"**), f.obj.time, f.size);
    Wr.PutText (s.lib_wr, f.obj.body);
    IF (f.size MOD 2 # 0) THEN
      Wr.PutChar (s.lib_wr, PadChar);
    END;
  END DumpFile;

PROCEDURE WriteHeader (VAR s: State;  nm: TEXT;  mode: TEXT;
                       time: INTEGER;  size: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  TYPE HdrChars = ARRAY [0..BYTESIZE(Header)-1] OF CHAR;
  VAR hdr: Header;
  BEGIN
    StuffT (hdr.Name,      nm);
    StuffI (hdr.Date,      time);
    StuffT (hdr.UserID,    "");
    StuffT (hdr.GroupID,   "");
    StuffT (hdr.Mode,      mode);
    StuffI (hdr.Size,      size);
    StuffT (hdr.EndHeader, EndHeader);

    Wr.PutString (s.lib_wr, LOOPHOLE (hdr, HdrChars));
  END WriteHeader;

PROCEDURE StuffI (VAR b: ARRAY OF BYTE;  n: INTEGER) =
  <*FATAL Convert.Failed*>
  VAR
    buf : ARRAY [0..BITSIZE(INTEGER)] OF CHAR;
    len := Convert.FromInt (buf, n);
  BEGIN
    FOR i := 0 TO MIN (len - 1, LAST (b)) DO
      b[i] := ORD (buf[i]);
    END;
    FOR i := len TO LAST (b) DO
      b[i] := ORD (' ');
    END;
  END StuffI;

PROCEDURE StuffT (VAR b: ARRAY OF BYTE;  txt: TEXT) =
  VAR len := Text.Length (txt);
  BEGIN
    FOR i := 0 TO MIN (len - 1, LAST (b)) DO
      b[i] := ORD (Text.GetChar (txt, i));
    END;
    FOR i := len TO LAST (b) DO
      b[i] := ORD (' ');
    END;
  END StuffT;

(*--------------------------------------------------------------- misc ---*)

PROCEDURE Warn (a, b, c, d, e: TEXT := NIL) =
  BEGIN
    IO.Put ("warning: ");
    IF (a # NIL) THEN IO.Put (a); END;
    IF (b # NIL) THEN IO.Put (b); END;
    IF (c # NIL) THEN IO.Put (c); END;
    IF (d # NIL) THEN IO.Put (d); END;
    IF (e # NIL) THEN IO.Put (e); END;
    IO.Put (Wr.EOL);
  END Warn;

PROCEDURE Die (VAR s: State;  a, b, c: TEXT := NIL) =
  BEGIN
    IF (a # NIL) THEN IO.Put (a); END;
    IF (b # NIL) THEN IO.Put (b); END;
    IF (c # NIL) THEN IO.Put (c); END;
    IO.Put (Wr.EOL);
    IF (s.lib_wr # NIL) THEN
      (* try to clean up by blowing away the bad output *)
      TRY Wr.Close (s.lib_wr); EXCEPT Wr.Failure, Thread.Alerted => END;
      TRY FS.DeleteFile (s.lib_name); EXCEPT OSError.E => END;
      s.lib_wr := NIL;
    END;
    Process.Exit (1);
  END Die;

BEGIN
END LibFile.
