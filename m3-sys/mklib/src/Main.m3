(* Some parts copied from WinNT.i3:                     *)
(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

UNSAFE MODULE Main;

IMPORT Process, IO, Rd, Wr, FileRd, FileWr, Thread, OSError, TextRefTbl;
IMPORT Convert, CoffTime, File, FS, Text, Word, TextWr, TextSeq;
IMPORT Fmt, Time, IntArraySort, RegularFile, Params, Pathname;
IMPORT ASCII, Ctypes, TextLiteral, WinNT;

TYPE
  UINT8 = Ctypes.unsigned_char;
  UINT32 = Ctypes.unsigned_int;

CONST
  (* TODO keep everything in memory *)
  MaxKeeper    = TextLiteral.MaxBytes;  (* max file size we'll hold in memory *)
  MaxTotalKeep = MaxKeeper;             (* max total file space we'll hold in memory *)
  ArchiveMagic = "!<arch>\n";
  EndHeader    = "`\n";
  PadChar      = '\n';

  (* TODO Remove these when m3core updated *)
  IMAGE_FILE_MACHINE_I386      = 16_014c;
  IMAGE_FILE_MACHINE_R3000     = 16_0162;
  IMAGE_FILE_MACHINE_R4000     = 16_0166;
  IMAGE_FILE_MACHINE_R10000    = 16_0168;
  IMAGE_FILE_MACHINE_WCEMIPSV2 = 16_0169;
  IMAGE_FILE_MACHINE_ALPHA     = 16_0184;
  IMAGE_FILE_MACHINE_SH3       = 16_01a2;
  IMAGE_FILE_MACHINE_SH3DSP    = 16_01a3;
  IMAGE_FILE_MACHINE_SH3E      = 16_01a4;
  IMAGE_FILE_MACHINE_SH4       = 16_01a6;
  IMAGE_FILE_MACHINE_SH5       = 16_01a8;
  IMAGE_FILE_MACHINE_ARM       = 16_01c0;
  IMAGE_FILE_MACHINE_THUMB     = 16_01c2;
  IMAGE_FILE_MACHINE_ARMNT     = 16_01c4; (* ARM Thumb-2 Little-Endian *)
  IMAGE_FILE_MACHINE_AM33      = 16_01d3;
  IMAGE_FILE_MACHINE_POWERPC   = 16_01F0;
  IMAGE_FILE_MACHINE_POWERPCFP = 16_01f1;
  IMAGE_FILE_MACHINE_IA64      = 16_0200;
  IMAGE_FILE_MACHINE_MIPS16    = 16_0266;
  IMAGE_FILE_MACHINE_ALPHA64   = 16_0284;
  IMAGE_FILE_MACHINE_MIPSFPU   = 16_0366;
  IMAGE_FILE_MACHINE_MIPSFPU16 = 16_0466;
  IMAGE_FILE_MACHINE_TRICORE   = 16_0520;
  IMAGE_FILE_MACHINE_CEF       = 16_0CEF;
  IMAGE_FILE_MACHINE_EBC       = 16_0EBC;
  IMAGE_FILE_MACHINE_AMD64     = 16_8664;
  IMAGE_FILE_MACHINE_M32R      = 16_9041;
  IMAGE_FILE_MACHINE_ARM64     = 16_AA64;
  IMAGE_FILE_MACHINE_CEE       = 16_C0EE;

(* File header format. *)

TYPE

  Header = WinNT.IMAGE_ARCHIVE_MEMBER_HEADER;

  FileDesc = REF RECORD
    next     : FileDesc := NIL;
    name     : TEXT     := NIL;   (* full file name *)
    tag      : TEXT     := NIL;   (* short name that fits in a header *)
    size     : INTEGER  := 0;
    time     : Time.T   := 0.0d0;
    contents : FileBuf  := NIL;
    index    : INTEGER  := 0;     (* ordinal index of file in the global list *)
    offset   : INTEGER  := 0;     (* final offset of the file in the lib *)
  END;

  FileBuf = REF ARRAY OF File.Byte;

TYPE
  ExportDesc = REF RECORD
    next   : ExportDesc;
    symbol : TEXT;
    file   : FileDesc;
    export := FALSE; (* Put in .def file or not, or only static lib. *)
  END;

VAR
  trimUnderscore := FALSE;
  Machine := 0;
  lib_wr      : Wr.T       := NIL;
  lib_name    : TEXT       := NIL;
  lib_time    : Time.T     := 0.0d0;
  def_name    : TEXT       := NIL;
  keep_size   : CARDINAL   := 0;
  n_files     : CARDINAL   := 0;
  files       : FileDesc   := NIL;
  n_exports   : CARDINAL   := 0;
  exports     : ExportDesc := NIL;
  export_vec  : REF ARRAY OF ExportDesc := NIL;
  export_map  : REF ARRAY OF INTEGER := NIL;
  export_len  : CARDINAL    := 0;
  export_tbl  : TextRefTbl.T := NIL;
  long_nms    : TextWr.T   := NIL;
  verbose := FALSE;
  cleanSymbols := TRUE;
  ignoreTexts : TextSeq.T := NIL;

PROCEDURE IsFloatingPointConstant(sym: TEXT; len: INTEGER): BOOLEAN =
BEGIN
    RETURN (len = 4 AND Match (sym, 0, "_xmm"))
        OR (len = 5 AND (Match (sym, 0, "_real") OR Match (sym, 0, "__xmm")))
        OR (len = 6 AND (Match (sym, 0, "__real") OR Match (sym, 0, "__mask")));
END IsFloatingPointConstant;

PROCEDURE HandleArchitecture(machine: INTEGER): BOOLEAN =
BEGIN
    IF Machine # 0 AND machine # Machine THEN
        Die ("multiple architectures: ", Fmt.Int(Machine), ", ",
             Fmt.Int(machine));
    END;

    Machine := machine;

    (* x86 is presumed to be the only wierd one *)
    trimUnderscore := machine = WinNT.IMAGE_FILE_MACHINE_I386;

    RETURN machine = IMAGE_FILE_MACHINE_I386
      OR machine = IMAGE_FILE_MACHINE_ARM
      OR machine = IMAGE_FILE_MACHINE_ARM64
      OR machine = IMAGE_FILE_MACHINE_ARMNT
      OR machine = IMAGE_FILE_MACHINE_AMD64

      (* The rest are historical/hypothetical. *)

      OR machine = IMAGE_FILE_MACHINE_R3000
      OR machine = IMAGE_FILE_MACHINE_R4000
      OR machine = IMAGE_FILE_MACHINE_R10000
      OR machine = IMAGE_FILE_MACHINE_WCEMIPSV2
      OR machine = IMAGE_FILE_MACHINE_ALPHA
      OR machine = IMAGE_FILE_MACHINE_SH3
      OR machine = IMAGE_FILE_MACHINE_SH3DSP
      OR machine = IMAGE_FILE_MACHINE_SH3E
      OR machine = IMAGE_FILE_MACHINE_SH4
      OR machine = IMAGE_FILE_MACHINE_SH5
      OR machine = IMAGE_FILE_MACHINE_THUMB
      OR machine = IMAGE_FILE_MACHINE_AM33
      OR machine = IMAGE_FILE_MACHINE_POWERPC
      OR machine = IMAGE_FILE_MACHINE_POWERPCFP
      OR machine = IMAGE_FILE_MACHINE_IA64
      OR machine = IMAGE_FILE_MACHINE_MIPS16
      OR machine = IMAGE_FILE_MACHINE_ALPHA64
      OR machine = IMAGE_FILE_MACHINE_MIPSFPU
      OR machine = IMAGE_FILE_MACHINE_MIPSFPU16
      OR machine = IMAGE_FILE_MACHINE_TRICORE
      OR machine = IMAGE_FILE_MACHINE_CEF
      OR machine = IMAGE_FILE_MACHINE_EBC
      OR machine = IMAGE_FILE_MACHINE_M32R
      OR machine = IMAGE_FILE_MACHINE_CEE;

END HandleArchitecture;

PROCEDURE DoIt () =
  BEGIN
    ParseCommandLine ();
    CheckLibName ();
    ScanFiles ();
    WriteLib ();
    WriteDef ();
  END DoIt;

(*------------------------------------------------------- command line ---*)

PROCEDURE ParseCommandLine () =
  BEGIN
    FOR i := 1 TO Params.Count-1 DO
      ProcessArg (Params.Get (i));
    END;
  END ParseCommandLine;

PROCEDURE ProcessArg (arg: TEXT) =
  VAR ch: CHAR;
  BEGIN
    ch := Text.GetChar (arg, 0);
    IF (ch = '@') THEN
      arg := Text.Sub (arg, 1);
      IF Text.Length (arg) <= 0 THEN
        Die ("missing command file name: @");
      END;
      ReadCommandFile (arg);
    ELSIF (ch = '-') THEN
      IF TextExtras_CIEqual (Text.Sub (arg, 0, 5), "-out:") THEN
        IF (lib_name # NIL) THEN
          Die ("multiple library names specified: \"", lib_name, "\" and \"",
                Text.Sub (arg, 5), "\".");
        END;
        lib_name := Text.Sub (arg, 5);
        IF Text.Length (lib_name) <= 0 THEN
          Die ("missing library name: -out:<lib>");
        END;
      ELSIF TextExtras_CIEqual (Text.Sub (arg, 0, 5), "-ign:") THEN
        WITH ignText = Text.Sub (arg, 5) DO
          IF ignoreTexts = NIL THEN
            ignoreTexts := NEW(TextSeq.T).init();
          END;
          ignoreTexts.addhi(ignText);
        END;
      ELSIF TextExtras_CIEqual (arg, "-v") THEN
        verbose := TRUE;
      ELSIF TextExtras_CIEqual (arg, "-h") OR TextExtras_CIEqual (arg, "-help") THEN
        Usage();
        Process.Exit(0);
      ELSIF TextExtras_CIEqual (arg, "-noclean") THEN
        cleanSymbols := FALSE;
      ELSE
        Die ("unrecognized option: \"", arg, "\"");
      END;
    ELSE
      (* add a file to the list! *)
      files := NEW (FileDesc, next := files, name := arg);
      INC (n_files);
    END;
  END ProcessArg;

PROCEDURE Usage() =
  BEGIN
    M("usage: mklib [-v] [-noclean] [-ign:<text>]* -out:<libname>", 
      " <files...>");
    M("  or");
    M("       mklib @<cmdfile>");
    M("");
    M("  produces a static library containing the specified object files.");
    M("  ");
    M("options:");
    M("  ");
    M("  -v         run verbosely (produce lots of trace output)");
    M("  -noclean   do not `clean' symbols which contain @ characters");
    M("  -ign:TEXT  ignore (don't export) symbols starting with TEXT");
    M("             This option may occur multiple times.");
    M("  -out:LIBFN Create library in file LIBFN.");
    M("");
  END Usage;

PROCEDURE ReadCommandFile (nm: TEXT) =
  (* Process each non-blank line in file "nm" as if it were
     a command line argument *)
  VAR rd := OpenRd (nm);  arg: TEXT;
  BEGIN
    TRY
      WHILE NOT Rd.EOF (rd) DO
        arg := Trim (Rd.GetLine (rd));
        IF Text.Length (arg) > 0 THEN ProcessArg (arg); END;
      END;
    EXCEPT
    | Rd.Failure, Rd.EndOfFile =>
        Die ("I/O failure while reading \"", nm, "\".");
    | Thread.Alerted =>
        Die ("interrupted while reading \"", nm, "\".");
    END;
  END ReadCommandFile;

PROCEDURE Trim (txt: TEXT): TEXT =
  (* Remove leading and trailing blanks from "txt" and return the result. *)
  VAR
    start := 0;
    len := Text.Length (txt);
  BEGIN
    WHILE (len > 0) AND (Text.GetChar (txt, start) = ' ') DO
      INC (start);
      DEC (len);
    END;
    WHILE (len > 0) AND (Text.GetChar (txt, start + len - 1) = ' ') DO
      DEC (len);
    END;
    RETURN Text.Sub (txt, start, len);
  END Trim;

(*---------------------------------------------------- output LIB name ---*)

PROCEDURE CheckLibName () =
  VAR ext: TEXT;
  BEGIN
    IF (lib_name = NIL) THEN
      Die ("usage: mklib -out:<libname>  <files...>");
    END;
    ext := Pathname.Last (lib_name);
    IF (ext = NIL) OR Text.Length (ext) = 0 THEN
      Die ("didn't specify an output file: \"", lib_name, "\".");
    END;
    ext := Pathname.LastExt (ext);
    IF (ext = NIL) OR Text.Length (ext) = 0 THEN
      (* add on a ".LIB" extension *)
      def_name := Pathname.Join (NIL, lib_name, "def");
      lib_name := Pathname.Join (NIL, lib_name, "lib");
    ELSIF TextExtras_CIEqual (ext, "lib") OR TextExtras_CIEqual (ext, "a") THEN
      def_name := Pathname.ReplaceExt (lib_name, "def");
    ELSIF TextExtras_CIEqual (ext, "def") THEN
      def_name := lib_name;
      lib_name := Pathname.ReplaceExt (lib_name, "lib");
    ELSE
      Die ("unrecognized output file extension: \".", ext, "\"");
    END;
  END CheckLibName;

(*------------------------------------------------------ file scanning ---*)

PROCEDURE ScanFiles () =
  VAR f := files;
  BEGIN
    WHILE (f # NIL) DO
      ScanFile (f);
      f := f.next;
    END;
  END ScanFiles;

PROCEDURE ScanFile (f: FileDesc) =
  VAR file: File.T;  stat: File.Status;  len: INTEGER;  hdr: Header;
  BEGIN
    TRY
      file   := FS.OpenFileReadonly (f.name);
      stat   := file.status ();
      f.time := stat.modificationTime;
      f.size := VAL(stat.size, INTEGER);
      IF (stat.type # RegularFile.FileType) THEN
        Die ("\"", f.name, "\" is not a regular file.");
      END;
      f.contents := NEW (FileBuf, f.size);
      len := file.read (f.contents^);
      IF (len # NUMBER (f.contents^)) THEN
        Die ("unexpected EOF while reading \"", f.name, "\".");
      END;
      file.close ();
    EXCEPT
    | OSError.E => Die ("unable to read file \"", f.name, "\".");
    END;

    (* make sure the name fits... *)
    IF Text.Length (f.name) + 1 <= NUMBER (hdr.Name) THEN
      f.tag := f.name & "/";
    ELSE
      IF (long_nms = NIL) THEN long_nms := TextWr.New (); END;
      VAR offs := Wr.Index (long_nms); <*FATAL Wr.Failure, Thread.Alerted*> BEGIN
        Wr.PutText (long_nms, f.name);
        Wr.PutChar (long_nms, '\000');
        f.tag := "/" & Fmt.Int (offs);
      END;
    END;

    ScanExports (f);

    (* should we keep the in-memory copy of the file? *)
    IF (f.size > MaxKeeper) OR (keep_size > MaxTotalKeep) THEN
      f.contents := NIL;
    ELSE
      INC (keep_size, f.size);
    END;
  END ScanFile;

(*----------------------------------------------- Windows Object Files ---*)

CONST (* we don't handle this stuff! *)
  BadObjFlags = WinNT.IMAGE_FILE_RELOCS_STRIPPED
              + WinNT.IMAGE_FILE_EXECUTABLE_IMAGE
              + WinNT.IMAGE_FILE_16BIT_MACHINE
              + WinNT.IMAGE_FILE_BYTES_REVERSED_LO
              + WinNT.IMAGE_FILE_DLL
              + WinNT.IMAGE_FILE_BYTES_REVERSED_HI;

TYPE
  ObjFile = RECORD
    file      : FileDesc;
    base      : ADDRESS;
    limit     : ADDRESS;
    hdr       : WinNT.PIMAGE_FILE_HEADER;
    symtab    : WinNT.PIMAGE_SYMBOL;
    stringtab : ADDRESS;
  END;

PROCEDURE ScanExports (f: FileDesc) =
  VAR o: ObjFile;  sym: WinNT.PIMAGE_SYMBOL;
  BEGIN
    o.file  := f;
    o.base  := ADR (f.contents[0]);           (* pin the contents so the collector*)
    o.limit := o.base + ADRSIZE (f.contents^);(* doesn't start moving them around *)
    o.hdr   := o.base;

    IF HandleArchitecture(o.hdr.Machine) = FALSE THEN
      Die ("not an object file \"", f.name, Wr.EOL);
      RETURN;
    END;

    IF Word.And (o.hdr.Characteristics, BadObjFlags) # 0 THEN
      (* this object contains stuff we don't understand *)
      Warn ("object file \"", f.name, "\" contains unhandled features.",
            Wr.EOL, "  Its exported symbols will be ignored.");
      RETURN;
    END;

    (* locate the symbol table *)
    o.symtab := o.base + o.hdr.PointerToSymbolTable;
    IF (o.symtab < o.base) OR (o.limit <= o.symtab) THEN
      Die ("cannot find symbol table in object file \"", f.name, "\".");
    END;

    (* locate the string table *)
    o.stringtab := o.symtab + o.hdr.NumberOfSymbols * WinNT.IMAGE_SIZEOF_SYMBOL;
    IF (o.symtab < o.base) OR (o.limit <= o.symtab) THEN
      Die ("cannot find string table in object file \"", f.name, "\".");
    END;

    sym := o.symtab;
    WHILE (sym < o.stringtab) DO
    (* Only export functions, that are defined and external.
     * Modula-3 codegen does not access imported data correctly.
     * e.g
     * link /dump /symbols C:\s\cm3\m3-libs\m3core\AMD64_NT\m3core.lib.sa | findstr _M3
     * 007 00000000 UNDEF  notype ()    External     | RT0_M3
     *              ^^^^^ no not defined
     * 012 00000000 SECT3  notype ()    External     | RTHooks_M3
     *                            ^^ yes function
     * 03D 00000000 SECTF  notype       Static       | $unwind$RTHooks_M3
     *                                  ^^^^^^ no not external
     * 040 00000000 SECT10 notype       Static       | $pdata$RTHooks_M3
     * 006 00000000 UNDEF  notype ()    External     | RTHooks_M3
     * 007 00000000 UNDEF  notype ()    External     | RTAllocator_M3
     *
     * We are building a .def file and a static lib.
     * Function-only is for .def file.
     *)
      IF sym.StorageClass = WinNT.IMAGE_SYM_CLASS_EXTERNAL AND sym.SectionNumber # WinNT.IMAGE_SYM_UNDEFINED THEN
        AddExport (GetSymbolName (o, sym), f, WinNT.ISFCN (sym.Type));
      END;
      sym := sym + WinNT.IMAGE_SIZEOF_SYMBOL * (1 + sym.NumberOfAuxSymbols);
    END;
  END ScanExports;

PROCEDURE AddExport (sym: TEXT;  f: FileDesc; export: BOOLEAN) =
  VAR ref: REFANY; f2: FileDesc;
  BEGIN
    IF NOT IsKeeper(CleanName(sym)) THEN
      (* IO.Put("AddExport skipping:" & sym & "\n"); *)
      RETURN;
    END;
    IF (export_tbl = NIL) THEN
      export_tbl := NEW (TextRefTbl.Default).init ();
    END;
    IF export_tbl.get (sym, ref) THEN
      f2 := ref;
      Warn ("symbol \"", sym, "\" is exported twice:" & Wr.EOL,
              "  " & f2.name & "  (using this instance)" & Wr.EOL,
              "  " & f.name  & "  (ignoring this instance)");
    ELSE
      IF ignoreTexts # NIL THEN
        FOR i := 0 TO ignoreTexts.size() - 1 DO
          WITH t = ignoreTexts.get(i) DO 
            (* ignore the symbol if it starts with one of the ignore texts *)
            IF Text.Equal(t, Text.Sub(sym, 0, Text.Length(t))) THEN
              RETURN;
            END;
          END;
        END;
      END;
      (* a new symbol *)
      EVAL export_tbl.put (sym, f);
      exports := NEW (ExportDesc, next := exports, symbol := sym, file := f, export := export);
      INC (n_exports);
      INC (export_len, Text.Length (sym) + 1);
    END;
  END AddExport;

PROCEDURE GetSymbolName (READONLY o: ObjFile;  sym: WinNT.PIMAGE_SYMBOL): TEXT =
  TYPE IntBytes = ARRAY [0..3] OF UINT8;
  VAR
    max_len, len: INTEGER;
    offset: UINT32;
    ptr: UNTRACED REF CHAR;
    buf: ARRAY [0..255] OF CHAR;
    res: TEXT;
  BEGIN
    IF (sym.N[0] = 0) AND (sym.N[1] = 0) AND (sym.N[2] = 0) AND (sym.N[3] = 0) THEN
      (* the name is long and stored in the string table *)
      WITH xx = LOOPHOLE (offset, IntBytes) DO
        xx[0] := sym.N[4];  xx[1] := sym.N[5];
        xx[2] := sym.N[6];  xx[3] := sym.N[7];
      END;
      ptr := o.stringtab + offset;
      IF (ptr < o.stringtab) OR (o.limit <= ptr) THEN
        Die ("symbol name is outside string table in \"", o.file.name, "\".");
      END;
      max_len := o.limit - ptr;
    ELSE
      (* the name is short and stored in sym.N *)
      ptr := ADR (sym.N[0]);
      max_len := NUMBER (sym.N);
    END;

    (* extract the string *)
    len := 0;
    FOR i := 0 TO MIN (max_len, NUMBER (buf)) - 1 DO
      IF ptr^ = '\000' THEN EXIT; END;
      buf[i] := ptr^;
      INC (ptr, ADRSIZE (ptr^));
      INC (len);
    END;

    res := Text.FromChars (SUBARRAY (buf, 0, len));
    V(res);
    RETURN res;
  END GetSymbolName;

(*--------------------------------------------------------- LIB writer ---*)

PROCEDURE WriteLib () =
  BEGIN
    AssignFileOffsets ();
    SortExports ();

    (* open the output file *)
    TRY
      lib_wr := FileWr.Open (lib_name);
    EXCEPT OSError.E =>
      Die ("unable to open \"", lib_name, "\" for writing.");
    END;
    lib_time := Time.Now ();

    (* write the archive... *)
    TRY
      Wr.PutText (lib_wr, ArchiveMagic);
      DumpExports1 ();
      DumpExports2 ();
      DumpLongNames ();
      DumpFiles ();
    EXCEPT
    | Wr.Failure =>
        Die ("I/O failure while writing \"", lib_name, "\".");
    | Thread.Alerted =>
        Die ("interrupted while writing \"", lib_name, "\".");
    END;

    TRY
      Wr.Close (lib_wr);
    EXCEPT Wr.Failure, Thread.Alerted =>
      Die ("mklib.WriteLib unable to close \"", lib_name, "\".");
    END;
  END WriteLib;

PROCEDURE AssignFileOffsets () =
  CONST Hdr = BYTESIZE (Header);
  VAR f := files;  offs := 0;  index := 1;
  BEGIN
    INC (offs, Text.Length (ArchiveMagic));

    IF (n_exports > 0) THEN
      (* export table 1 *)
      INC (offs, Hdr + 4 * (n_exports + 1) + export_len);
      IF (offs MOD 2 # 0) THEN INC (offs); END;
    END;

    IF (n_exports > 0) THEN
      (* export table 2 *)
      INC (offs, Hdr + 4 * (n_files + 2) + 2 * n_exports + export_len);
      IF (offs MOD 2 # 0) THEN INC (offs); END;
    END;

    IF (long_nms # NIL) THEN
      (* long filename table *)
      INC (offs, Hdr + Wr.Index (long_nms));
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

PROCEDURE SortExports () =
  VAR e := exports;
  BEGIN
    IF (e = NIL) THEN RETURN; END;
    export_vec := NEW (REF ARRAY OF ExportDesc, n_exports);
    export_map := NEW (REF ARRAY OF INTEGER, n_exports);
    FOR i := 0 TO n_exports-1 DO
      export_vec[i] := e;  e := e.next;
      export_map[i] := i;
    END;
    IntArraySort.Sort (export_map^, CmpExport);
  END SortExports;

PROCEDURE CmpExport (a, b: INTEGER): [-1..+1] =
  BEGIN
    RETURN Text.Compare (export_vec [a].symbol, export_vec[b].symbol);
  END CmpExport;

PROCEDURE DumpExports1 ()
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR len, odd: INTEGER;  e: ExportDesc;
  BEGIN
    IF (n_exports <= 0) THEN RETURN; END;
    len := 4 * (n_exports + 1) + export_len;
    odd := export_len MOD 2;
    WriteHeader ("/", "0", lib_time, len);
    WriteBE4 (n_exports);
    FOR i := 0 TO n_exports-1 DO
      e := export_vec [export_map [i]];
      WriteBE4 (e.file.offset);
    END;
    FOR i := 0 TO n_exports-1 DO
      e := export_vec [export_map [i]];
      Wr.PutText (lib_wr, e.symbol);
      Wr.PutChar (lib_wr, '\000');
    END;
    IF (odd # 0) THEN Wr.PutChar (lib_wr, PadChar); END;
  END DumpExports1;

PROCEDURE WriteBE4 (n: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  (* write the little-endian 4-byte value 'n' *)
  BEGIN
    Wr.PutChar (lib_wr, VAL (Word.And (Word.RightShift (n, 24), 16_ff), CHAR));
    Wr.PutChar (lib_wr, VAL (Word.And (Word.RightShift (n, 16), 16_ff), CHAR));
    Wr.PutChar (lib_wr, VAL (Word.And (Word.RightShift (n, 8), 16_ff), CHAR));
    Wr.PutChar (lib_wr, VAL (Word.And (n, 16_ff), CHAR));
  END WriteBE4;

PROCEDURE DumpExports2 ()
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR len, odd: INTEGER;  e: ExportDesc;  f := files;
  BEGIN
    IF (n_exports <= 0) THEN RETURN; END;
    len := 4 * (n_files + 2) + 2 * (n_exports) + export_len;
    odd := export_len MOD 2;
    WriteHeader ("/", "0", lib_time, len);
    WriteLE4 (n_files);
    FOR i := 0 TO n_files-1 DO
      WriteLE4 (f.offset);
      f := f.next;
    END;
    WriteLE4 (n_exports);
    FOR i := 0 TO n_exports-1 DO
      e := export_vec [export_map [i]];
      WriteLE2 (e.file.index);
    END;
    FOR i := 0 TO n_exports-1 DO
      e := export_vec [export_map [i]];
      Wr.PutText (lib_wr, e.symbol);
      Wr.PutChar (lib_wr, '\000');
    END;
    IF (odd # 0) THEN Wr.PutChar (lib_wr, PadChar); END;
  END DumpExports2;

PROCEDURE WriteLE4 (n: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  (* write the little-endian 4-byte value 'n' *)
  BEGIN
    Wr.PutChar (lib_wr, VAL (Word.And (n, 16_ff), CHAR));
    Wr.PutChar (lib_wr, VAL (Word.And (Word.RightShift (n, 8), 16_ff), CHAR));
    Wr.PutChar (lib_wr, VAL (Word.And (Word.RightShift (n, 16), 16_ff), CHAR));
    Wr.PutChar (lib_wr, VAL (Word.And (Word.RightShift (n, 24), 16_ff), CHAR));
  END WriteLE4;

PROCEDURE WriteLE2 (n: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  (* write the little-endian 2-byte value 'n' *)
  BEGIN
    Wr.PutChar (lib_wr, VAL (Word.And (n, 16_ff), CHAR));
    Wr.PutChar (lib_wr, VAL (Word.And (Word.RightShift (n, 8), 16_ff), CHAR));
  END WriteLE2;

PROCEDURE DumpLongNames ()
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR len, odd: INTEGER;
  BEGIN
    IF (long_nms = NIL) THEN RETURN; END;
    len := Wr.Index (long_nms);
    odd := len MOD 2;
    WriteHeader ("//", "0", lib_time, len);
    Wr.PutText (lib_wr, TextWr.ToText (long_nms));
    IF (odd # 0) THEN Wr.PutChar (lib_wr, PadChar); END;
  END DumpLongNames;

PROCEDURE DumpFiles ()
  RAISES {Wr.Failure, Thread.Alerted} =
  (* dump the "real" files *)
  VAR f := files;
  BEGIN
    WHILE (f # NIL) DO
      DumpFile (f);
      f := f.next;
    END;
  END DumpFiles;

PROCEDURE DumpFile (f: FileDesc)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR ptr: UNTRACED REF ARRAY [0..MaxKeeper+1] OF CHAR;
  BEGIN
    <*ASSERT BYTESIZE (File.Byte) = BYTESIZE (CHAR) *>
    WriteHeader (f.tag, "100666", f.time, f.size);
    IF (f.contents # NIL) THEN
      ptr := ADR (f.contents [0]);
      Wr.PutString (lib_wr, SUBARRAY (ptr^, 0, f.size));
    ELSE
      CopyFile (f);
    END;
    IF (f.size MOD 2 # 0) THEN
      Wr.PutChar (lib_wr, PadChar);
    END;
  END DumpFile;

PROCEDURE CopyFile (f: FileDesc)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    rd  := OpenRd (f.name);
    sz  : CARDINAL := 0;
    len : CARDINAL;
    buf : ARRAY [0..1024 * 8 - 1] OF CHAR;
  BEGIN
    TRY
      LOOP
        len := Rd.GetSub (rd, buf);  INC (sz, len);
        IF (len <= 0) THEN EXIT; END;
        Wr.PutString (lib_wr, SUBARRAY (buf, 0, len));
      END;
    EXCEPT Rd.Failure =>
      Die ("I/O failure while reading \"", f.name, "\".");
    END;
    IF (sz # f.size) THEN
      Die ("file \"", f.name, "\" changed size while building!");
    END;
  END CopyFile;

PROCEDURE WriteHeader (nm: TEXT;  mode: TEXT;  time: Time.T;  size: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  TYPE HdrChars = ARRAY [0..BYTESIZE(Header)-1] OF CHAR;
  VAR hdr: Header;
  BEGIN
    StuffT (hdr.Name,      nm);
    StuffI (hdr.Date,      ROUND (time - CoffTime.EpochAdjust));
    StuffT (hdr.UserID,    "");
    StuffT (hdr.GroupID,   "");
    StuffT (hdr.Mode,      mode);
    StuffI (hdr.Size,      size);
    StuffT (hdr.EndHeader, EndHeader);

    Wr.PutString (lib_wr, LOOPHOLE (hdr, HdrChars));
  END WriteHeader;

PROCEDURE StuffI (VAR b: ARRAY OF UINT8;  n: INTEGER) =
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

PROCEDURE StuffT (VAR b: ARRAY OF UINT8;  txt: TEXT) =
  VAR len := Text.Length (txt);
  BEGIN
    FOR i := 0 TO MIN (len - 1, LAST (b)) DO
      b[i] := ORD (Text.GetChar (txt, i));
    END;
    FOR i := len TO LAST (b) DO
      b[i] := ORD (' ');
    END;
  END StuffT;

(*--------------------------------------------------------- DEF writer ---*)

PROCEDURE WriteDef () =
  VAR def_wr: Wr.T;  e: ExportDesc;  sym: TEXT;
  BEGIN
    (* open the output file *)
    TRY
      def_wr := FileWr.Open (def_name);
    EXCEPT OSError.E =>
      Die ("unable to open \"", def_name, "\" for writing.");
    END;

    (* write the file... *)
    TRY
      Wr.PutText (def_wr, "LIBRARY ");
      Wr.PutText (def_wr, Pathname.LastBase (def_name));
      Wr.PutText (def_wr, Wr.EOL);
      Wr.PutText (def_wr, "EXPORTS");
      Wr.PutText (def_wr, Wr.EOL);
      FOR i := 0 TO n_exports-1 DO
        e := export_vec [export_map [i]];
        sym := CleanName (e.symbol);
        <* ASSERT IsKeeper (sym) *> (* should already be checked *)
        IF IsKeeper (sym) AND e.export THEN
          Wr.PutText (def_wr, "  ");
          Wr.PutText (def_wr, sym);
          Wr.PutText (def_wr, Wr.EOL);
        END;
      END;
    EXCEPT
    | Wr.Failure =>
        Die ("I/O failure while writing \"", def_name, "\".");
    | Thread.Alerted =>
        Die ("interrupted while writing \"", def_name, "\".");
    END;

    TRY
      Wr.Close (def_wr);
    EXCEPT Wr.Failure, Thread.Alerted =>
      Die ("mklib.WriteDef unable to close \"", def_name, "\".");
    END;
  END WriteDef;

PROCEDURE CleanName (sym: TEXT): TEXT =
  VAR
    start  := ORD(trimUnderscore AND Text.GetChar (sym, 0) = '_');
    stop   := Text.Length (sym) + 1;
    at     := Text.FindChar (sym, '@');
  BEGIN
    IF (at > 0) AND cleanSymbols THEN stop := at; END;
    RETURN Text.Sub (sym, start, stop - start);
  END CleanName;

PROCEDURE IsKeeper (sym: TEXT): BOOLEAN =
  (* Sort these by length for later optimization.
     Every symbol must be duplicated -- with and without
     a leading underscore. *)
  CONST syms = ARRAY OF TEXT {
    "printf",
    "_printf",
    "_wassert",
    "__wassert",
    "_ftol2_sse",
    "_vfprintf_l",
    "__ftol2_sse",
    "__vfprintf_l"
    };
  VAR len := Text.Length (sym);
  BEGIN
    IF (len > 7)
      AND Match (sym, 0, "_INITM_") THEN
      (* module main body *)
      RETURN FALSE;
    ELSIF (len > 9)
      AND Match (sym, 0, "MM_")
      AND Match (sym, len-6, "_CRASH") THEN
      (* module crash routine *)
      RETURN FALSE;
    END;
    IF (len > 17)
      AND (Match (sym, 0, "M_") OR Match (sym, 0, "I_"))
      AND (Match (sym, len-5, "_INIT") OR Match (sym, len-5, "_LINK"))
      AND Match (sym, len-15, "_t") THEN
      (* a type initialization or setup routine *)
      RETURN FALSE;
    END;

    (* Skip C++ symbols, from C runtime, and floating point constants. *)

    IF len > 1 AND Match(sym, 0, "?") THEN
      RETURN FALSE;
    END;

    IF IsFloatingPointConstant (sym, len) THEN
      RETURN FALSE;
     END;

    IF len > Text.Length (syms[LAST(syms)]) THEN
      RETURN TRUE;
    END;

    FOR i := FIRST(syms) TO LAST(syms) DO
      IF Text.Equal (sym, syms[i]) THEN
        RETURN FALSE;
      END;
    END;

    RETURN TRUE;

  END IsKeeper;

PROCEDURE Match (txt: TEXT;  start: INTEGER;  key: TEXT): BOOLEAN =
  BEGIN
    FOR i := 0 TO Text.Length (key) - 1 DO
      IF Text.GetChar (txt, start + i) # Text.GetChar (key, i) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END Match;

(*--------------------------------------------------------------- misc ---*)

PROCEDURE OpenRd (nm: TEXT): Rd.T =
  VAR rd: Rd.T;
  BEGIN
    TRY
      rd := FileRd.Open (nm);
    EXCEPT OSError.E =>
      Die ("unable to open file \"", nm, "\" for reading.");
    END;
    RETURN rd;
  END OpenRd;

PROCEDURE Warn (a, b, c, d, e: TEXT := NIL) =
  BEGIN
    IO.Put ("warning: ");
    M(a, b, c, d, e);
  END Warn;

PROCEDURE V (a, b, c, d, e: TEXT := NIL) =
  BEGIN
    IF verbose THEN
      M (a, b, c, d, e);
    END;
  END V;

PROCEDURE M (a, b, c, d, e: TEXT := NIL) =
  BEGIN
    IF (a # NIL) THEN IO.Put (a); END;
    IF (b # NIL) THEN IO.Put (b); END;
    IF (c # NIL) THEN IO.Put (c); END;
    IF (d # NIL) THEN IO.Put (d); END;
    IF (e # NIL) THEN IO.Put (e); END;
    IO.Put (Wr.EOL);
  END M;

PROCEDURE Die (a, b, c, d, e: TEXT := NIL) =
  BEGIN
    IF (a # NIL) THEN IO.Put (a); END;
    IF (b # NIL) THEN IO.Put (b); END;
    IF (c # NIL) THEN IO.Put (c); END;
    IF (d # NIL) THEN IO.Put (d); END;
    IF (e # NIL) THEN IO.Put (e); END;
    IO.Put (Wr.EOL);
    IF (lib_wr # NIL) THEN
      (* try to clean up by blowing away the bad output *)
      TRY Wr.Close (lib_wr); EXCEPT Wr.Failure, Thread.Alerted => END;
      TRY FS.DeleteFile (lib_name); EXCEPT OSError.E => END;
      lib_wr := NIL;
    END;
    Process.Exit (1);
  END Die;

(*--------------------------------------------------------------------------*)

PROCEDURE TextExtras_CIEqual(t, u: Text.T): BOOLEAN RAISES {} =
  VAR
    lt: CARDINAL := Text.Length(t);
    lu: CARDINAL := Text.Length(u);
    i: CARDINAL := 0;
  BEGIN
    IF lt = lu THEN
      IF Text.Equal(t, u) THEN
        RETURN TRUE;
      END;
      WHILE i<lt DO
        IF ASCII.Upper[Text.GetChar (t, i)] # ASCII.Upper[Text.GetChar (u, i)] THEN
          RETURN FALSE
        ELSE INC(i)
        END;
      END;
      RETURN TRUE;
    ELSE RETURN FALSE
    END;
  END TextExtras_CIEqual;

(*--------------------------------------------------------------------------*)

BEGIN
  DoIt ();
END Main.
