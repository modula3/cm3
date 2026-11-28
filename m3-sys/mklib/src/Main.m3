(* Some parts copied from WinNT.i3:                     *)
(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)

UNSAFE MODULE Main;

IMPORT Process, IO, Rd, Wr, FileRd, FileWr, Thread, OSError, TextTextTbl;
IMPORT File, FS, Text, Word, TextSeq;
IMPORT Fmt, RegularFile, Params, Pathname;
IMPORT ASCII, Ctypes, WinNT, TextExtras;

TYPE
  UINT8 = Ctypes.unsigned_char;
  UINT32 = Ctypes.unsigned_int;

(*----------------------------------------------- Windows Object Files ---*)

CONST (* we don't handle this stuff! *)
  BadObjFlags = WinNT.IMAGE_FILE_RELOCS_STRIPPED
              + WinNT.IMAGE_FILE_EXECUTABLE_IMAGE
              + WinNT.IMAGE_FILE_16BIT_MACHINE
              + WinNT.IMAGE_FILE_BYTES_REVERSED_LO
              + WinNT.IMAGE_FILE_DLL
              + WinNT.IMAGE_FILE_BYTES_REVERSED_HI;

  IMAGE_FILE_MACHINE_I386  = 16_014c;
  IMAGE_FILE_MACHINE_ARMNT = 16_01c4; (* ARM Thumb-2 Little-Endian *)
  IMAGE_FILE_MACHINE_AMD64 = 16_8664;
  IMAGE_FILE_MACHINE_ARM64 = 16_AA64;

TYPE
  FileBuf = REF ARRAY OF File.Byte;

PROCEDURE Main () =

VAR
  trimUnderscore := FALSE;
  Machine := 0;
  def_name    : TEXT       := NIL;
  export_tbl  := NEW (TextTextTbl.Default).init ();
  verbose := FALSE;
  cleanSymbols := TRUE;
  ignoreTexts : TextSeq.T := NIL;
(*--------------------------------------------------------- DEF writer ---*)
  def_wr: Wr.T;

PROCEDURE IsFloatingPointConstant(sym: TEXT; len: INTEGER): BOOLEAN =
BEGIN
    RETURN (len >= 4 AND Match (sym, 0, "_xmm"))
        OR (len >= 5 AND (Match (sym, 0, "_real") OR Match (sym, 0, "__xmm")))
        OR (len >= 6 AND (Match (sym, 0, "__real") OR Match (sym, 0, "__mask")));
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
      OR machine = IMAGE_FILE_MACHINE_ARM64
      OR machine = IMAGE_FILE_MACHINE_ARMNT
      OR machine = IMAGE_FILE_MACHINE_AMD64;
END HandleArchitecture;

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
      IF TextExtras.CIEqual (Text.Sub (arg, 0, 5), "-out:") THEN
        IF (def_name # NIL) THEN
          Die ("multiple names specified: \"", def_name, "\" and \"",
                Text.Sub (arg, 5), "\".");
        END;
        def_name := Text.Sub (arg, 5);
        IF Text.Length (def_name) <= 0 THEN
          Die ("missing name: -out:<name>");
        END;
        CheckLibName ();
        OpenDef ();
      ELSIF TextExtras.CIEqual (Text.Sub (arg, 0, 5), "-ign:") THEN
        WITH ignText = Text.Sub (arg, 5) DO
          IF ignoreTexts = NIL THEN
            ignoreTexts := NEW(TextSeq.T).init();
          END;
          ignoreTexts.addhi(ignText);
        END;
      ELSIF TextExtras.CIEqual (arg, "-v") THEN
        verbose := TRUE;
      ELSIF TextExtras.CIEqual (arg, "-h") OR TextExtras.CIEqual (arg, "-help") THEN
        Usage();
        Process.Exit(0);
      ELSIF TextExtras.CIEqual (arg, "-noclean") THEN
        cleanSymbols := FALSE;
      ELSE
        Die ("unrecognized option: \"", arg, "\"");
      END;
    ELSE
      (* scan file *)
      ScanFile (arg);
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
    M("  -out:LIBFN Create .def file based on the name LIBFN (changing .lib to .def).");
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
    IF (def_name = NIL) THEN
      Die ("usage: mklib -out:<name>  <files...>");
    END;
    ext := Pathname.Last (def_name);
    IF (ext = NIL) OR Text.Length (ext) = 0 THEN
      Die ("did not specify an output file: \"", def_name, "\".");
    END;
    ext := Pathname.LastExt (ext);
    IF (ext = NIL) OR Text.Length (ext) = 0 THEN
      (* add on a ".def" extension *)
      def_name := Pathname.Join (NIL, def_name, "def");
    ELSIF TextExtras.CIEqual (ext, "lib") OR TextExtras.CIEqual (ext, "a") THEN
      def_name := Pathname.ReplaceExt (def_name, "def");
    ELSIF TextExtras.CIEqual (ext, "def") THEN
      def_name := def_name;
    ELSE
      Die ("unrecognized output file extension: \".", ext, "\"");
    END;
  END CheckLibName;

(*------------------------------------------------------ file scanning ---*)

PROCEDURE ScanFile (file_name: TEXT) =
  VAR file: File.T;  stat: File.Status;  len: INTEGER;
      size := 0;
      contents : FileBuf  := NIL;

PROCEDURE ScanExports () =
  VAR sym: WinNT.PIMAGE_SYMBOL := NIL;
    obj_hdr: WinNT.PIMAGE_FILE_HEADER := NIL;
    obj_base      : ADDRESS := NIL;
    obj_limit     : ADDRESS := NIL;
    obj_symtab    : WinNT.PIMAGE_SYMBOL := NIL;
    obj_stringtab : ADDRESS := NIL;

PROCEDURE GetSymbolName (sym: WinNT.PIMAGE_SYMBOL): TEXT =
  TYPE IntBytes = ARRAY [0..3] OF UINT8;
  VAR
    max_len, len: INTEGER;
    offset: UINT32;
    ptr: UNTRACED REF CHAR;
    buf: ARRAY [0..255] OF CHAR;
    res: TEXT;

  BEGIN (* GetSymbolName *)
    IF (sym.N[0] = 0) AND (sym.N[1] = 0) AND (sym.N[2] = 0) AND (sym.N[3] = 0) THEN
      (* the name is long and stored in the string table *)
      WITH xx = LOOPHOLE (offset, IntBytes) DO
        xx[0] := sym.N[4];  xx[1] := sym.N[5];
        xx[2] := sym.N[6];  xx[3] := sym.N[7];
      END;
      ptr := obj_stringtab + offset;
      IF (ptr < obj_stringtab) OR (obj_limit <= ptr) THEN
        Die ("symbol name is outside string table in \"", file_name, "\".");
      END;
      max_len := obj_limit - ptr;
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

  BEGIN (* ScanExports *)
    obj_base  := ADR (contents[0]);              (* pin the contents so the collector*)
    obj_limit := obj_base + ADRSIZE (contents^); (* doesn't start moving them around *)
    obj_hdr   := obj_base;

    IF HandleArchitecture(obj_hdr.Machine) = FALSE THEN
      Die ("not an object file \"", file_name, Wr.EOL);
      RETURN;
    END;

    IF Word.And (obj_hdr.Characteristics, BadObjFlags) # 0 THEN
      (* this object contains stuff we don't understand *)
      Warn ("object file \"", file_name, "\" contains unhandled features.",
            Wr.EOL, "  Its exported symbols will be ignored.");
      RETURN;
    END;

    (* locate the symbol table *)
    obj_symtab := obj_base + obj_hdr.PointerToSymbolTable;
    IF (obj_symtab < obj_base) OR (obj_limit <= obj_symtab) THEN
      Die ("cannot find symbol table in object file \"", file_name, "\".");
    END;

    (* locate the string table *)
    obj_stringtab := obj_symtab + obj_hdr.NumberOfSymbols * WinNT.IMAGE_SIZEOF_SYMBOL;
    IF (obj_symtab < obj_base) OR (obj_limit <= obj_symtab) THEN
      Die ("cannot find string table in object file \"", file_name, "\".");
    END;

    sym := obj_symtab;
    WHILE (sym < obj_stringtab) DO
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
      IF WinNT.ISFCN (sym.Type)
          AND sym.StorageClass = WinNT.IMAGE_SYM_CLASS_EXTERNAL
          AND sym.SectionNumber # WinNT.IMAGE_SYM_UNDEFINED THEN
        AddExport (GetSymbolName (sym));
      END;
      sym := sym + WinNT.IMAGE_SIZEOF_SYMBOL * (1 + sym.NumberOfAuxSymbols);
    END;
  END ScanExports;

PROCEDURE AddExport (sym: TEXT) =
  VAR file2_name: TEXT := NIL;

PROCEDURE WriteDef () =
  BEGIN
    (* write the file *)
    TRY
      Wr.PutText (def_wr, "  ");
      Wr.PutText (def_wr, CleanName (sym));
      Wr.PutText (def_wr, Wr.EOL);
    EXCEPT
      Wr.Failure => Die ("I/O failure while writing \"", def_name, "\".");
    | Thread.Alerted => Die ("interrupted while writing \"", def_name, "\".");
    END;
  END WriteDef;

  BEGIN (* AddExport *)
    IF NOT IsKeeper(CleanName(sym)) THEN
      (* IO.Put("AddExport skipping:" & sym & "\n"); *)
      RETURN;
    END;
    IF export_tbl.get (sym, file2_name) THEN
      Warn ("symbol \"", sym, "\" is exported twice:" & Wr.EOL,
              "  " & file2_name & "  (using this instance)" & Wr.EOL,
              "  " & file_name  & "  (ignoring this instance)");
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
      EVAL export_tbl.put (sym, file_name);
      WriteDef ();
    END;
  END AddExport;

  BEGIN (* ScanFile *)
    TRY
      file   := FS.OpenFileReadonly (file_name);
      stat   := file.status ();
      size   := VAL(stat.size, INTEGER);
      IF (stat.type # RegularFile.FileType) THEN
        Die ("\"", file_name, "\" is not a regular file.");
      END;
      contents := NEW (FileBuf, size);
      len := file.read (contents^);
      IF (len # NUMBER (contents^)) THEN
        Die ("unexpected EOF while reading \"", file_name, "\".");
      END;
      file.close ();
    EXCEPT OSError.E => Die ("unable to read file \"", file_name, "\".");
    END;

    ScanExports ();

    (* drop in-memory copy of the file *)
    contents := NIL;
  END ScanFile;

(*--------------------------------------------------------- DEF writer ---*)

PROCEDURE OpenDef () =
  BEGIN
    (* open the output file *)
    TRY
      def_wr := FileWr.Open (def_name);
    EXCEPT OSError.E =>
      Die ("unable to open \"", def_name, "\" for writing.");
    END;
    (* write the file *)
    TRY
      Wr.PutText (def_wr, "LIBRARY ");
      Wr.PutText (def_wr, Pathname.LastBase (def_name));
      Wr.PutText (def_wr, Wr.EOL);
      Wr.PutText (def_wr, "EXPORTS");
      Wr.PutText (def_wr, Wr.EOL);
    EXCEPT
      Wr.Failure => Die ("I/O failure while writing \"", def_name, "\".");
    | Thread.Alerted => Die ("interrupted while writing \"", def_name, "\".");
    END;
  END OpenDef;

PROCEDURE CloseDef () =
  BEGIN
    TRY
      Wr.Close (def_wr);
    EXCEPT Wr.Failure, Thread.Alerted =>
      Die ("mklib.WriteDef unable to close \"", def_name, "\".");
    END;
  END CloseDef;

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
    "__vfprintf_l",
    "_Avx2WmemEnabledWeakValue",
    "__Avx2WmemEnabledWeakValue"
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
    IF (def_name # NIL) THEN
      (* try to clean up bad output *)
      TRY FS.DeleteFile (def_name); EXCEPT OSError.E => END;
    END;
    Process.Exit (1);
  END Die;

(*--------------------------------------------------------------------------*)

  BEGIN (* Main *)
    ParseCommandLine ();
    CloseDef ();
  END Main;

(*--------------------------------------------------------------------------*)

BEGIN
  Main ();
END Main.
