(* Copyright 1995-96 Critical Mass, Inc. All rights reserved.    *)

(* This code is derived from a version of Matt Pietrek's PEDUMP program.
   It's ugly Modula-3 because it's a straight translation from the C code. *)

UNSAFE MODULE DLL;

IMPORT Fmt, M3toC, Stdio, WinDef, WinBase, WinNT, Word, Wr;

TYPE
  State = RECORD
    file             : TEXT;
    hFile            : WinDef.HANDLE                 := NIL;
    hFileMapping     : WinDef.HANDLE                 := NIL;
    lpFileBase       : ADDRESS                       := NIL;
    dosHeader        : WinNT.PIMAGE_DOS_HEADER       := NIL;
    pNTHeader        : WinNT.PIMAGE_NT_HEADERS       := NIL;
    pImageFileHeader : WinNT.PIMAGE_FILE_HEADER      := NIL;
    imageBase        : WinDef.DWORD                  := 0;
    header           : WinNT.PIMAGE_SECTION_HEADER   := NIL;
    exportDir        : WinNT.PIMAGE_EXPORT_DIRECTORY := NIL;
    delta            : INTEGER                       := 0;
    exports          : ExportList                    := NIL;
    n_exports        : INTEGER                       := 0;
  END;

PROCEDURE GetExports (file: TEXT): ExportList  RAISES {Error} =
  VAR s: State;
  BEGIN
    s.file := file;
    s.exports := NEW (ExportList, 32);
    TRY
      s.hFile := WinBase.CreateFile (M3toC.TtoS (file), WinNT.GENERIC_READ,
                   WinNT.FILE_SHARE_READ, NIL, WinBase.OPEN_EXISTING,
                   WinNT.FILE_ATTRIBUTE_NORMAL, NIL);
      IF (LOOPHOLE (s.hFile, INTEGER) = WinBase.INVALID_HANDLE_VALUE) THEN
        Err ("cannot open file file");
      END;

      s.hFileMapping := WinBase.CreateFileMapping (s.hFile, NIL,
                                          WinNT.PAGE_READONLY, 0, 0, NIL);
      IF (s.hFileMapping = NIL) THEN
        Err ("cannot open file mapping");
      END;

      s.lpFileBase := WinBase.MapViewOfFile (s.hFileMapping, WinBase.FILE_MAP_READ,
                                           0, 0, 0);
      IF (s.lpFileBase = NIL) THEN
        Err ("cannot map view of file");
      END;

      s.dosHeader := LOOPHOLE (s.lpFileBase, WinNT.PIMAGE_DOS_HEADER);
      IF (s.dosHeader.e_magic # WinNT.IMAGE_DOS_SIGNATURE) THEN
        Err ("file is not a DLL");
      END;

      FindDLLExports (s);

    FINALLY
      IF (s.lpFileBase # NIL) THEN
        EVAL WinBase.UnmapViewOfFile (s.lpFileBase);
      END;
      IF (s.hFileMapping # NIL) THEN
        EVAL WinBase.CloseHandle (s.hFileMapping);
      END;
      IF (s.hFile # NIL) THEN
        EVAL WinBase.CloseHandle (s.hFile);
      END;
    END;

    RETURN s.exports;
  END GetExports;

PROCEDURE FindDLLExports (VAR s: State) RAISES {Error} =
  VAR
    addr     : WinDef.DWORD;
    functions: WinDef.PDWORD;
    ordinals : WinDef.PWORD;
    names    : UNTRACED REF WinNT.PSTR;
  BEGIN
    s.pNTHeader := s.dosHeader + s.dosHeader.e_lfanew;
    IF WinBase.IsBadReadPtr (s.pNTHeader, BYTESIZE (WinNT.IMAGE_NT_HEADERS)) # 0
      OR (s.pNTHeader.Signature # WinNT.IMAGE_NT_SIGNATURE) THEN
      Err ("unrecognized EXE file type");
    END;

    s.pImageFileHeader := ADR (s.pNTHeader.FileHeader);
    IF Word.And (s.pImageFileHeader.Characteristics, WinNT.IMAGE_FILE_DLL) = 0 THEN
      Err ("file is not a DLL");
    END;

    s.imageBase := s.pNTHeader.OptionalHeader.ImageBase;

    s.header := GetSectionHeader (s, ".edata");

    IF (s.header # NIL) THEN
      s.exportDir := s.lpFileBase + s.header.PointerToRawData;
      s.delta := s.header.VirtualAddress - s.header.PointerToRawData;

    ELSE (* try the top-level image directory *)
      addr := GetDirectoryEntry (s, WinNT.IMAGE_DIRECTORY_ENTRY_EXPORT);
      IF (addr = 0) THEN
        Err ("unable to locate the DLL's export section in the directory");
      END;
      s.header := GetSecHeaderFromAddr (s, addr);
      IF (s.header = NIL) THEN
        Err ("unable to locate the DLL's export section");
      END;
      s.exportDir := s.lpFileBase + s.header.PointerToRawData
                                  + (addr - s.header.VirtualAddress);
      s.delta := s.header.VirtualAddress - s.header.PointerToRawData;
    END;

    (*****
    Out ("DLL name:     ", M3toC.StoT (s.lpFileBase + s.exportDir.Name - s.delta));
    Out ("Ordinal base: ", Fmt.Unsigned (s.exportDir.Base));
    Out ("#functions:   ", Fmt.Unsigned (s.exportDir.NumberOfFunctions));
    Out ("#names:       ", Fmt.Unsigned (s.exportDir.NumberOfNames));
    ****)

    functions := s.lpFileBase - s.delta
                   + LOOPHOLE (s.exportDir.AddressOfFunctions, INTEGER);
    ordinals  := s.lpFileBase - s.delta
                   + LOOPHOLE (s.exportDir.AddressOfNameOrdinals, INTEGER);
    names     := s.lpFileBase - s.delta
                   + LOOPHOLE (s.exportDir.AddressOfNames, INTEGER);

    FOR i := 0 TO s.exportDir.NumberOfNames - 1 DO
      VAR
        offset := 0;
        ord    := ordinals^ + s.exportDir.Base;
        fname  := s.lpFileBase + LOOPHOLE (names^, INTEGER) - s.delta;
      BEGIN
        IF (0 <= ordinals^) AND (ordinals^ < s.exportDir.NumberOfFunctions) THEN
          offset := LOOPHOLE (functions + ordinals^ * BYTESIZE(WinDef.DWORD),
                              WinDef.PDWORD)^;
          AddExport (s, ord, offset, M3toC.CopyStoT (fname));
        ELSE
          Out ("mysterious export:  ordinal = ", Fmt.Int (ordinals^),
               ",  name = ", M3toC.StoT (fname));
        END;
      END;
      INC (names, ADRSIZE (names^));
      INC (ordinals, ADRSIZE (ordinals^));
    END;
  END FindDLLExports;

PROCEDURE AddExport (VAR s: State;  ord: INTEGER;  offset: INTEGER;  name: TEXT) =
  BEGIN
    (****
    Out (Fmt.Int (ord) & "  ", Fmt.Unsigned (offset), "  ", name);
    *****)
    IF (s.n_exports >= NUMBER (s.exports^)) THEN ExpandExports (s); END;
    WITH z = s.exports[s.n_exports] DO
      z.ord    := ord;
      z.offset := offset;
      z.name   := name;
    END;
    INC (s.n_exports);
  END AddExport;

PROCEDURE ExpandExports (VAR s: State) =
  VAR n := NUMBER (s.exports^);  xx := NEW (ExportList, n+n);
  BEGIN
    SUBARRAY (xx^, 0, n) := s.exports^;
    s.exports := xx;
  END ExpandExports;

PROCEDURE GetSectionHeader (VAR s: State;  nm: TEXT): WinNT.PIMAGE_SECTION_HEADER =
  VAR section: WinNT.PIMAGE_SECTION_HEADER;
  BEGIN
    section := s.pNTHeader + ADRSIZE (s.pNTHeader^);
    FOR i := 0 TO s.pNTHeader.FileHeader.NumberOfSections - 1 DO
      IF strnicmp (ADR (section.Name[0]), M3toC.TtoS (nm),
                   WinNT.IMAGE_SIZEOF_SHORT_NAME) = 0 THEN
        RETURN section;
      END;
      section := section + BYTESIZE (section^);
    END;
    RETURN NIL;
  END GetSectionHeader;

PROCEDURE GetSecHeaderFromAddr (VAR s: State;
                                addr: WinDef.DWORD): WinNT.PIMAGE_SECTION_HEADER =
  VAR section: WinNT.PIMAGE_SECTION_HEADER;  start, stop: INTEGER;
  BEGIN
    section := s.pNTHeader + ADRSIZE (s.pNTHeader^);
    FOR i := 0 TO s.pNTHeader.FileHeader.NumberOfSections - 1 DO
      start := section.VirtualAddress;
      stop  := start + section.SizeOfRawData;
      IF (start <= addr) AND (addr <= stop) THEN RETURN section; END;
      section := section + BYTESIZE (section^);
    END;
    RETURN NIL;
  END GetSecHeaderFromAddr;

PROCEDURE GetDirectoryEntry (VAR s: State;  dir_index: INTEGER): WinDef.DWORD =
  VAR oh: WinNT.PIMAGE_OPTIONAL_HEADER := ADR (s.pNTHeader.OptionalHeader);
  BEGIN
    IF (dir_index >= oh.NumberOfRvaAndSizes)
      THEN RETURN 0;
      ELSE RETURN oh.DataDirectory [dir_index].VirtualAddress;
    END;
  END GetDirectoryEntry;

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

PROCEDURE Err (msg: TEXT) RAISES {Error} =
  BEGIN
    RAISE Error (msg);
  END Err;

PROCEDURE strnicmp (a, b: ADDRESS;  n: INTEGER): INTEGER =
  VAR
    ca: UNTRACED REF CHAR := a;
    cb: UNTRACED REF CHAR := b;
  BEGIN
    FOR i := 0 TO n-1 DO
      IF (ca^ # cb^) THEN
        RETURN ORD (ca^) - ORD (cb^);
      END;
      IF (ca^ = '\000') THEN EXIT END;
      INC (ca, ADRSIZE (ca^));
      INC (cb, ADRSIZE (cb^));
    END;
    RETURN 0;
  END strnicmp;

BEGIN
END DLL.
