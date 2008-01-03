(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Nov 10 15:08:43 PST 1994 by isard      *)

MODULE M3LoaderRd;

IMPORT RegularFile, FS, Fmt, Word, Text, IntRefTbl, M3ID, SortedIntIntTbl;
IMPORT SortedIntRefTbl, OSError, Wr, Stdio;
IMPORT M3Loader, M3LoaderObj, M3LoaderObjRep, M3LoaderAccess, M3LoaderProcess;
IMPORT M3LoaderUnsafe, M3LoaderDebug AS Debug;

FROM M3Loader IMPORT ObjModule, LibModule;
FROM M3LoaderObj IMPORT Symbol, LinkDef, FileType;
FROM M3LoaderObjRep IMPORT Relocation, ProcLabel;
FROM M3LoaderAccess IMPORT Buffer, SegType, to_int, to_char, to_text;
FROM M3LoaderAccess IMPORT from_int, do_reloc, ascii_to_int;

REVEAL
  T = Public BRANDED "M3LoaderRd.T" OBJECT
      process         : M3LoaderProcess.T;
      obj             : ObjModule;
      filebufsize     := 16_10000;
      filebuffer      : Buffer := NIL;
      lib_header_buf  : Buffer := NIL;
      nsections,
      symtable,
      nsymbols,
      sect_start,
      stringtable     : INTEGER;
      section_header  : REF ARRAY OF SectionHeader;
      nimports        : INTEGER;
      imports         : REF ARRAY OF M3ID.T;
      nexports        : INTEGER;
      exports         : REF ARRAY OF Symbol;
      nlinkdefs       : INTEGER;
      linkdefs        : REF ARRAY OF LinkDef;
      nimportrelocs   : INTEGER;
      importrelocs    : REF ARRAY OF Relocation;
    OVERRIDES
      read_object := read_object;
      fill_object := fill_object;
      scan_library := scan_library;
    END;

REVEAL
  M3Loader.LibModule = M3LoaderObj.PublicLib
    BRANDED "M3LoaderObj.LibModule" OBJECT
      longnames       := FALSE;
      objnames        : Buffer;
    END;

TYPE
  SectionHeader = RECORD
    name       : TEXT;
    vsize      : INTEGER;
    offset     : INTEGER;
    rawsize    : INTEGER;
    rawoffset  : INTEGER;
    relocs     : INTEGER;
    nrelocs    : INTEGER;
    lines      : INTEGER;
    nlines     : INTEGER;
    flags      : INTEGER;
    seg        : SegType;
    pos        : INTEGER;
    initpos    : INTEGER;
    used       := FALSE;
  END;

PROCEDURE error (msg: TEXT) =
  <* FATAL ANY *>
  BEGIN
    Wr.PutText(Stdio.stderr, msg & "\n");
  END error;

PROCEDURE read_object (t: T; filename: TEXT): ObjModule
            RAISES {M3Loader.LoadError} =
  VAR
    file    : RegularFile.T;
    obj     : ObjModule;
    success : BOOLEAN;
  BEGIN
    TRY
      file := FS.OpenFileReadonly(filename);
    EXCEPT
      OSError.E =>
        error("Can't open file " & filename);
        RAISE M3Loader.LoadError;
    END;

    obj := NEW(ObjModule, name := M3ID.Add(filename),
                          whole_file := TRUE);

    success := fill_object(t, obj, file);

    TRY
      file.close();
    EXCEPT
      OSError.E =>
        error("Error closing file " & filename);
        RAISE M3Loader.LoadError;
    END;

    IF NOT success THEN RAISE M3Loader.LoadError END;

    RETURN obj;
  END read_object;

PROCEDURE fill_object (t: T; obj: ObjModule; file: RegularFile.T): BOOLEAN =
  VAR
    size     : INTEGER;
  BEGIN
    IF obj.loaded THEN RETURN TRUE END;

    TRY
      IF obj.name = M3ID.NoID THEN
        read_lib_member_header(t, obj);
      END;

      IF obj.whole_file THEN
        size := file.status().size;
      ELSE
        size := obj.length;
        IF file.seek(RegularFile.Origin.Beginning, obj.start) # obj.start
         THEN
          error("fill_object tried to seek past the end of file");
          RAISE M3Loader.LoadError;
        END
      END;

      IF size > t.filebufsize THEN
        INC(t.filebufsize, size);
        t.filebuffer := NEW(Buffer, t.filebufsize);
      END;

      IF file.read(SUBARRAY(t.filebuffer^, 0, size), FALSE) # size
       THEN
        error("fill_object file read failed");
        RAISE M3Loader.LoadError;
      END;

      t.obj := obj;

      interpret_object(t);

      obj.fixup();
    EXCEPT
      OSError.E =>
        error("fill object got file a system error");
        RETURN FALSE;
    | M3Loader.LoadError =>
        RETURN FALSE;
    END;

    RETURN TRUE;
  END fill_object;

CONST
  LIB_HEADER_SIZE      = 60;
  LIB_MEMBER_SIZE_OFFS = 48;

PROCEDURE scan_library (t: T; filename: TEXT): LibModule
            RAISES {M3Loader.LoadError} =
  VAR
    lib     := NEW(LibModule, name := M3ID.Add(filename));
    sympos,
    size    : INTEGER;
    file    : RegularFile.T;
  BEGIN
    TRY
      file := FS.OpenFileReadonly(filename);
    EXCEPT
      OSError.E =>
        filename := Text.Cat("C:\\mstools\\lib\\", filename);
        TRY
          file := FS.OpenFileReadonly(filename);
        EXCEPT
          OSError.E =>
            error("Can't open file " & filename &
                    " (tried current directory and c:\\mstools\\lib)");
            RAISE M3Loader.LoadError;
        END
    END;

    lib.file := file;

    TRY
      TRY
        IF file.read(SUBARRAY(t.filebuffer^, 0, LIB_HEADER_SIZE + 8), FALSE)
             # LIB_HEADER_SIZE + 8 THEN
          RAISE M3Loader.LoadError;
        END;

        IF NOT Text.Equal(to_text(t.filebuffer, 0, 24),
                          "!<arch>\n/               ") THEN
          RAISE M3Loader.LoadError;
        END;

        size := ascii_to_int(t.filebuffer, LIB_MEMBER_SIZE_OFFS + 8, 10);

        size := Word.And(size + 1, 16_FFFFFFFE);

        EVAL file.seek(RegularFile.Origin.Current, size);

        IF file.read(SUBARRAY(t.filebuffer^, 0, LIB_HEADER_SIZE), FALSE)
             # LIB_HEADER_SIZE THEN
          RAISE M3Loader.LoadError;
        END;

        IF NOT Text.Equal(to_text(t.filebuffer, 0, 16),
                          "/               ") THEN
          RAISE M3Loader.LoadError;
        END;

        size := ascii_to_int(t.filebuffer, LIB_MEMBER_SIZE_OFFS, 10);

        size := Word.And(size + 1, 16_FFFFFFFE);

        sympos := file.seek(RegularFile.Origin.Current, 0);

        EVAL file.seek(RegularFile.Origin.Current, size);

        IF file.read(SUBARRAY(t.filebuffer^, 0, LIB_HEADER_SIZE), FALSE)
             # LIB_HEADER_SIZE THEN
          RAISE M3Loader.LoadError;
        END;

        IF Text.Equal(to_text(t.filebuffer, 0, 16),
                          "//              ") THEN
          read_lib_objnames(t, lib, file);
        END;

        IF size > t.filebufsize THEN
          INC(t.filebufsize, size);
          t.filebuffer := NEW(Buffer, t.filebufsize);
        END;

        IF file.seek(RegularFile.Origin.Beginning, sympos) # sympos THEN
          RAISE M3Loader.LoadError;
        END;

        IF file.read(SUBARRAY(t.filebuffer^, 0, size), FALSE) # size THEN
          RAISE M3Loader.LoadError;
        END;
      EXCEPT
        OSError.E, M3Loader.LoadError =>
          error("Can't read library header");
          RAISE M3Loader.LoadError;
      END;

      interpret_lib_symbols(t, lib);

      read_lib_member_header(t, lib.members[0]);
      VAR objname := M3ID.ToText(lib.members[0].name);
      BEGIN
        IF Text.Equal(Text.Sub(objname, Text.Length(objname)-4, 4), ".def")
           THEN
          Debug.Txt("Library contains stubs for DLL ");
          IF NOT fill_object(t, lib.members[0], lib.file) THEN
            RAISE M3Loader.LoadError;
          END;
          IF lib.dll = NIL THEN
          error("Error: unexpected DLL format");
          RAISE M3Loader.LoadError;
          END;

          Debug.Name(lib.dll.name);
          Debug.NL();
        END
      END
    EXCEPT
      M3Loader.LoadError =>
        TRY
          file.close();
        EXCEPT
          OSError.E =>
            error("Error closing file " & filename);
        END;
        RAISE M3Loader.LoadError;
    END;

    RETURN lib;
  END scan_library;

PROCEDURE read_lib_objnames (t: T; lib: LibModule; file: RegularFile.T)
            RAISES {M3Loader.LoadError} =
  VAR
    size: INTEGER;
  BEGIN
    size := ascii_to_int(t.filebuffer, LIB_MEMBER_SIZE_OFFS, 10);

    lib.objnames := NEW(Buffer, size);

    TRY
      IF file.read(lib.objnames^, FALSE) # size THEN
        RAISE M3Loader.LoadError;
      END
    EXCEPT
      OSError.E, M3Loader.LoadError =>
        error("Can't read library header");
        RAISE M3Loader.LoadError;
    END;

    lib.longnames := TRUE;
  END read_lib_objnames;

PROCEDURE interpret_lib_symbols (t: T; lib: LibModule) =
  VAR
    objbase,
    parent,
    symcount,
    nsymbols  : INTEGER;
    nametext  : TEXT;
    name      : M3ID.T;
  BEGIN
    lib.nmembers := to_int(t.filebuffer, 0, 4);

    lib.members := NEW(REF ARRAY OF ObjModule, lib.nmembers);
    FOR i := 0 TO lib.nmembers-1 DO
      lib.members[i] := NEW(ObjModule, library := lib,
                                       type := FileType.Lib,
                                       start := to_int(t.filebuffer,
                                                       4 + i*4, 4),
                                       name := M3ID.NoID,
                                       whole_file := FALSE);
    END;

    nsymbols := to_int(t.filebuffer, 4 + lib.nmembers*4, 4);

    lib.exports := NEW(IntRefTbl.Default);
    lib.exports := lib.exports.init(nsymbols);

    objbase := 8 + lib.nmembers*4;
    symcount := objbase + 2 * nsymbols;

    FOR i := 0 TO nsymbols-1 DO
      nametext := to_text(t.filebuffer, symcount, 16_10000);

      INC(symcount, Text.Length(nametext)+1);
      parent := to_int(t.filebuffer, objbase + i*2, 2) - 1;

      name := M3ID.Add(nametext);
      EVAL lib.exports.put(name, lib.members[parent]);
    END
  END interpret_lib_symbols;    

PROCEDURE read_lib_member_header (t: T; obj: ObjModule)
            RAISES {M3Loader.LoadError} =
  VAR
    slashpt : INTEGER;
    name    : TEXT;
  BEGIN
    TRY
      IF obj.library.file.seek(RegularFile.Origin.Beginning, obj.start) #
         obj.start THEN
        RAISE M3Loader.LoadError;
      END;

      IF obj.library.file.read(t.lib_header_buf^, FALSE) # LIB_HEADER_SIZE THEN
        RAISE M3Loader.LoadError;
      END;
    EXCEPT
      OSError.E, M3Loader.LoadError =>
        error("Can't read library member header");
        RAISE M3Loader.LoadError;
    END;

    IF t.lib_header_buf[0] = ORD('/') THEN
      obj.name := get_long_lib_name(obj.library,
                                    ascii_to_int(t.lib_header_buf,
                                    1, 15));
    ELSE
      name := to_text(t.lib_header_buf, 0, 16);
      slashpt := Text.FindChar(name, '/');
      IF slashpt = -1 THEN
        obj.name := M3ID.Add(name);
      ELSE
        obj.name := M3ID.Add(Text.Sub(name, 0, slashpt));
      END
    END;

    obj.start := obj.start + LIB_HEADER_SIZE;
    obj.length := ascii_to_int(t.lib_header_buf, 48, 10);
  END read_lib_member_header;

PROCEDURE get_long_lib_name(lib: LibModule; pos: INTEGER): M3ID.T
            RAISES {M3Loader.LoadError} =
  BEGIN
    IF NOT lib.longnames THEN
      error("Tried to read a long name from a short named library");
      RAISE M3Loader.LoadError;
    END;
    RETURN M3ID.Add(to_text(lib.objnames, pos, NUMBER(lib.objnames^)));
  END get_long_lib_name;

PROCEDURE interpret_object (t: T) RAISES {M3Loader.LoadError} =
  BEGIN
    read_header(t);

    t.section_header := NEW(REF ARRAY OF SectionHeader, t.nsections);

    FOR i := 0 TO t.nsections - 1 DO
      read_section_header(t, t.sect_start + i * 40, t.section_header[i]);
    END;

    process_sections(t);

    allocate_segments(t);

    process_relocations(t);

    fill_in_exports(t);

    add_line_numbers(t);

    t.section_header := NIL;
  END interpret_object;

PROCEDURE read_header (t: T) RAISES {M3Loader.LoadError} =
  BEGIN
    IF to_int(t.filebuffer, 0, 2) # 16_014C THEN
      error("Can't read object header");
      RAISE M3Loader.LoadError;
    END;

    IF Word.And(to_int(t.filebuffer, 18, 2), 3) # 0 THEN
      error("Can't read object header");
      RAISE M3Loader.LoadError;
    END;

    t.sect_start := to_int(t.filebuffer, 16, 2) + 20;

    t.nsections := to_int(t.filebuffer, 2, 2);
    t.symtable := to_int(t.filebuffer, 8, 4);
    t.nsymbols := to_int(t.filebuffer, 12, 4);
    t.stringtable := t.symtable + t.nsymbols * SymTabSize;
  END read_header;

PROCEDURE read_section_header(t: T; offset: INTEGER;
                              VAR header: SectionHeader) =
  BEGIN
    IF to_char(t.filebuffer, offset) = '/' THEN
      header.name := getstring(t, ascii_to_int(t.filebuffer, offset + 1, 7));
    ELSE
      header.name := to_text(t.filebuffer, offset, 8);
    END;

    header.vsize     := to_int(t.filebuffer, offset + 8, 4);
    header.offset    := to_int(t.filebuffer, offset + 12, 4);
    header.rawsize   := to_int(t.filebuffer, offset + 16, 4);
    header.rawoffset := to_int(t.filebuffer, offset + 20, 4);
    header.relocs    := to_int(t.filebuffer, offset + 24, 4);
    header.nrelocs   := to_int(t.filebuffer, offset + 32, 2);
    header.lines     := to_int(t.filebuffer, offset + 28, 4);
    header.nlines    := to_int(t.filebuffer, offset + 34, 2);
    header.flags     := to_int(t.filebuffer, offset + 36, 4);

    IF header.vsize < header.rawsize THEN
      header.vsize := header.rawsize;
    END
  END read_section_header;

CONST
  IMAGE_SCN_TYPE_NOPAD             = 16_00000008;
  IMAGE_SCN_CNT_CODE               = 16_00000020;
  IMAGE_SCN_CNT_INITIALISED_DATA   = 16_00000040;
  IMAGE_SCN_CNT_UNINITIALISED_DATA = 16_00000080;
  IMAGE_SCN_LNK_INFO               = 16_00000200;
  IMAGE_SCN_LNK_OVERLAY            = 16_00000400;
  IMAGE_SCN_MEM_DISCARDABLE        = 16_02000000;

  SECTION_ALIGN                    = 16_10000;

  IMAGE_SYM_CLASS_EXTERNAL         = 2;
  IMAGE_SYM_CLASS_STATIC           = 3;
  IMAGE_SYM_CLASS_EXTERNAL_DEF     = 5;
  IMAGE_SYM_CLASS_LABEL            = 6;

  SymTabSize = 18;
  RelocSize  = 10;

PROCEDURE process_sections (t: T) RAISES {M3Loader.LoadError} =
  BEGIN
    FOR i := FIRST(SegType) TO LAST(SegType) DO
      t.obj.segsize[i] := 0;
    END;

    FOR i := 0 TO t.nsections-1 DO
      WITH section = t.section_header[i] DO
        IF NOT section_ignorable(t, section) THEN
          IF section_unsupported(section) THEN
            error("Object " & M3ID.ToText(t.obj.name) &
                    " contains unsupported section type");
            RAISE M3Loader.LoadError;
          END;

          find_space_for_section(t, section);
        END
      END
    END

  END process_sections;

PROCEDURE find_space_for_section (t: T; VAR sect: SectionHeader)
            RAISES {M3Loader.LoadError} =
  VAR
    type : SegType;
  BEGIN
    IF Word.And(sect.flags, IMAGE_SCN_CNT_CODE) # 0 THEN
      sect.used := TRUE;
      type := SegType.Text;
    END;

    IF Word.And(sect.flags, IMAGE_SCN_CNT_INITIALISED_DATA) # 0 THEN
      sect.used := TRUE;
      type := SegType.Data;
    END;

    IF Word.And(sect.flags, IMAGE_SCN_CNT_UNINITIALISED_DATA) # 0 THEN
      sect.used := TRUE;
      type := SegType.Bss;
    END;

    IF NOT sect.used THEN
      error("Unknown section type in object " & M3ID.ToText(t.obj.name));
      RAISE M3Loader.LoadError;
    END;

    pad_if(t, sect.flags, type);
    sect.seg := type;
    sect.pos := t.obj.segsize[type];
    INC(t.obj.segsize[type], MAX(sect.rawsize, sect.vsize));
  END find_space_for_section;

PROCEDURE pad_if (t: T; sectflags: INTEGER; seg: SegType) =
  BEGIN
    IF Word.And(sectflags, IMAGE_SCN_TYPE_NOPAD) = 0 THEN
      t.obj.segsize[seg] :=
        Word.And(t.obj.segsize[seg] + SECTION_ALIGN - 1,
                 Word.Not(SECTION_ALIGN - 1));
    END
  END pad_if;

PROCEDURE section_ignorable (t: T; READONLY sect: SectionHeader): BOOLEAN
            RAISES {M3Loader.LoadError} =
  BEGIN
    IF Word.And(sect.flags, Word.Or(IMAGE_SCN_LNK_INFO,
                                    IMAGE_SCN_MEM_DISCARDABLE)) # 0 OR
       group_name(sect.name, ".edata") THEN
      RETURN TRUE;
    ELSIF group_name(sect.name, ".idata") THEN
      deal_with_idata(t, sect);
      RETURN FALSE;
    ELSE
      RETURN FALSE;
    END
  END section_ignorable;

PROCEDURE deal_with_idata (t: T; READONLY sect: SectionHeader)
            RAISES {M3Loader.LoadError} =
  VAR
    ordinal : INTEGER;
  BEGIN
    IF group_leaf(sect.name, "2") THEN
      IF t.obj.library.dll # NIL THEN
        error("Unrecognised DLL format in library " &
                M3ID.ToText(t.obj.library.name) & " object " &
                M3ID.ToText(t.obj.name));
        RAISE M3Loader.LoadError;
      END;

      t.obj.library.dll := NEW(M3LoaderProcess.DllLib,
                               name := M3ID.Add(get_idata_name(t, 0)));

      RETURN;
    ELSIF group_leaf(sect.name, "5") THEN
      IF sect.nrelocs = 0 THEN
        ordinal := to_int(t.filebuffer, sect.rawoffset, 4);

        IF Word.And(ordinal, 16_80000000) # 0 THEN
          t.obj.dllordinal := Word.And(ordinal, 16_FFFF);
        END
      ELSE
        t.obj.dllname := M3ID.Add(get_idata_name(t, 2));
      END;

      IF t.nimportrelocs = NUMBER(t.importrelocs^) THEN
        expand_importrelocs(t);
      END
    END
  END deal_with_idata;

PROCEDURE get_idata_name (t: T; offs: INTEGER): TEXT 
            RAISES {M3Loader.LoadError} =
  BEGIN
    FOR i := 0 TO t.nsections-1 DO
      WITH sect = t.section_header[i] DO
        IF Text.Equal(sect.name, ".idata$6") THEN
          RETURN to_text(t.filebuffer, sect.rawoffset + offs,
                         NUMBER(t.filebuffer^));
        END
      END
    END;

    error("Unrecognised DLL format in library " &
      M3ID.ToText(t.obj.library.name));
    RAISE M3Loader.LoadError;
  END get_idata_name;

PROCEDURE section_unsupported (READONLY sect: SectionHeader): BOOLEAN =
  BEGIN
    IF Word.And(sect.flags, IMAGE_SCN_LNK_OVERLAY) # 0 OR
       group_name(sect.name, ".tls") THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END
  END section_unsupported;

PROCEDURE group_name (fullname, groupname: TEXT): BOOLEAN =
  VAR
    place: INTEGER;
  BEGIN
    place := Text.FindChar(fullname, '$');
    IF place = -1 THEN
      RETURN Text.Equal(fullname, groupname);
    ELSE
      RETURN Text.Equal(Text.Sub(fullname, 0, place), groupname);
    END
  END group_name;

PROCEDURE group_leaf (fullname, groupleaf: TEXT): BOOLEAN =
  VAR
    place: INTEGER;
  BEGIN
    place := Text.FindChar(fullname, '$');
    IF place = -1 THEN
      RETURN FALSE;
    ELSE
      RETURN Text.Equal(Text.Sub(fullname, place+1), groupleaf);
    END
  END group_leaf;

PROCEDURE allocate_segments (t: T) RAISES {M3Loader.LoadError} =
  BEGIN
    Debug.Txt(M3ID.ToText(t.obj.name) & "; ");
    FOR i := FIRST(SegType) TO LAST(SegType) DO
      TRY
        t.obj.segment[i] := t.process.allocate(t.obj.segsize[i], i);
      EXCEPT
        M3LoaderProcess.AllocateError =>
          error("Out of memory in allocator, segment " & Fmt.Int(ORD(i)));
          RAISE M3Loader.LoadError;
      END;
      Debug.Int(t.obj.segment[i].address);
      Debug.Txt(" ");
    END;
    Debug.NL();

    FOR i := 0 TO t.nsections-1 DO
      WITH section = t.section_header[i] DO
        IF section.used AND section.rawsize > 0 AND
           section.seg # SegType.Bss THEN
          M3LoaderUnsafe.copy_buf_to_seg(t.filebuffer, section.rawoffset,
                                         t.obj.segment[section.seg].address,
                                         section.pos, section.rawsize);
          IF section.vsize > section.rawsize THEN
            M3LoaderUnsafe.zero_data(t.obj.segment[section.seg].address+
                                     section.pos+section.rawsize,
                                     section.vsize-section.rawsize);
          END
        END
      END
    END
  END allocate_segments;

PROCEDURE legalreloc(type: INTEGER): BOOLEAN =
  BEGIN
    FOR i := FIRST(M3LoaderAccess.RelocTable) TO
             LAST(M3LoaderAccess.RelocTable) DO
      IF type = M3LoaderAccess.RelocTable[i] THEN
        RETURN TRUE;
      END
    END;
    RETURN FALSE;
  END legalreloc;

PROCEDURE process_relocations (t: T) RAISES {M3Loader.LoadError} =
  VAR
    offs,
    sym,
    reloctype,
    storage_class,
    section,
    value,
    symaddr        : INTEGER;
  BEGIN
    t.nimports := 0;
    t.nimportrelocs := 0;

    FOR i := 0 TO t.nsections-1 DO
      WITH sect = t.section_header[i] DO
        IF sect.used AND NOT fake_idata_import(t, sect) THEN
          FOR reloc := 0 TO sect.nrelocs-1 DO
            WITH relocaddr = sect.relocs + RelocSize * reloc DO
              offs := to_int(t.filebuffer, relocaddr, 4) - sect.offset +
                          sect.pos;
              sym := t.symtable + to_int(t.filebuffer, relocaddr + 4, 4) *
                                  SymTabSize;
              reloctype := to_int(t.filebuffer, relocaddr + 8, 2);
              IF NOT legalreloc(reloctype) THEN
                RAISE M3Loader.LoadError;
              END
            END;

            storage_class := to_int(t.filebuffer, sym + 16, 1);
            section := to_int(t.filebuffer, sym + 12, 2);
            value := to_int(t.filebuffer, sym+8, 4);

            IF storage_class = IMAGE_SYM_CLASS_EXTERNAL_DEF OR
              section = 0 THEN
              store_import(t, sect.seg, offs, reloctype, sym);
            ELSE
              IF t.section_header[section-1].used THEN
                symaddr := get_sym_addr(t, section, value, storage_class);
                do_reloc(t.obj.segment[sect.seg], offs, symaddr,
                         reloctype, TRUE);
              END
            END
          END
        END
      END
    END;

    t.obj.imports := NEW(REF ARRAY OF M3ID.T, t.nimports);
    SUBARRAY(t.obj.imports^, 0, t.nimports) :=
      SUBARRAY(t.imports^, 0, t.nimports);

    t.obj.relocs := NEW(REF ARRAY OF Relocation, t.nimportrelocs);
    t.obj.nrelocs := t.nimportrelocs;
    SUBARRAY(t.obj.relocs^, 0, t.nimportrelocs) :=
      SUBARRAY(t.importrelocs^, 0, t.nimportrelocs);
  END process_relocations;

PROCEDURE get_sym_addr (t: T; section, value, storage_class: INTEGER):
            INTEGER RAISES {M3Loader.LoadError} =
  VAR
    pos           : INTEGER;
    seg           : SegType;
  BEGIN
    IF section = 0 OR section < -1 THEN
      error("Bad object " & M3ID.ToText(t.obj.name));
      RAISE M3Loader.LoadError;
    END;

    IF section = -1 THEN
      RETURN value;
    END;

    DEC(section); (* Microsoft counts sections from 1 *)

    WITH sect = t.section_header[section] DO
      seg := sect.seg;
      pos := sect.pos;
    END;

    IF storage_class # IMAGE_SYM_CLASS_EXTERNAL AND
       storage_class # IMAGE_SYM_CLASS_STATIC AND
       storage_class # IMAGE_SYM_CLASS_LABEL THEN
      error("Bad object " & M3ID.ToText(t.obj.name));
      RAISE M3Loader.LoadError;
    END;

    RETURN t.obj.segment[seg].address + pos + value;
  END get_sym_addr;

PROCEDURE store_import (t: T; seg: SegType; offs, type, sym: INTEGER)
             RAISES {M3Loader.LoadError} =
  VAR
    nametest : INTEGER;
    name     : M3ID.T;
  BEGIN
    IF to_int(t.filebuffer, sym+12, 2) # 0 THEN
      error("Bad object " & M3ID.ToText(t.obj.name));
      RAISE M3Loader.LoadError;
    END;

    nametest := to_int(t.filebuffer, sym, 4);
    IF nametest = -1 THEN
      name := to_int(t.filebuffer, sym+4, 4);
    ELSE
      IF nametest = 0 THEN
        name := M3ID.Add(getstring(t, to_int(t.filebuffer, sym+4, 4)));
      ELSE
        name := M3ID.Add(to_text(t.filebuffer, sym, 8));
      END;

      from_int(t.filebuffer, sym, 4, -1);
      from_int(t.filebuffer, sym+4, 4, name);

      IF t.nimports = NUMBER(t.imports^) THEN
        expand_imports(t);
      END;

      t.imports[t.nimports] := name;
      INC(t.nimports);
    END;

    IF t.nimportrelocs = NUMBER(t.importrelocs^) THEN
      expand_importrelocs(t);
    END;

    t.importrelocs[t.nimportrelocs].symbol := name;
    t.importrelocs[t.nimportrelocs].seg := seg;
    t.importrelocs[t.nimportrelocs].offs := offs;
    t.importrelocs[t.nimportrelocs].type := type;

    INC(t.nimportrelocs);
  END store_import;

PROCEDURE fill_in_exports (t: T) RAISES {M3Loader.LoadError} =
  VAR
    symcount       := 0;
    section,
    value,
    complex_type,
    storage_class  : INTEGER;
  BEGIN
    t.nexports := 0;

    t.obj.labels := NEW(SortedIntRefTbl.Default).init();

    WHILE symcount < t.nsymbols DO
      WITH symbase = t.symtable + symcount * SymTabSize DO
        value := to_int(t.filebuffer, symbase + 8, 4);
        section := to_int(t.filebuffer, symbase + 12, 2);
        storage_class := to_int(t.filebuffer, symbase + 16, 1);

        IF storage_class = IMAGE_SYM_CLASS_EXTERNAL THEN
          IF section # 0 THEN
            IF section < 0 OR t.section_header[section-1].used THEN
              add_export(t, symbase, section, value, storage_class);
            END
          ELSIF value > 0 THEN
            add_linker_defined(t, symbase, value);
          END
        END;

        IF section > 0 AND t.section_header[section-1].seg = SegType.Text THEN
          complex_type := to_int(t.filebuffer, symbase + 14, 1);
          IF Word.And(complex_type, 16_F0) = 16_20 THEN
            add_label(t, symbase, section, value, storage_class);
          END
        END;

        INC(symcount, 1 + to_int(t.filebuffer, symbase+17, 1));
      END
    END;

    t.obj.exports := NEW(REF ARRAY OF Symbol, t.nexports);
    SUBARRAY(t.obj.exports^, 0, t.nexports) :=
      SUBARRAY(t.exports^, 0, t.nexports);

    t.obj.linker_defined := NEW(REF ARRAY OF LinkDef, t.nlinkdefs);
    IF t.nlinkdefs > 0 THEN
      SUBARRAY(t.obj.linker_defined^, 0, t.nlinkdefs) :=
          SUBARRAY(t.linkdefs^, 0, t.nlinkdefs);
    END;
  END fill_in_exports;

PROCEDURE add_line_numbers (t: T) =
  VAR
    curproc : ProcLabel := NIL;
    lineno  : INTEGER;
  BEGIN
    TRY
      FOR i := 0 TO t.nsections - 1 DO
        WITH section = t.section_header[i] DO
          IF section.used AND section.nlines > 0 THEN
            IF to_int(t.filebuffer, section.lines+4, 2) # 0 THEN
              error("Warning: First linenumber is not a function pointer in " &
                      M3ID.ToText(t.obj.name));
              RAISE M3Loader.LoadError;
            END;
            curproc := get_line_proc(t, section.lines);
            FOR line := 1 TO section.nlines - 1 DO
              WITH lineaddr = section.lines + (line * 6) DO
                lineno := to_int(t.filebuffer, lineaddr + 4, 2);
                IF lineno = 0 THEN
                  curproc := get_line_proc(t, lineaddr);
                ELSE
                  EVAL curproc.lines.put(to_int(t.filebuffer, lineaddr, 4) +
                                         section.pos +
                                         t.obj.segment[section.seg].address,
                                         lineno);
                END
              END
            END
          END
        END
      END
    EXCEPT
      M3Loader.LoadError =>
        IF curproc # NIL THEN
          curproc.lines := NIL;
        END
    END
  END add_line_numbers;

PROCEDURE get_line_proc (t: T; lineaddr: INTEGER): ProcLabel
            RAISES {M3Loader.LoadError} =
  VAR
    storage_class,
    section,
    value,
    addr,
    bf,
    sym            : INTEGER;
    dummy          : REFANY;
    proc           : ProcLabel;
  BEGIN
    sym := t.symtable + to_int(t.filebuffer, lineaddr, 4)*SymTabSize;
    storage_class := to_int(t.filebuffer, sym + 16, 1);
    section := to_int(t.filebuffer, sym + 12, 2);
    value := to_int(t.filebuffer, sym+8, 4);
    addr := get_sym_addr(t, section, value, storage_class);

    IF NOT t.obj.labels.get(addr, dummy) THEN
      error("Warning: No procedure corresponding to linenumber in " &
              M3ID.ToText(t.obj.name));
      RAISE M3Loader.LoadError;
    END;

    proc := dummy;

    IF proc.lines # NIL THEN
      error("Warning: Duplicate sets of lines for procedure " &
              M3ID.ToText(proc.name) & " in " &
              M3ID.ToText(t.obj.name));
      RAISE M3Loader.LoadError;
    END;

    IF to_int(t.filebuffer, sym+17, 1) # 1 THEN
      error("Warning: Procedure has no auxiliary symbol in " &
             M3ID.ToText(t.obj.name));
      RAISE M3Loader.LoadError;
    END;

    bf := t.symtable + to_int(t.filebuffer, sym+SymTabSize, 4)*SymTabSize;

    IF to_int(t.filebuffer, bf+16, 1) # 101 THEN
      error("Warning: Couldn't find .bf symbol in " & M3ID.ToText(t.obj.name));
      RAISE M3Loader.LoadError;
    END;

    IF to_int(t.filebuffer, bf+17, 1) # 1 THEN
      error("Warning: .bf symbol has no auxiliary symbol in " &
                  M3ID.ToText(t.obj.name));
      RAISE M3Loader.LoadError;
    END;

    proc.baseline := to_int(t.filebuffer, bf+SymTabSize+4, 2);

    proc.lines := NEW(SortedIntIntTbl.Default).init();

    RETURN proc;
  END get_line_proc;

PROCEDURE add_label (t: T; symbase, section, value, storage_class: INTEGER)
            RAISES {M3Loader.LoadError} =
  VAR
    name := get_sym_name(t, symbase);
    addr := get_sym_addr(t, section, value, storage_class);
    proc := NEW(ProcLabel, name := name);
  BEGIN
    EVAL t.obj.labels.put(addr, proc);
  END add_label;

PROCEDURE add_linker_defined (t: T; symbase, size: INTEGER) =
  VAR
    name := get_sym_name(t, symbase);
  BEGIN
    IF t.nlinkdefs = NUMBER(t.linkdefs^) THEN
      expand_linkdefs(t);
    END;

    t.linkdefs[t.nlinkdefs] := NEW(LinkDef, name := name,
                                            size := size);
    INC(t.nlinkdefs);
  END add_linker_defined;

PROCEDURE get_sym_name (t: T; symbase: INTEGER): M3ID.T =
  VAR
    nametest : INTEGER;
    name     : M3ID.T;
  BEGIN
    nametest := to_int(t.filebuffer, symbase, 4);

    IF nametest = -1 THEN
      name := to_int(t.filebuffer, symbase+4, 4);
    ELSE
      IF nametest = 0 THEN
        name := M3ID.Add(getstring(t, to_int(t.filebuffer, symbase+4, 4)));
      ELSE
        name := M3ID.Add(to_text(t.filebuffer, symbase, 8));
      END;
    END;

    RETURN name;
  END get_sym_name;

PROCEDURE fake_idata_import (t: T; READONLY sect: SectionHeader): BOOLEAN
            RAISES {M3Loader.LoadError} =
  VAR
    name     : M3ID.T;
  BEGIN
    IF (NOT group_name(sect.name, ".idata")) OR
       (NOT group_leaf(sect.name, "5")) THEN RETURN FALSE END;

    IF t.obj.dllname = M3ID.NoID THEN
      IF t.obj.dllordinal = -1 THEN
        error("Unrecognised DLL format in library " &
                M3ID.ToText(t.obj.library.name));
        RAISE M3Loader.LoadError;
      END;
      name := M3ID.Add("__dllord__" & M3ID.ToText(t.obj.library.dll.name) &
                       Fmt.Int(t.obj.dllordinal));
    ELSE
      name := M3ID.Add("__dllimp__" & M3ID.ToText(t.obj.dllname));
    END;

    IF t.nimports = NUMBER(t.imports^) THEN
      expand_imports(t);
    END;

    t.imports[t.nimports] := name;
    INC(t.nimports);

    IF t.nimportrelocs = NUMBER(t.importrelocs^) THEN
      expand_importrelocs(t);
    END;

    t.importrelocs[t.nimportrelocs].symbol := name;
    t.importrelocs[t.nimportrelocs].seg := SegType.Data;
    t.importrelocs[t.nimportrelocs].offs := sect.pos;
    t.importrelocs[t.nimportrelocs].type := 6;

    INC(t.nimportrelocs);

    RETURN TRUE;
  END fake_idata_import;

PROCEDURE add_export (t: T; symbase, section, value, storage_class: INTEGER)
            RAISES {M3Loader.LoadError} =
  VAR
    addr,
    nametest : INTEGER;
    name     : M3ID.T;
  BEGIN
    IF t.nexports = NUMBER(t.exports^) THEN
      expand_exports(t);
    END;

    nametest := to_int(t.filebuffer, symbase, 4);

    IF nametest = -1 THEN
      name := to_int(t.filebuffer, symbase+4, 4);
    ELSE
      IF nametest = 0 THEN
        name := M3ID.Add(getstring(t, to_int(t.filebuffer, symbase+4, 4)));
      ELSE
        name := M3ID.Add(to_text(t.filebuffer, symbase, 8));
      END;
    END;

    addr := get_sym_addr(t, section, value, storage_class);

    t.exports[t.nexports] := NEW(Symbol, name := name,
                                         address := addr,
                                         parent := t.obj);

    INC(t.nexports);
  END add_export;

PROCEDURE getstring (t: T; offset: INTEGER): TEXT =
  BEGIN
    RETURN to_text(t.filebuffer, offset+t.stringtable, t.filebufsize);
  END getstring;

PROCEDURE expand_imports (t: T) =
  VAR
    newarr := NEW(REF ARRAY OF M3ID.T, NUMBER(t.imports^) * 2);
  BEGIN
    SUBARRAY(newarr^, 0, NUMBER(t.imports^)) := t.imports^;
    t.imports := newarr;
  END expand_imports;

PROCEDURE expand_exports (t: T) =
  VAR
    newarr := NEW(REF ARRAY OF Symbol, NUMBER(t.exports^) * 2);
  BEGIN
    SUBARRAY(newarr^, 0, NUMBER(t.exports^)) := t.exports^;
    t.exports := newarr;
  END expand_exports;

PROCEDURE expand_linkdefs (t: T) =
  VAR
    newarr := NEW(REF ARRAY OF LinkDef, NUMBER(t.linkdefs^) * 2);
  BEGIN
    SUBARRAY(newarr^, 0, NUMBER(t.linkdefs^)) := t.linkdefs^;
    t.linkdefs := newarr;
  END expand_linkdefs;

PROCEDURE expand_importrelocs (t: T) =
  VAR
    newarr := NEW(REF ARRAY OF Relocation, NUMBER(t.importrelocs^) * 2);
  BEGIN
    SUBARRAY(newarr^, 0, NUMBER(t.importrelocs^)) := t.importrelocs^;
    t.importrelocs := newarr;
  END expand_importrelocs;

PROCEDURE New (process: M3LoaderProcess.T): T =
  VAR t := NEW(T, process := process);
  BEGIN
    t.filebuffer := NEW(Buffer, t.filebufsize);
    t.lib_header_buf := NEW(Buffer, LIB_HEADER_SIZE);
    t.imports := NEW(REF ARRAY OF M3ID.T, 64);
    t.exports := NEW(REF ARRAY OF Symbol, 64);
    t.linkdefs := NEW(REF ARRAY OF LinkDef, 64);
    t.importrelocs := NEW(REF ARRAY OF Relocation, 512);
    RETURN t;
  END New;

BEGIN
END M3LoaderRd.
