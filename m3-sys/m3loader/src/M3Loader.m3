(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Nov 10 15:32:28 PST 1994 by isard      *)

MODULE M3Loader;

IMPORT IntRefTbl, Text, IO, Fmt, Time, Wr, Stdio;
IMPORT M3ID, M3LoaderObj, M3LoaderRd, M3LoaderDebug AS Debug;
IMPORT M3LoaderProcess;

FROM M3LoaderObj IMPORT Symbol, FileType;
FROM M3LoaderAccess IMPORT Segment, SegType;

REVEAL
  T = Public BRANDED "M3Loader.T" OBJECT
    process               : M3LoaderProcess.T;
    reader                : M3LoaderRd.T;
    symbols               : IntRefTbl.Default := NIL;
    objects               : ObjList := NIL;
    possible_link_defined : LinkDefList := NIL;
    linker_segment        : Segment;
    libs                  : LibModule := NIL;
    dlls                  : M3LoaderProcess.DllLib := NIL;
    ndlls                 := 0;
    unresolved            := 0;
    just_called           := FALSE;
  OVERRIDES
    load_obj := load_obj;
    unload_obj := unload_obj;
    load_lib := load_lib;
    unload_lib := unload_lib;
    call := call;
    shutdown := shutdown;
    show_stuff := show_stuff;
  END;

TYPE
  SymbolStruct = REF RECORD
    sym     : ARRAY FileType OF Symbol;
    size    : INTEGER := 0;
    clients : ClientList := NIL;
  END;

TYPE
  ClientList = REF RECORD
    obj    : ObjModule;
    relind : INTEGER;
    next   : ClientList;
  END;

TYPE
  LinkDefList = REF RECORD
    name : M3ID.T;
    pos  : INTEGER := 0;
    offs := 0;
    next : LinkDefList;
  END;

PROCEDURE error (msg: TEXT) =
  <* FATAL ANY *>
  BEGIN
    Wr.PutText(Stdio.stderr, msg & "\n");
  END error;

PROCEDURE shutdown (t: T) =
  BEGIN
    t.process.kill_off_last();
  END shutdown;

PROCEDURE call (t: T; cmd_line, cwd, exename: TEXT; console: BOOLEAN;
                start_time: Time.T) =
  VAR
    name        : M3ID.T;
    dummystruct : REFANY;
    struct      : SymbolStruct;
    symname     := "_main";
    sym         : Symbol;
  BEGIN
    IF t.unresolved < 0 THEN
      error("Internal error: t.unresolved negative, not doing call");
      RETURN;
    END;

    restore_data(t);

    IF NOT console THEN
      symname := "_WinMain@16";
    END;

    IF t.unresolved > 0 THEN
      flag_unresolved(t);
      RETURN;
    END;

    name := M3ID.Add(symname);
    IF NOT t.symbols.get(name, dummystruct) THEN
      error("Asked to call symbol " & symname & " which is undefined.");
      RETURN;
    END;

    struct := dummystruct;

    IF struct.sym[FileType.Obj] # NIL THEN
      sym := struct.sym[FileType.Obj];
    ELSIF struct.sym[FileType.Lib] # NIL THEN
      sym := struct.sym[FileType.Lib];
    ELSE
      error("Internal Error: asked to call unresolved symbol. Not doing call");
      RETURN;
    END;

    Debug.Txt("Calling symbol ");
    Debug.Name(sym.name);
    Debug.Txt(" from object ");
    Debug.Name(sym.parent.name);
    Debug.NL();

    IF NOT pre_call_link_defined(t) THEN RETURN END;

    IF t.process.start(t.dlls, t.ndlls, cmd_line, cwd, exename, console) THEN
      relocate_dlls(t, TRUE);

      t.process.initialise_data();

      IO.Put("Elapsed time since 'go': ");
      IO.Put(Fmt.LongReal(Time.Now()-start_time, Fmt.Style.Fix, 2));
      IO.Put(" seconds\n");

      t.process.call(sym.address, t.objects);

      t.just_called := TRUE;
    ELSE
      post_call_link_defined(t);
    END
  END call;

PROCEDURE relocate_dlls (t: T; load: BOOLEAN) =
  VAR
    dlllib := t.dlls;
    dllsym : M3LoaderProcess.DllSymbol;
    struct : SymbolStruct;
  BEGIN
    WHILE dlllib # NIL DO
      dllsym := dlllib.dllsyms;
      WHILE dllsym # NIL DO
        struct := get_struct(t, dllsym.mangledname);
        relocate_clients(struct.clients, dllsym.address, load);
        dllsym := dllsym.next;
      END;
      dlllib := dlllib.next;
    END
  END relocate_dlls;

PROCEDURE pre_call_link_defined (t: T): BOOLEAN =
  VAR
    symbol      := t.possible_link_defined;
    dummystruct : REFANY;
    struct      : SymbolStruct;
    linkdefsize := 0;
  BEGIN
    WHILE symbol # NIL DO
      IF t.symbols.get(symbol.name, dummystruct) THEN
        struct := dummystruct;
        IF struct.sym[FileType.Obj] = NIL AND
          struct.sym[FileType.Lib] = NIL THEN
          IF struct.size = 0 THEN
            error("Internal error: unresolved symbol not link-defined");
            RETURN FALSE;
          END;

          symbol.offs := linkdefsize;
          INC(linkdefsize, struct.size);
        END
      END;

      symbol := symbol.next;
    END;

    TRY
      t.linker_segment := t.process.allocate(linkdefsize, SegType.Data);
    EXCEPT
      M3LoaderProcess.AllocateError =>
        error("Not enough memory to allocate linker defined symbols");
        RETURN FALSE;
    END;

    symbol := t.possible_link_defined;
    WHILE symbol # NIL DO
      IF t.symbols.get(symbol.name, dummystruct) THEN
        struct := dummystruct;
        IF struct.sym[FileType.Obj] = NIL AND
          struct.sym[FileType.Lib] = NIL THEN
          symbol.pos := t.linker_segment.address + symbol.offs;
          relocate_clients(struct.clients, symbol.pos, TRUE);
        END
      END;

      symbol := symbol.next;
    END;

    RETURN TRUE;
  END pre_call_link_defined;

PROCEDURE post_call_link_defined (t: T) =
  VAR
    symbol      := t.possible_link_defined;
    dummystruct : REFANY;
    struct      : SymbolStruct;
  BEGIN
    WHILE symbol # NIL DO
      IF t.symbols.get(symbol.name, dummystruct) THEN
        struct := dummystruct;
        IF struct.sym[FileType.Obj] = NIL AND
          struct.sym[FileType.Lib] = NIL THEN
          relocate_clients(struct.clients, symbol.pos, FALSE);
        END
      END;

      symbol := symbol.next;
    END;

    TRY
      t.process.free(t.linker_segment);
    EXCEPT
      M3LoaderProcess.AllocateError =>
        error("Interal error: post_call_linker_defined failed to free");
        <* ASSERT FALSE *>
    END
  END post_call_link_defined;

PROCEDURE restore_data(t: T) =
  BEGIN
    IF NOT t.just_called THEN RETURN END;

    t.process.restore_data();

    relocate_dlls(t, FALSE);

    post_call_link_defined(t);

    t.just_called := FALSE;
  END restore_data;

PROCEDURE flag_unresolved (t: T) =
  VAR
    symbols := t.symbols.iterate();
    name    : M3ID.T;
    nameint : INTEGER;
    junk    : REFANY;
    symbol  : SymbolStruct;
    client  : ClientList;
    nametxt : TEXT;
  BEGIN
    IO.Put("There are ");
    IO.PutInt(t.unresolved);
    IO.Put(" unresolved symbols:\n");
    WHILE symbols.next(nameint, junk) DO
      name := nameint;
      nametxt := M3ID.ToText(name);
      symbol := junk;
      IF symbol.sym[FileType.Obj] = NIL AND
         symbol.sym[FileType.Lib] = NIL AND
         symbol.size = 0 AND
         NOT Text.Equal(Text.Sub(nametxt, 0, 5), "__dll") THEN
        IO.Put(nametxt);
        IO.Put("\n");
        client := symbol.clients;
        WHILE client # NIL DO
          IO.Put("-- ");
          IO.Put(M3ID.ToText(client.obj.name));
          IO.Put("\n");
          client := client.next;
        END
      END
    END
  END flag_unresolved;

PROCEDURE show_stuff (t: T) =
  VAR
    symbols := t.symbols.iterate();
    name    : M3ID.T;
    nameint : INTEGER;
    junk    : REFANY;
    symbol  : SymbolStruct;
    client  : ClientList;
  BEGIN
    WHILE symbols.next(nameint, junk) DO
      name := nameint;
      symbol := junk;
      Debug.Name(name);
      Debug.Txt("  :  ");
      IF symbol.sym[FileType.Obj] # NIL THEN
        Debug.Name(symbol.sym[FileType.Obj].parent.name);
        Debug.Txt("  =  ");
        Debug.Int(symbol.sym[FileType.Obj].address);
      ELSIF symbol.sym[FileType.Lib] # NIL THEN
        Debug.Name(symbol.sym[FileType.Lib].parent.name);
        Debug.Txt("  =  ");
        Debug.Int(symbol.sym[FileType.Lib].address);
      ELSIF symbol.size > 0 THEN
        Debug.Txt("Linker defined size ");
        Debug.Int(symbol.size);
      ELSE
        Debug.Txt("Unresolved");
      END;
      Debug.NL();
      client := symbol.clients;
      WHILE client # NIL DO
        Debug.Txt("-- ");
        Debug.Name(client.obj.name);
        Debug.NL();
        client := client.next;
      END
    END;
    Debug.Txt("\n****\n\n");
  END show_stuff;

PROCEDURE load_obj (t: T; filename: TEXT): ObjModule RAISES {LoadError} =
  VAR
    obj : ObjModule;
  BEGIN
    IO.Put("Loading object ");
    IO.Put(filename);
    IO.Put("\n");

    restore_data(t);

    obj := t.reader.read_object(filename);

    assimilate_obj(t, obj);

    RETURN obj;
  END load_obj;

PROCEDURE assimilate_obj (t: T; obj: ObjModule) =
  VAR
    lib_obj   : ObjList := NIL;
  BEGIN
    IF obj.loaded THEN RETURN END;

    EVAL check_for_duplicates(t, obj);

    register_linker_defined(t, obj);

    claim_exports(t, obj);

    lib_obj := resolve_imports(t, obj);

    t.objects := NEW(ObjList, obj := obj, next := t.objects);

    obj.loaded := TRUE;

    WHILE lib_obj # NIL DO
      Debug.Txt("Loading ");
      IF lib_obj.obj.name = M3ID.NoID THEN
        Debug.Txt("DLL stubs");
      ELSE
        Debug.Name(lib_obj.obj.name);
      END;
      Debug.Txt(" from library ");
      Debug.Name(lib_obj.obj.library.name);
      Debug.NL();
      IF t.reader.fill_object(lib_obj.obj, lib_obj.obj.library.file) THEN
        assimilate_obj(t, lib_obj.obj);
      END;
      lib_obj := lib_obj.next;
    END
  END assimilate_obj;

PROCEDURE unload_obj (t: T; obj: ObjModule; silent := FALSE) =
  VAR
    objfind : ObjList;
  BEGIN
    IF silent THEN
      Debug.Txt("Unloading object ");
      Debug.Name(obj.name);
      Debug.NL();
    ELSE
      IO.Put("Unloading object ");
      IO.Put(M3ID.ToText(obj.name));
      IO.Put("\n");
    END;

    restore_data(t);

    relinquish_exports(t, obj);

    relinquish_imports(t, obj);

    obj.discard(t.process);

    IF t.objects.obj = obj THEN
      t.objects := t.objects.next;
    ELSE
      objfind := t.objects;
      WHILE objfind.next # NIL DO
        IF objfind.next.obj = obj THEN
          objfind.next := objfind.next.next;
          EXIT;
        END;

        objfind := objfind.next;
      END
    END
  END unload_obj;

PROCEDURE load_lib (t: T; filename: TEXT): LibModule RAISES {LoadError} =
  VAR
    newlib := t.reader.scan_library(filename);
  BEGIN
    IO.Put("Loading library ");
    IO.Put(filename);
    IO.Put("\n");

    restore_data(t);

    newlib.next := t.libs;
    IF newlib.dll # NIL THEN
      newlib.dll.next := t.dlls;
      t.dlls := newlib.dll;
      INC(t.ndlls);
    END;
    t.libs := newlib;

    IF t.unresolved > 0 THEN
      check_new_library(t);
    END;

    RETURN newlib;
  END load_lib;

PROCEDURE check_unresolved (t: T): ObjList =
  VAR
    symbols := t.symbols.iterate();
    name    : M3ID.T;
    nameint : INTEGER;
    junk    : REFANY;
    symbol  : SymbolStruct;
    nametxt : TEXT;
    lib_obj : ObjList := NIL;
  BEGIN
    WHILE symbols.next(nameint, junk) DO
      name := nameint;
      nametxt := M3ID.ToText(name);
      symbol := junk;
      IF symbol.sym[FileType.Obj] = NIL AND
         symbol.sym[FileType.Lib] = NIL AND
         symbol.size = 0 AND
         NOT Text.Equal(Text.Sub(nametxt, 0, 5), "__dll") THEN
        lib_obj := check_in_libraries(t, name, symbol, lib_obj);
      END
    END;

    RETURN lib_obj;
  END check_unresolved;

PROCEDURE check_new_library (t: T) =
  VAR
    lib_obj := check_unresolved(t);
  BEGIN
    WHILE lib_obj # NIL DO
      Debug.Txt("Loading ");
      Debug.Name(lib_obj.obj.name);
      Debug.Txt(" from library ");
      Debug.Name(lib_obj.obj.library.name);
      Debug.NL();
      IF t.reader.fill_object(lib_obj.obj, lib_obj.obj.library.file) THEN
        assimilate_obj(t, lib_obj.obj);
      END;
      lib_obj := lib_obj.next;
    END
  END check_new_library;

PROCEDURE unload_lib (t: T; lib: LibModule) =
  <* FATAL ANY *>
  BEGIN
    IO.Put("Unloading library ");
    IO.Put(M3ID.ToText(lib.name));
    IO.Put("\n");

    restore_data(t);

    FOR i := 0 TO lib.nmembers-1 DO
      IF lib.members[i].loaded THEN
        unload_obj(t, lib.members[i], silent := TRUE);
      END
    END;

    IF lib.dll # NIL THEN
      IF t.dlls = lib.dll THEN
        t.dlls := t.dlls.next;
      ELSE
        VAR dll := t.dlls;
        BEGIN
          WHILE dll.next # NIL DO
            IF dll.next = lib.dll THEN
              dll.next := dll.next.next;
              EXIT;
            ELSE
              dll := dll.next;
            END
          END
        END
      END;
      DEC(t.ndlls);
    END;

    IF t.libs = lib THEN
      t.libs := t.libs.next
    ELSE
      VAR checklib := t.libs;
      BEGIN
        WHILE checklib.next # NIL DO
          IF checklib.next = lib THEN
            checklib.next := checklib.next.next;
            EXIT;
          ELSE
            checklib := checklib.next;
          END
        END
      END
    END;

    lib.file.close();
  END unload_lib;

PROCEDURE register_linker_defined (t: T; obj: ObjModule) =
  VAR
    struct : SymbolStruct;
  BEGIN
    FOR i := FIRST(obj.linker_defined^) TO LAST(obj.linker_defined^) DO
      WITH link_defined = obj.linker_defined[i] DO
        struct := get_struct(t, link_defined.name);
        IF struct.size = 0 AND struct.clients # NIL THEN
          DEC(t.unresolved);
          Debug.Txt("dec:");
          Debug.Name(link_defined.name);
          Debug.NL();
        END;
        struct.size := MAX(struct.size, link_defined.size);
	add_if_new_link_defined(t, link_defined.name);
      END
    END
  END register_linker_defined;

PROCEDURE add_if_new_link_defined(t: T; name: M3ID.T) =
  VAR
    linkdef := t.possible_link_defined;
  BEGIN
    WHILE linkdef # NIL DO
      IF linkdef.name = name THEN
        RETURN;
      END;
      linkdef := linkdef.next;
    END;

    t.possible_link_defined := NEW(LinkDefList, name := name,
                                   next := t.possible_link_defined);
  END add_if_new_link_defined;

PROCEDURE check_for_duplicates (t: T; obj: ObjModule): BOOLEAN =
  VAR
    struct  : SymbolStruct;
  BEGIN
    FOR i := FIRST(obj.exports^) TO LAST(obj.exports^) DO
      WITH export = obj.exports[i] DO
        struct := get_struct(t, export.name);

        IF struct.sym[obj.type] # NIL THEN
          flag_duplicates(export.name, struct.sym[obj.type].parent.name,
                          obj.name);
        END
      END
    END;

    RETURN TRUE;
  END check_for_duplicates;

PROCEDURE claim_exports (t: T; obj: ObjModule) =
  VAR
    struct  : SymbolStruct;
  BEGIN
    FOR i := FIRST(obj.exports^) TO LAST(obj.exports^) DO
      WITH export = obj.exports[i] DO
        struct := get_struct(t, export.name);
        IF struct.sym[obj.type] = NIL THEN
          IF struct.sym[FileType.Lib] # NIL THEN
            relocate_clients(struct.clients, struct.sym[FileType.Lib].address,
                             FALSE);
          ELSE
            IF struct.clients # NIL THEN
              DEC(t.unresolved);
              Debug.Txt("dec:");
              Debug.Name(export.name);
              Debug.NL();
            END
          END;

          struct.sym[obj.type] := export;

          relocate_clients(struct.clients, export.address, TRUE);
        END
      END
    END
  END claim_exports;

PROCEDURE relocate_clients (client: ClientList; addr: INTEGER; add: BOOLEAN) =
  BEGIN
    WHILE client # NIL DO
      client.obj.relocate(addr, client.relind, add);
      client := client.next;
    END
  END relocate_clients;

PROCEDURE flag_duplicates (sym, obj1, obj2: M3ID.T) =
  BEGIN
    IO.Put("Duplicate symbol ");
    IO.Put(M3ID.ToText(sym));
    IO.Put(" in ");
    IO.Put(M3ID.ToText(obj1));
    IO.Put(" and ");
    IO.Put(M3ID.ToText(obj2));
    IO.Put("\n");
  END flag_duplicates;

PROCEDURE relinquish_exports (t: T; obj: ObjModule) =
  VAR
    struct : SymbolStruct;
  BEGIN
    FOR i := FIRST(obj.exports^) TO LAST(obj.exports^) DO
      WITH export = obj.exports[i] DO
        struct := get_struct(t, export.name);
        IF struct.sym[obj.type].parent = obj THEN
          IF obj.type = FileType.Obj OR
             struct.sym[FileType.Obj] = NIL THEN
            IF struct.clients # NIL THEN
              relocate_clients(struct.clients, struct.sym[obj.type].address,
                               FALSE);
              INC(t.unresolved);
              Debug.Txt("inc:");
              Debug.Name(export.name);
              Debug.NL();
            END
          END;

          struct.sym[obj.type] := NIL;

          IF struct.sym[FileType.Lib] # NIL THEN
            relocate_clients(struct.clients, struct.sym[FileType.Lib].address,
                             TRUE);
            DEC(t.unresolved);
            Debug.Txt("dec:");
            Debug.Name(export.name);
            Debug.NL();
          ELSE
            IF empty_struct(struct) THEN
              delete_struct(t, export.name);
            END
          END
        END
      END
    END
  END relinquish_exports;

PROCEDURE remove_dll_name (dlllib: M3LoaderProcess.DllLib; name: M3ID.T) =
  VAR
    dllsym := dlllib.dllsyms;
  BEGIN
    DEC(dlllib.nsyms);
    IF dllsym.mangledname = name THEN
      dlllib.dllsyms := dlllib.dllsyms.next;
    ELSE
      WHILE dllsym.next # NIL DO
        IF dllsym.next.name = name THEN
          dllsym.next := dllsym.next.next;
          EXIT
        END;
        dllsym := dllsym.next;
      END
    END
  END remove_dll_name;

PROCEDURE relinquish_imports (t: T; obj: ObjModule) =
  VAR
    prev,
    client : ClientList;
    struct : SymbolStruct;
  BEGIN
    FOR i := FIRST(obj.imports^) TO LAST(obj.imports^) DO
      struct := get_struct(t, obj.imports[i]);

      prev := struct.clients;
      IF prev.obj = obj THEN
        struct.clients := prev.next;
      ELSE
        client := prev.next;
        WHILE client # NIL DO
          IF client.obj = obj THEN
            prev.next := client.next;
            EXIT;
          END;

          prev := client;
          client := client.next;
        END
      END;

      IF empty_struct(struct) THEN
        IF Text.Equal(Text.Sub(M3ID.ToText(obj.imports[i]), 0, 5),
                      "__dll") THEN
          remove_dll_name(obj.library.dll, obj.imports[i]);
        ELSE
          DEC(t.unresolved);
          Debug.Txt("inc:");
          Debug.Name(obj.imports[i]);
          Debug.NL();
        END;
        delete_struct(t, obj.imports[i]);
      END
    END
  END relinquish_imports;

PROCEDURE resolve_imports (t: T; obj: ObjModule): ObjList =
  VAR
    struct          : SymbolStruct;
    lib_obj_to_load : ObjList := NIL;
  BEGIN
    FOR i := FIRST(obj.imports^) TO LAST(obj.imports^) DO
      struct := get_struct(t, obj.imports[i]);

      struct.clients := NEW(ClientList, obj := obj,
                                        relind := i,
                                        next := struct.clients);

      IF Text.Equal(Text.Sub(M3ID.ToText(obj.imports[i]), 0, 5),
                    "__dll") THEN
        INC(obj.library.dll.nsyms);
        obj.library.dll.dllsyms :=
            NEW(M3LoaderProcess.DllSymbol,
                mangledname := obj.imports[i],
                next := obj.library.dll.dllsyms);
        IF obj.dllordinal = -1 THEN
          obj.library.dll.dllsyms.name := obj.dllname;
        ELSE
          obj.library.dll.dllsyms.ordinal := obj.dllordinal;
        END
      ELSE
        IF struct.sym[FileType.Obj] # NIL THEN
          obj.relocate(struct.sym[FileType.Obj].address, i, TRUE);
        ELSIF struct.sym[FileType.Lib] # NIL THEN
          obj.relocate(struct.sym[FileType.Lib].address, i, TRUE);
        ELSIF struct.size = 0 THEN
          IF struct.clients.next = NIL THEN
            Debug.Txt("inc:");
            Debug.Name(obj.imports[i]);
            Debug.NL();
            INC(t.unresolved);
          END;
          lib_obj_to_load := check_in_libraries(t, obj.imports[i],
                                                struct, lib_obj_to_load);
        END
      END
    END;

    RETURN lib_obj_to_load;
  END resolve_imports;

PROCEDURE check_in_libraries (t: T; import: M3ID.T; struct: SymbolStruct;
                              already: ObjList): ObjList =
  VAR
    libmember := t.libs;
    check     : ObjList := NIL;
    found     := FALSE;
    obj       : ObjModule;
    dummyref  : REFANY;
  BEGIN
    WHILE libmember # NIL AND NOT found DO
      IF libmember.exports.get(import, dummyref) THEN
        obj := dummyref;

        IF obj.loaded AND struct.size = 0 THEN
          error("Library " & M3ID.ToText(obj.library.name) &
                  " claims that object " & M3ID.ToText(obj.name) &
                  " exports symbol " & M3ID.ToText(import) &
                  " but the object thinks otherwise");
          RETURN already;
        END;

        IF NOT obj.loaded THEN
          check := already;

          WHILE check # NIL AND NOT found DO
            IF check.obj = obj THEN
              found := TRUE;
            END;

            check := check.next;
          END;

          IF NOT found THEN
            already := NEW(ObjList, obj := obj,
                                    next := already);
          END;
        END;

        found := TRUE;
      END;

      libmember := libmember.next;
    END;

    RETURN already;
  END check_in_libraries;

PROCEDURE get_struct (t: T; name: M3ID.T): SymbolStruct =
  VAR
    structref : REFANY;
    struct    : SymbolStruct;
  BEGIN
    IF t.symbols.get(name, structref) THEN
      struct := structref;
    ELSE
      struct := NEW(SymbolStruct);
      struct.sym[FileType.Obj] := NIL;
      struct.sym[FileType.Lib] := NIL;
      EVAL t.symbols.put(name, struct);
    END;

    RETURN struct;
  END get_struct;

PROCEDURE delete_struct (t: T; name: M3ID.T) =
  VAR
    dummy: REFANY;
  BEGIN
    EVAL t.symbols.delete(name, dummy);
  END delete_struct;

PROCEDURE empty_struct (struct: SymbolStruct): BOOLEAN =
  BEGIN
    RETURN struct.sym[FileType.Obj] = NIL AND
           struct.sym[FileType.Lib] = NIL AND
           struct.size = 0 AND
           struct.clients = NIL;
  END empty_struct;

PROCEDURE New (): T =
  VAR
    t := NEW(T, process := M3LoaderProcess.New(),
                symbols := NEW(IntRefTbl.Default));
  BEGIN
    t.reader := M3LoaderRd.New(t.process);
    t.symbols := t.symbols.init();
    RETURN t;
  END New;

BEGIN
END M3Loader. 
