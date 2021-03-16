(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE Builder;

IMPORT Text, Wr, Stdio, IntIntTbl AS IntSet;
IMPORT OSError, Fmt, IntRefTbl;
IMPORT FS, File, Time, Fingerprint;
IMPORT Thread, ETimer, Dirs;
IMPORT M3File, M3ID, M3CG, M3Timers, M3Front, WebFile;
IMPORT Mx, MxMerge, MxCheck, MxGen, MxIn, MxOut, MxVS;
IMPORT Msg, Arg, Utils, M3Path, M3Backend, M3Compiler;
IMPORT Quake, QMachine, QValue, QVal, QVSeq;
IMPORT M3Loc, M3Unit, M3Options, MxConfig;
IMPORT QIdent;
IMPORT Target; 
FROM M3Path IMPORT OSKind, OSKindStrings;
IMPORT Pathname;
IMPORT QPromise, QPromiseSeq, RefSeq;

TYPE
  UK = M3Unit.Kind;

(*------------------------------------------------- external entry points ---*)

PROCEDURE BuildPgm (prog: TEXT;  READONLY units: M3Unit.Set;
                    sys_libs: Arg.List;  shared: BOOLEAN;  m: Quake.Machine) =
  VAR s := CompileUnits (prog, units, sys_libs, UK.PGMX, m);
  BEGIN
    IF s.bootstrap_mode
      THEN BuildBootProgram (s);
      ELSE BuildProgram (s, shared);
    END;
    IF s.compile_failed THEN M3Options.exit_code := 1; END;
  END BuildPgm;

PROCEDURE BuildLib (lib: TEXT;  READONLY units: M3Unit.Set;
                    sys_libs: Arg.List;  shared: BOOLEAN;  m: Quake.Machine) =
  VAR s := CompileUnits (lib, units, sys_libs, UK.LIBX, m);
  BEGIN
    IF s.bootstrap_mode
      THEN BuildBootLibrary (s);
      ELSE BuildLibrary (s, shared);
    END;
    IF s.compile_failed THEN M3Options.exit_code := 1; END;
  END BuildLib;

PROCEDURE JustCompile (READONLY units: M3Unit.Set;
                       sys_libs: Arg.List;  m: Quake.Machine) =
  VAR s := CompileUnits ("noname", units, sys_libs, UK.PGMX, m);
  BEGIN
    IF s.compile_failed THEN M3Options.exit_code := 1; END;
  END JustCompile;

PROCEDURE BuildCPgm (prog: TEXT;  READONLY units: M3Unit.Set;
                     sys_libs: Arg.List;  shared: BOOLEAN;  m: Quake.Machine) =
  VAR s := CompileUnits (prog, units, sys_libs, UK.PGMX, m);
  BEGIN
    BuildCProgram (s, shared);
    IF s.compile_failed THEN M3Options.exit_code := 1; END;
  END BuildCPgm;

VAR current_state: State := NIL;

PROCEDURE CleanUp () =
  VAR s := current_state;
  BEGIN
    current_state := NIL;
    IF (s # NIL) THEN
      DumpLinkInfo (s);
      WebFile.Dump ();
    END;
  END CleanUp;

PROCEDURE EmitPkgImports (READONLY units: M3Unit.Set) =
  VAR src := units.head;
  BEGIN
    WHILE (src # NIL) DO
      IF (src.imported) AND (src.kind = UK.M3LIB) THEN
        WITH name = M3ID.ToText(src.loc.pkg) DO
          Msg.Out (" ", name);
        END;
      END;
      src := src.next;
    END;
    Msg.Out (Wr.EOL);
  END EmitPkgImports;

(*-------------------------------------------------- general compilation ---*)
(* The "global" variables of a compilation are passed around in a "State". *)

TYPE
  State = REF RECORD
    result_name   : TEXT;               (* base of program or library name *)
    info_name     : TEXT;               (* name of the version stamp file *)
    config_file   : TEXT;               (* name of the current config file *)
    sys_libs      : Arg.List;           (* linker args for system libraries *)
    machine       : Quake.Machine;      (* to access configuration procs *)
    units         : M3Unit.Set;         (* initial source pool *)
    link_base     : Mx.LinkSet := NIL;  (* accumulated version stamps  *)
    magic         : IntRefTbl.T;        (* type name -> info *)
    ast_cache     : IntRefTbl.T;        (* interface name -> AST *)
    include_path  : Arg.List;           (* -I include path for C compiler *)
    include_path_empty : Arg.List;      (* empty replacement for include_path *)
    pending_impls : M3Unit.TList;       (* deferred implementation modules *)
    main          : M3ID.T;             (* "Main" *)
    m3env         : Env;                (* the compiler's environment closure *)
    target        : TEXT;               (* target machine *)
    (* target_oskind is misused; needs work *)
    target_oskind := M3Path.OSKind.Unix; (* target oskind: Win32 or Unix *)
    m3backend_mode: Target.M3BackendMode_t; (* tells how to turn M3CG -> object *)
    m3backend     : ConfigProc;         (* translate M3CG -> ASM or OBJ *)
    m3llvm        : ConfigProc;         (* translate M3CG -> LLVM bitcode *) 
    llvmbackend   : ConfigProc;         (* translate llvm bitcode -> ASM or OBJ *)
    llvmopt       : ConfigProc;         (* optimize llvm bitcode *)
    c_compiler    : ConfigProc;         (* compile C code *)
    assembler     : ConfigProc;         (* assemble  *)
    librarian     : ConfigProc;         (* make libraries *)
    skip_lib      : ConfigProc;         (* don't make libraries *)
    linker        : ConfigProc;         (* link programs *)
    skip_linker   : ConfigProc;         (* don't link programs *)
    keep_files    : BOOLEAN;            (* don't delete temporary files *)
    compile_failed: BOOLEAN;            (* did anything fail? *)
    new_link_info : BOOLEAN;            (* did we generate any new version stamps?*)
    bootstrap_mode: BOOLEAN;            (* stop compiling at assembly code *)
    compile_once  : BOOLEAN;            (* don't recompile for better code *)
    has_loader    : BOOLEAN;            (* gen loader info file *)
    skip_link     : BOOLEAN;            (* don't bother linking final exe *)
    keep_resolved : BOOLEAN;            (* pass resolved library names to linker *)
    gui           : BOOLEAN;            (* generate a Windows GUI subsystem prog *)
    do_coverage   : BOOLEAN;            (* compile and link for coverage *)
    broken_linker : BOOLEAN;            (* linker can't do build_standalone() *)
    lazy_init     : BOOLEAN;            (* only initialize the main module and
                                           its imports *)
    Rpath_flag    : TEXT;               (* linker needs -R switches too... *)
    link_coverage : TEXT;               (* coverage library *)
    m3_front_flags: Arg.List;           (* configuration options for the front *)
    m3_options    : Arg.List;           (* misc. user options for the frontend *)
    parallelback  : INTEGER;            (* back-end parallelism *)
    
    delayBackend  := FALSE;             (* delay back-end? *)
  END;

TYPE
  ConfigProc = RECORD
    name    : TEXT;
    n_args  : INTEGER;
    binding : QValue.Binding;
  END;

PROCEDURE SetupNamingConventionsInternal (VAR s : State; mach : Quake.Machine) =
  VAR
    value : QValue.Binding;
  BEGIN

    s.machine       := mach;

    (* This area seems to always been messed up, and more work is needed.  In
       particular NAMING_CONVENTIONS and TARGET_NAMING seem to have been
       confused.

       Really, neither one should be configurable in Quake.  The host's naming
       conventions are not relevant. It only cares about slashes.  The
       target's naming conventions should map directly from what the target
       is.  Granted, how to form linker commands is not clearly a host or
       target decision.

       The host has always been probed correctly, and the Quake variables were
       not checked at the right time. Host and target naming conventions rarely
       varied.  Current uses of target_oskind need attention. *)
    
    value := GetDefn (s, "NAMING_CONVENTIONS");
    IF value # NIL THEN
      M3Path.SetTargetOS (ConvertNamingConventionStringToEnum (s, value));
    END;

    value := GetDefn (s, "TARGET_NAMING");
    IF value # NIL THEN
      WITH target_oskind = ConvertNamingConventionStringToEnum (s, value) DO
        s.target_oskind := target_oskind;
        M3Path.SetTargetOS (target_oskind);
      END;
    END;

  END SetupNamingConventionsInternal;

PROCEDURE FormatErrorAvailableEnumValues(
    min, max: INTEGER; READONLY map: ARRAY OF TEXT): TEXT =
VAR result := "\navailable values: ";
BEGIN
    FOR i := min TO max DO
      IF i # min THEN
        result := Text.Cat(result, ", ");
      END;
    result := Text.Cat(result, map[i]);
    END;
    RETURN result;
END FormatErrorAvailableEnumValues;

PROCEDURE ConvertStringToEnum(s: State; name : TEXT; binding: QValue.Binding;
                              min, max: INTEGER; READONLY map: ARRAY OF TEXT):
  INTEGER =
  VAR
    i : INTEGER;
    value := BindingToText (s, binding);
  BEGIN

    IF Text.Length (value) = 0 THEN
        Msg.FatalError (NIL, "unrecognized " & name & ": ", "(empty)",
          FormatErrorAvailableEnumValues(min, max, map));
    END;

    TRY
      i := QVal.ToInt (s.machine, binding.value);
      IF (i < min) OR (i > max) THEN
        Msg.FatalError (NIL, "unrecognized " & name & ": ", value,
          FormatErrorAvailableEnumValues(min, max, map));
      END;
      RETURN i;
    EXCEPT Quake.Error =>
    END;

    FOR i := min TO max DO
      IF Text.Equal(value, map[i]) THEN
        RETURN i;
      END;
    END;

    Msg.FatalError (NIL, "unrecognized " & name & ": ", value,
      FormatErrorAvailableEnumValues(min, max, map));
    RETURN -1;

  END ConvertStringToEnum;

PROCEDURE ConvertBackendModeStringToEnum(s: State; binding: QValue.Binding):
  Target.M3BackendMode_t =
  BEGIN
    RETURN VAL(ConvertStringToEnum(s, "backend mode", binding,
                                   ORD(FIRST(Target.M3BackendMode_t)),
                                   ORD(LAST(Target.M3BackendMode_t)),
                                   Target.BackendModeStrings),
               Target.M3BackendMode_t);
  END ConvertBackendModeStringToEnum;

PROCEDURE ConvertNamingConventionStringToEnum(s: State;
                                              binding: QValue.Binding):
  OSKind =
  BEGIN
    RETURN VAL(ConvertStringToEnum(s, "naming convention", binding,
                                   ORD(FIRST(OSKind)),
                                   ORD(LAST(OSKind)),
                                   OSKindStrings),
               OSKind);
  END ConvertNamingConventionStringToEnum;

PROCEDURE SetupNamingConventions (mach : Quake.Machine) =
VAR s := NEW (State); (* TODO: Refactor to avoid creating garbage. *)
  BEGIN
    SetupNamingConventionsInternal (s, mach);
  END SetupNamingConventions;

PROCEDURE CompileUnits (main     : TEXT;
               READONLY units    : M3Unit.Set;
                        sys_libs : Arg.List;
                        info_kind: UK;
                        mach     : Quake.Machine): State =
  VAR
    s := NEW (State);  nm := M3Path.Parse (main);
    value : QValue.Binding;
  BEGIN
    DumpUnits (units);
    ETimer.ResetAll ();

    SetupNamingConventionsInternal (s, mach);
    s.result_name   := main;
    s.config_file   := MxConfig.FindFile ();
    s.sys_libs      := sys_libs;
    s.units         := units;
    s.link_base     := NIL;
    s.magic         := NEW (IntRefTbl.Default).init (100);
    s.ast_cache     := NEW (IntRefTbl.Default).init (100);
    s.include_path  := Arg.NewList ();
    s.include_path_empty := Arg.NewList ();
    s.pending_impls := NIL;
    s.main          := M3ID.Add ("Main");
    s.m3env         := NEW (Env);
    s.m3env.globals := s;

    s.target := GetConfigItem (s, "TARGET");

    value := GetDefn (s, "M3_BACKEND_MODE");
    IF value = NIL THEN
      value := GetDefn (s, "BACKEND_MODE");
    END;
    IF value = NIL THEN
        ConfigErr (s, "BACKEND_MODE or M3_BACKEND_MODE", "not defined");
    END;
    s.m3backend_mode := ConvertBackendModeStringToEnum(s, value);
    Target.BackendMode := Target.BackendMode;
    Target.BackendModeInitialized := TRUE;

    value := GetDefn (s, "TARGET_NAMING");
    IF value # NIL THEN
      WITH target_oskind = ConvertNamingConventionStringToEnum (s, value) DO
        s.target_oskind := target_oskind;
        M3Path.SetTargetOS (target_oskind);
      END;
    END;

    IF NOT Target.Init (s.target, GetConfigItem (s, "OS_TYPE", "POSIX"), s.m3backend_mode) THEN
      Msg.FatalError (NIL, "unrecognized target machine: TARGET = ", s.target);
    END;

    Target.Has_stack_walker := GetConfigBool(s, "M3_USE_STACK_WALKER",
                                             Target.Has_stack_walker);

    s.info_name   := M3Path.Join (NIL, nm.base, info_kind);
    s.m3backend   := GetConfigProc (s, "m3_backend", 4);
    s.m3llvm      := GetConfigProc (s, "m3llvm", 4); 
    s.llvmbackend := GetConfigProc (s, "llvm_backend", 5);
    s.llvmopt     := GetConfigProc (s, "llvm_opt", 2);
    s.c_compiler  := GetConfigProc (s, "compile_c", 5);
    s.assembler   := GetConfigProc (s, "assemble", 2);
    s.librarian   := GetConfigProc (s, "make_lib", 5);
    s.skip_lib    := GetConfigProc (s, "skip_lib", 2);
    s.linker      := GetConfigProc (s, "m3_link", 5);
    s.skip_linker := GetConfigProc (s, "skip_link", 2);

    s.compile_failed := FALSE;
    s.new_link_info  := FALSE;
    s.keep_files     := GetConfigBool (s, "M3_KEEP_FILES");
    s.bootstrap_mode := GetConfigBool (s, "M3_BOOTSTRAP");
    s.compile_once   := GetConfigBool (s, "M3_COMPILE_ONCE");
    s.has_loader     := GetConfigBool (s, "SYS_HAS_LOADER");
    s.skip_link      := GetConfigBool (s, "M3_SKIP_LINK");
    s.keep_resolved  := NOT GetConfigBool (s, "M3_SPLIT_LIBNAMES");
    s.gui            := GetConfigBool (s, "M3_WINDOWS_GUI");
    s.do_coverage    := GetConfigBool (s, "M3_COVERAGE");
    s.broken_linker  := GetConfigBool (s, "M3_NEED_STANDALONE_LINKS");
    s.lazy_init      := GetConfigBool (s, "M3_LAZY_MODULE_INIT", TRUE);
    s.Rpath_flag     := GetConfigText (s, "M3_SHARED_LIB_ARG");
    s.link_coverage  := GetConfigText (s, "M3_COVERAGE_LIB");
    s.m3_front_flags := GetConfigArray (s, "M3_FRONT_FLAGS");
    s.m3_options     := GetConfigArray (s, "M3_OPTIONS");

    IF GetDefn (s, "M3_PARALLEL_BACK") # NIL THEN
      s.parallelback := GetConfigInt (s, "M3_PARALLEL_BACK");
    ELSE
      s.parallelback := 1
    END;

    ETimer.Push (M3Timers.localobj);
      Utils.NoteLocalFileTimes ();
    ETimer.Pop ();
    BuildSearchPaths (s);
    Utils.InitWidechar (); 
    InhaleLinkInfo (s);
    BuildLibraryPool (s);

    current_state := s;
      CompileEverything (s, SortUnits (s));
      CleanUp ();
    current_state := NIL;

    RETURN s;
  END CompileUnits;

PROCEDURE BindingToText (s: State; bind: QValue.Binding; default: TEXT := NIL): TEXT =
  BEGIN
    IF bind = NIL THEN
      IF default # NIL THEN
        RETURN default;
      END;
      ConfigErr (s, s.machine.map.id2txt(bind.name), "not defined");
    END;
    TRY
      RETURN QVal.ToText (s.machine, bind.value);
    EXCEPT Quake.Error (msg) =>
      ConfigErr (s, s.machine.map.id2txt(bind.name), msg);
    END;
    RETURN NIL;
  END BindingToText;

PROCEDURE GetConfigItem (s: State;  symbol: TEXT; default: TEXT := NIL): TEXT =
  BEGIN
    RETURN BindingToText (s, GetDefn (s, symbol), default);
  END GetConfigItem;

PROCEDURE GetConfigProc (s: State;  symbol: TEXT;
                         n_args: INTEGER): ConfigProc =
  VAR x: ConfigProc;
  BEGIN
    x.name    := symbol;
    x.n_args  := n_args;
    x.binding := GetDefn (s, symbol);
    RETURN x;
  END GetConfigProc;

PROCEDURE GetConfigInt (s: State;  symbol: TEXT): INTEGER =
  VAR bind := GetDefn (s, symbol);
  BEGIN
    IF (bind = NIL) THEN ConfigErr (s, symbol, "not defined"); END;
    TRY
      RETURN QVal.ToInt (s.machine, bind.value);
    EXCEPT Quake.Error (msg) =>
      ConfigErr (s, symbol, msg);
    END;
    RETURN 0;
  END GetConfigInt;

PROCEDURE GetConfigBool (s: State;  symbol: TEXT; default := FALSE): BOOLEAN =
  VAR bind := GetDefn (s, symbol);
  BEGIN
    IF (bind = NIL) THEN RETURN default; END;
    TRY
      RETURN QVal.ToBool (s.machine, bind.value);
    EXCEPT Quake.Error (msg) =>
      ConfigErr (s, symbol, msg);
      RETURN FALSE;
    END;
  END GetConfigBool;

PROCEDURE GetConfigText (s: State;  symbol: TEXT): TEXT =
  VAR bind := GetDefn (s, symbol);
  BEGIN
    IF (bind = NIL) THEN RETURN NIL; END;
    TRY
      RETURN QVal.ToText (s.machine, bind.value);
    EXCEPT Quake.Error (msg) =>
      ConfigErr (s, symbol, msg);
      RETURN NIL;
    END;
  END GetConfigText;

PROCEDURE GetConfigArray (s: State;  symbol: TEXT): Arg.List =
  VAR
    bind := GetDefn (s, symbol);
    args := Arg.NewList ();
    arr: QVSeq.T;
  BEGIN
    IF (bind = NIL) THEN RETURN args; END;
    TRY
      arr := QVal.ToArray (s.machine, bind.value);
    EXCEPT Quake.Error (msg) =>
      ConfigErr (s, symbol, msg);
      RETURN args;
    END;

    FOR i := 0 TO arr.size() - 1 DO
      TRY
        Arg.Append (args, QVal.ToText (s.machine, arr.get (i)));
      EXCEPT Quake.Error (msg) =>
        ConfigErr (s, symbol, "array element not a text string: " & msg);
      END;
    END;

    RETURN args;
  END GetConfigArray;

PROCEDURE GetDefn (s: State;  symbol: TEXT): QValue.Binding =
  BEGIN
    RETURN s.machine.lookup (M3ID.Add (symbol));
  END GetDefn;

PROCEDURE ConfigErr (s: State;  symbol, msg: TEXT) =
  BEGIN
    Msg.FatalError (NIL, "Unable to use definition of \"" & symbol
                     & "\" from configuration file \"" & s.config_file
                     & "\": " & msg);
  END ConfigErr;

PROCEDURE DumpUnits (READONLY units: M3Unit.Set) =
  VAR cnt := 0;  u := units.head;
  BEGIN
    IF (Msg.level < Msg.Level.Debug) THEN RETURN END;
    Msg.Debug (Wr.EOL);
    Msg.Debug ("incoming units:", Wr.EOL);
    WHILE (u # NIL) DO
      Msg.Debug (" ", M3Unit.FileName (u));
      u := u.next;  INC (cnt);
    END;
    Msg.Debug (Wr.EOL);
    Msg.Debug ("  (list size = ", Fmt.Int (cnt), ")");
    Msg.Debug ("  (map size = ", Fmt.Int (units.map.size()), ")", Wr.EOL);
    Msg.Debug (Wr.EOL);
  END DumpUnits;

(*-------------------------------------------------------- C search paths ---*)

PROCEDURE BuildSearchPaths (s: State) =
  (* find the directories containing C source and include files and
     find the newest include file... *)
  VAR u := s.units.head;  seen := NEW (IntRefTbl.Default).init ();
  BEGIN
    WHILE (u # NIL) DO
      IF (u.kind = UK.C) OR (u.kind = UK.H) THEN
        IF NOT seen.put (M3ID.Add (u.loc.path), NIL) THEN
          Arg.Append (s.include_path, "-I" & u.loc.path);
        END;
      END;
      u := u.next;
    END;
  END BuildSearchPaths;

(*------------------------------------------------------- local link info ---*)

PROCEDURE InhaleLinkInfo (s: State) =
  VAR ux: Mx.UnitList;
  BEGIN
    ETimer.Push (M3Timers.inhale);
    Msg.Commands ("inhale ", s.info_name);
    ux := GetLinkUnits (s.info_name, NIL, imported := FALSE);
    IF (ux = NIL) THEN
      Msg.Debug ("no local link info", Wr.EOL);
    ELSE
      Msg.Debug ("adding units: ");
      WHILE (ux # NIL) DO
        EVAL MatchLocalUnit (s, ux.unit, FALSE);
        ux := ux.next;
      END;
      Msg.Debug (Wr.EOL);
    END;

    FindLocalExporters (s);

    s.new_link_info := FALSE;
    ETimer.Pop ();
  END InhaleLinkInfo;

PROCEDURE MatchLocalUnit (s: State;  uu: Mx.Unit;  imported: BOOLEAN): M3Unit.T =
  CONST KMap = ARRAY BOOLEAN OF UK { UK.M3, UK.I3 };
  VAR unit: M3Unit.T;
  BEGIN
    IF (uu = NIL) THEN RETURN NIL; END;

    unit := M3Unit.Get (s.units, uu.name, KMap [uu.interface]);
    IF (unit = NIL) THEN
      (* no source to match this unit (=> probably M3_BUILTIN.i3) *)
      IF (uu.interface AND Text.Equal (M3ID.ToText (uu.name), "M3_BUILTIN")) THEN
        unit := M3Unit.Get (s.units, M3ID.Add ("RTBuiltin"), UK.PGMX);
      END;
      IF (unit = NIL) THEN
        IF imported THEN
          unit := M3Unit.New (uu.name,  KMap[uu.interface],
                              M3Loc.New (M3Loc.noPkg, M3ID.Add ("."), "."),
                              hidden := TRUE, imported := imported);
          M3Unit.Add (s.units, unit);
          Msg.Verbose ("no source to match imported link unit ", UnitPath (unit));
        ELSE
          Msg.Verbose ("no source to match local link unit ", M3ID.ToText (uu.name));
          RETURN NIL;
        END;
      END;
    END;

    IF (unit.link_info # NIL) THEN
      BadFile ("duplicate link info", unit);
    END;
    unit.link_info := uu;
    IF (uu.file # NIL) AND (uu.file.name = NIL) THEN
      uu.file.name := UnitPath (unit);
    END;

    RETURN unit;
  END MatchLocalUnit;

PROCEDURE DumpLinkInfo (s: State) =
  VAR src := s.units.head;  units: Mx.UnitList := NIL;  wr: Wr.T;
  BEGIN
    IF NOT s.new_link_info THEN RETURN END;
    s.new_link_info := FALSE;  (* in case we die writing the info *)
    ETimer.Push (M3Timers.exhale);

    (* build a list of the local units *)
    WHILE (src # NIL) DO
      IF (NOT src.imported) AND (src.link_info # NIL) THEN
        units := NEW (Mx.UnitList, unit := src.link_info, next := units);
      END;
      src := src.next;
    END;

    (* and write them *)
    Msg.Commands ("exhale ", s.info_name);
    wr := Utils.OpenWriter (s.info_name, fatal := TRUE);
    MxOut.WriteUnits (units, wr);
    Utils.CloseWriter (wr, s.info_name);

    ETimer.Pop ();
  END DumpLinkInfo;

(*---------------------------------------------------------- library pool ---*)

PROCEDURE BuildLibraryPool (s: State) =
  VAR src := s.units.head;  ux: Mx.UnitList;
  BEGIN
    WHILE (src # NIL) DO
      IF (src.imported) AND (src.kind = UK.M3LIB) THEN
        (* Msg.Explain ("imported package ", M3ID.ToText(src.name)); *)
        ETimer.Push (M3Timers.inhale);
        Msg.Commands ("inhale ", UnitPath (src));
        ux := GetUnitLinkInfo (src, imported := TRUE);
        IF (ux = NIL) THEN
          Msg.Debug ("no link info for ", UnitPath (src), Wr.EOL);
        ELSE
          Msg.Debug ("adding units: ");
          WHILE (ux # NIL) DO
            AddLibraryUnit (s, ux.unit, src);
            ux := ux.next;
          END;
          Msg.Debug (Wr.EOL);
        END;
        ETimer.Pop ();
      END;
      src := src.next;
    END;
  END BuildLibraryPool;

PROCEDURE AddLibraryUnit (s: State;  uu: Mx.Unit;  lib: M3Unit.T) =
  CONST suffix = ARRAY BOOLEAN OF TEXT {".m3", ".i3"};
  VAR u: M3Unit.T;
  BEGIN
    Msg.Debug (" ", M3ID.ToText (uu.name), suffix[uu.interface]);
    u := MatchLocalUnit (s, uu, TRUE);
    IF (u # NIL) THEN
      u.library := lib;
      IF (NOT uu.interface) THEN
        WITH z = uu.exported_units DO
          FOR i := z.start TO z.start + z.cnt - 1 DO
            AddExportHook (s, uu.info [i], u);
          END;
        END;
      END;
    END;
  END AddLibraryUnit;


(*------------------------------------------- interface -> exporter links ---*)

PROCEDURE FindLocalExporters (s: State) =
  (* Build the initial set of export links for the local modules. *)
  VAR u: M3Unit.T;
  BEGIN
    (* scan the .M3 files for export information *)
    u := s.units.head;
    WHILE (u # NIL) DO
      IF (NOT u.imported) AND (u.kind = UK.M3) THEN
        IF (u.link_info # NIL) THEN
          (* we already know something about this guy *)
          WITH z = u.link_info.exported_units DO
            FOR i := z.start TO z.start + z.cnt - 1 DO
              AddExportHook (s, u.link_info.info[i], u);
            END;
          END;
        ELSE
          (* guess that he exports an interface with the same name! *)
          AddExportGuess (s, u);
        END;
      END;
      u := u.next;
    END;
  END FindLocalExporters;

PROCEDURE AddExportHook (s: State;  intf_name: M3ID.T;  impl: M3Unit.T) =
  VAR intf: M3Unit.T;
  BEGIN
    intf := M3Unit.Get (s.units, intf_name, UK.I3);
    IF (intf = NIL) THEN
      s.compile_failed := TRUE;
      Msg.Error (NIL, "missing interface: ", M3ID.ToText (intf_name), ".i3");
    ELSIF (intf.name = s.main) THEN
      (* Ignore "EXPORTS Main".  The linker is responsible for finding and
         explicitly initializing modules that claim to be the main program.  *)
    ELSIF (intf.imported # impl.imported) AND (intf.name # s.main) THEN
      s.compile_failed := TRUE;
      BadExport (intf, impl);
    ELSE
      intf.exporters := NEW (M3Unit.Exporter,
                             next     := intf.exporters,
                             name     := impl.name,
                             unit     := impl,
                             used     := FALSE,
                             verified := TRUE );
    END;
  END AddExportHook;

PROCEDURE AddExportGuess (s: State;  impl: M3Unit.T) =
  (* Guess that module "M" exports interface "M". *)
  VAR intf: M3Unit.T;
  BEGIN
    intf := M3Unit.Get (s.units, impl.name, UK.I3);
    IF (intf = NIL) THEN
      (* No such interface.  The guess must be no good. *)
    ELSIF (intf.name = s.main) THEN
      (* Ignore "EXPORTS Main".  The linker is responsible for finding and
         explicitly initializing modules that claim to be the main program.  *)
    ELSIF (intf.imported # impl.imported) THEN
      (* Nope.  We don't allow exports to cross library boundaries. *)
    ELSE
      intf.exporters := NEW (M3Unit.Exporter,
                             next     := intf.exporters,
                             name     := impl.name,
                             unit     := impl,
                             used     := FALSE,
                             verified := FALSE );
    END;
  END AddExportGuess;

PROCEDURE BadExport (intf, impl: M3Unit.T) =
  CONST X0 = ARRAY BOOLEAN OF TEXT { "local", "library" };
  BEGIN
    Msg.Error (NIL,
        X0[impl.imported] & " module (" & M3Unit.FileName (impl) & ")"
      & " cannot export "
      & X0[intf.imported] & " interface (" & M3Unit.FileName (intf) & ")");
  END BadExport;

PROCEDURE ResetExports (s: State;  u: M3Unit.T) =
  (* Forget any export information we may have for "u" because
     we're about the recompile it. *)
  VAR ex := u.exporters;
  BEGIN
    (* for interfaces, mark all the exporters "unused" *)
    WHILE (ex # NIL) DO  ex.used := FALSE;  ex := ex.next; END;

    (* for implementations, mark all the exporters "unverified" *)
    IF (u.kind =  UK.M3) AND (u.link_info # NIL) THEN
      WITH z = u.link_info.exported_units DO
        FOR i := z.start TO z.start + z.cnt - 1 DO
          ForgetExport (s, u.link_info.info[i], u);
        END;
      END;
    END;
  END ResetExports;

PROCEDURE ForgetExport (s: State;  intf_name: M3ID.T;  impl: M3Unit.T) =
  VAR intf: M3Unit.T;  ex: M3Unit.Exporter;
  BEGIN
    intf := M3Unit.Get (s.units, intf_name, UK.I3);
    IF (intf # NIL) THEN
      ex := intf.exporters;
      WHILE (ex # NIL) DO
        IF (ex.unit = impl) THEN ex.verified := FALSE; END;
        ex := ex.next;
      END;
    END;
  END ForgetExport;

PROCEDURE GetExporters (intf: M3Unit.T): M3Compiler.ImplList =
  VAR ex: M3Unit.Exporter;  xx: M3Compiler.ImplList := NIL;
  BEGIN
    ex := intf.exporters;
    WHILE (ex # NIL) DO
      xx := NEW (M3Compiler.ImplList, impl := ex.name, next := xx);
      ex.used := TRUE;
      ex := ex.next;
    END;
    RETURN xx;
  END GetExporters;

PROCEDURE MarkExportsUsed (intf: M3Unit.T) =
  (* Even though we're not going to compile "intf", pretend that
     we did using any verified exporters on its current export list. *)
  VAR ex := intf.exporters;
  BEGIN
    WHILE (ex # NIL) DO
      IF (ex.verified) THEN ex.used := TRUE; END;
      ex := ex.next;
    END;
  END MarkExportsUsed;

PROCEDURE UsedBogusExportList (intf: M3Unit.T): BOOLEAN =
  CONST U = ARRAY BOOLEAN OF TEXT { " not used,", " used," };
  CONST V = ARRAY BOOLEAN OF TEXT { " not verified", " verified" };
  VAR ex := intf.exporters;
  BEGIN
    WHILE (ex # NIL) DO
      IF (ex.used # ex.verified) THEN
        VerboseF ("new exporters ", intf);
        Msg.Verbose (" -> export ", M3ID.ToText (ex.name),
                     U[ex.used], V[ex.verified]);
        RETURN TRUE;
      END;
      ex := ex.next;
    END;
    RETURN FALSE;
  END UsedBogusExportList;

PROCEDURE NoteExporter (s: State;  intf_name: M3ID.T;  impl: M3Unit.T) =
  VAR intf: M3Unit.T;  ex: M3Unit.Exporter;
  BEGIN
    IF (impl = NIL) OR (impl.kind # UK.M3) THEN RETURN; END;
    intf := M3Unit.Get (s.units, intf_name, UK.I3);
    IF (intf = NIL) THEN
      s.compile_failed := TRUE;
      Msg.Error (NIL, "missing interface: ", M3ID.ToText (intf_name), ".i3");
    ELSIF (intf.name = s.main) THEN
      (* Ignore "EXPORTS Main".  The linker is responsible for finding and
         explicitly initializing modules that claim to be the main program.  *)
    ELSIF (intf.imported # impl.imported) AND (intf.name # s.main) THEN
      s.compile_failed := TRUE;
      BadExport (intf, impl);
    ELSE
      ex := intf.exporters;
      WHILE (ex # NIL) DO
        IF (ex.unit = impl) THEN ex.verified := TRUE; RETURN; END;
        ex := ex.next;
      END;
      (* no match was found => build a new exporter *)
      intf.exporters := NEW (M3Unit.Exporter,
                             next     := intf.exporters,
                             name     := impl.name,
                             unit     := impl,
                             used     := FALSE,
                             verified := TRUE );
    END;
  END NoteExporter;

(*----------------------------------------- determine the compilation order--*)

TYPE SourceList = REF ARRAY OF M3Unit.T;

CONST
  OrderMatters = ARRAY UK OF BOOLEAN {
    FALSE (*Unknown*),
     TRUE (*I3*),     TRUE (*IB*),   TRUE (*IC*),    TRUE (*IS*),    TRUE (*IO*),
     TRUE (*M3*),     TRUE (*MB*),   TRUE (*MC*),    TRUE (*MS*),    TRUE (*MO*),
    FALSE (*IG*),    FALSE (*MG*),
    FALSE (*C*),     FALSE (*H*),   FALSE (*B*),    FALSE (*S*),     FALSE (*O*),
    FALSE (*M3LIB*), FALSE (*LIB*),  TRUE (*LIBX*), FALSE (*PGM*),
     TRUE (*PGMX*),  FALSE (*TMPL*) };

TYPE
  SCCState = RECORD
    s          : State;
    next_class : INTEGER;
    tos        : INTEGER;
    stack      : SourceList;
    n_sched    : INTEGER;
    schedule   : SourceList;
  END;

CONST
  Ignore_class = 0;
  Phase0_class = 1;

PROCEDURE SortUnits (s: State): SourceList =
  VAR
    n_units: INTEGER;  u: M3Unit.T;
    units: SourceList;
    scc: SCCState;
  BEGIN
    (* first, count the local source units *)
    u := s.units.head;  n_units := 0;
    WHILE (u # NIL) DO
      IF NOT u.imported THEN INC (n_units); END;
      u := u.next;
    END;

    (* allocate space for the result and initialize it *)
    units := NEW (SourceList, n_units);
    scc.s          := s;
    scc.next_class := Phase0_class + 1;
    scc.tos        := 0;
    scc.stack      := NEW (SourceList, n_units + n_units);
    scc.n_sched    := 0;
    scc.schedule   := NEW (SourceList, n_units);

    u := s.units.head;  n_units := 0;
    WHILE (u # NIL) DO
      IF u.imported THEN
        u.class := Ignore_class;
      ELSIF OrderMatters [u.kind] THEN
        scc.schedule [scc.n_sched] := u;  INC (scc.n_sched);
        u.class := Ignore_class;
      ELSE
        units [n_units] := u;  INC (n_units);
        u.class    := Phase0_class;
        u.low_link := -1;
      END;
      u := u.next;
    END;

    (* find strongly-connected components in a bottom-up order
       and schedule them. *)
    FOR i := 0 TO n_units-1 DO VisitSCC (scc, Phase0_class, units[i]);  END;

    RETURN scc.schedule;
  END SortUnits;

PROCEDURE VisitSCC (VAR scc: SCCState;  cur_class: INTEGER;  u: M3Unit.T) =
  (* This procedure is adapted from the algorithm, SEARHC, given in
     "The Design and Analysis of Computer Algorithms" by Aho, Hopcroft,
     and Ullman for finding strongly connected components. *)
  VAR my_link := scc.tos;
  BEGIN
    IF (u.class # cur_class) THEN RETURN; END;

    (* push "u" on the stack *)
    u.low_link := my_link;
    scc.stack[scc.tos] := u;  INC (scc.tos);

    (* visit its imports *)
    IF u.link_info # NIL THEN
      VisitImports (scc, cur_class, u, u.link_info.imported_units,  UK.I3);
      VisitImports (scc, cur_class, u, u.link_info.exported_units,  UK.I3);
      IF (cur_class = Phase0_class) THEN
        VisitImports (scc, cur_class, u, u.link_info.used_interfaces, UK.I3);
        VisitImports (scc, cur_class, u, u.link_info.used_modules,    UK.M3);
      END;
    END;

    IF (u.low_link # my_link) THEN RETURN; END;
    (* Otherwise, "u" is the root of a strongly connected component *)
    (* => "pop" the component off the stack *)

    IF (cur_class = Phase0_class) THEN
      (* given an SCC using all the edges, refine that set
         using just the strict IMPORT/EXPORT edges. *)
      VAR class := scc.next_class; BEGIN
        INC (scc.next_class);
        (* reset the current set for the recursive visit *)
        FOR i := my_link TO scc.tos-1 DO
          u := scc.stack[i];
          u.class := class;
          u.low_link := -1;
        END;
        (* form the finer partition *)
        FOR i := my_link TO scc.tos-1 DO
          VisitSCC (scc, class, scc.stack[i]);
        END;
      END;
    ELSE
      (* SCCs found during the nested traversal can be scheduled *)
      FOR i := my_link TO scc.tos-1 DO
        scc.schedule[scc.n_sched] := scc.stack[i];
        INC (scc.n_sched);
      END;
    END;

    (* finally, pop the stack *)
    scc.tos := my_link;
  END VisitSCC;

PROCEDURE VisitImports (VAR scc: SCCState;  class: INTEGER;  u: M3Unit.T;
                        READONLY z: Mx.InfoList;  kind: UK) =
  VAR unit: M3Unit.T;
  BEGIN
    FOR i := z.start TO z.start + z.cnt - 1 DO
      unit := M3Unit.Get (scc.s.units, u.link_info.info[i], kind);
      IF (unit # NIL) THEN VisitProbe (scc, class, u, unit);  END;
    END;
  END VisitImports;

PROCEDURE VisitProbe (VAR scc: SCCState;  class: INTEGER;
                     source, dest: M3Unit.T) =
  BEGIN
    IF (dest.class # class) THEN
      (* ignore it *)
    ELSIF (dest.low_link < 0) THEN
      VisitSCC (scc, class, dest);
      source.low_link := MIN (source.low_link, dest.low_link);
    ELSE (* "dest" is already on the stack... *)
      source.low_link := MIN (source.low_link, dest.low_link);
    END;
  END VisitProbe;

(*------------------------------------------------------------ compilation --*)

(* parallel back-end build added by Mika Nystrom, February 2011

   We do the parallel build by commanding the QMachine to return its
   builds as "promises" rather than completing them on-the-fly.

   The promises are returned in s.machine.promises, of type QPromiseSeq.T

   Between each call to the compile step, we insert an innocuous
   marker of type QPromise.Empty.

   Within each compile (M3BACK ; ASM; DELETE TEMP FILES) we maintain
   sequencing.  We do this by running these tasks from a SeqClosure.
   However the different compiles are launched in parallel and landed
   in one join.

   Hence, we scan the promises and launch each subsequence of promises
   between QPromise.Empty markers as a sequence but unordered with
   respect to all other subsequences.

   The mechanism is currently only enabled in PushOneM3, case 3.
   This is the majority of normal operation cycles.

   Parallel speedups of about 2x on a 4-processor machine have been
   observed with n = 40.

*)

TYPE SeqClosure = Thread.Closure OBJECT seq : QPromiseSeq.T; OVERRIDES apply := SeqApply END;

PROCEDURE SeqApply(cl : SeqClosure) : REFANY =
  BEGIN
    TRY
      FOR i := 0 TO cl.seq.size()-1 DO
        EVAL cl.seq.get(i).fulfil()
      END;
    EXCEPT Quake.Error(x) =>
      Msg.FatalError (NIL, "quake error in parallel build ", x)
    | Thread.Alerted =>
      Msg.FatalError (NIL, "Thread.Alerted in parallel build ")
    END;
    RETURN NIL
  END SeqApply;

PROCEDURE ForceAllPromisesInParallel(promises : QPromiseSeq.T;
                                     parallelism : CARDINAL) =
  VAR
    curSeq  := NEW(QPromiseSeq.T).init();
    threads := NEW(RefSeq.T).init();
  BEGIN
    FOR i := 0 TO promises.size()-1 DO
      WITH p = promises.get(i) DO
        curSeq.addhi(p);
        IF i = promises.size()-1 OR ISTYPE(p,QPromise.Empty) THEN
          WITH cl = NEW(SeqClosure, seq := curSeq) DO
            threads.addhi (Thread.Fork(cl));
            
            IF threads.size() > parallelism-1 THEN 
              EVAL Thread.Join(threads.remlo()); 
            END;
            
            curSeq := NEW(QPromiseSeq.T).init()
          END
        END
      END
    END;
    WHILE threads.size() > 0 DO EVAL Thread.Join(threads.remlo()) END;
    
    EVAL promises.init(); (* empty promises *)
  END ForceAllPromisesInParallel;

PROCEDURE CompileEverything (s: State;  schedule: SourceList) =
  VAR u: M3Unit.T;
  BEGIN
    s.link_base := Mx.NewSet ();
    u := M3Unit.Get (s.units, M3ID.Add (Mx.BuiltinUnitName), UK.Unknown);
    IF (u # NIL) THEN CompileOne (s, u); END;

    (* compile all the sources using the initial schedule *)
    FOR i := 0 TO LAST (schedule^) DO
      s.delayBackend := s.parallelback > 1;
      TRY
        CompileOne (s, schedule[i]);
      FINALLY
        s.delayBackend := FALSE;
      END;

      s.machine.promises.addhi(NEW(QPromise.Empty));
    END;

    IF s.parallelback > 1 THEN
      Msg.Explain ("****  PARALLEL BACK-END BUILD, M3_PARALLEL_BACK = ", Fmt.Int(s.parallelback))
    END;

    ForceAllPromisesInParallel(s.machine.promises,s.parallelback);

    FlushPending (s);

    (* recompile any interfaces where we goofed on the exports *)
    u := s.units.head;
    WHILE (u # NIL) DO
      IF (NOT u.imported) AND (u.kind = UK.I3) AND UsedBogusExportList (u) THEN
        RecompileI3 (s, u);
      END;
      u := u.next;
    END;

    IF NOT s.compile_once THEN
      (* recompile those that could use the new opaque object information *)
      u := s.units.head;
      WHILE (u # NIL) DO
        IF (NOT u.imported) AND CouldBeImproved (s, u) THEN
          RecompileM3 (s, u);
        END;
        u := u.next;
      END;
    END;

    FlushPending (s);
  END CompileEverything;

PROCEDURE CompileOne (s: State;  u: M3Unit.T) =
  BEGIN
    IF (u.compiling) THEN RETURN; END;
    u.compiling := TRUE;
    VerboseF ("checking ", u);

    IF (u.kind = UK.LIBX) OR (u.kind = UK.PGMX) THEN
      FlushPending (s);
      CompileM3X (s, u);
    ELSIF (NOT u.imported) THEN
      FlushPending (s);
      FinalNameForUnit (s, u);
      IF IfDebug () THEN
        DebugF ("CompileOne FinalNameForUnit(", u, "):" & u.object);
      END;
      CASE u.kind OF
      | UK.I3, UK.M3       => CompileM3 (s, u);
      | UK.IB, UK.MB, UK.B => CompileLlc (s, u);
      | UK.IC, UK.MC       
        => IF s.m3backend_mode IN Target.BackendStAloneLlvmSet  
           THEN CompileM3llvm (s, u); 
           ELSE CompileM3cc (s, u); 
           END; 
      | UK.C               => CompileC (s, u); 
      | UK.IS, UK.MS, UK.S => CompileS (s, u);
      | UK.IO, UK.MO, UK.O => CompileO (s, u);
      | UK.H               => CompileH (s, u);
      | UK.IG, UK.MG       => (*skip*)
      | UK.M3LIB, UK.LIB   => (*skip*)
      | UK.PGMX, UK.LIBX   => (*skip*)
      | UK.TMPL            => (*skip*)
      ELSE Msg.Verbose ("unrecognized unit type: ", FName (u), Wr.EOL);
      END;
    ELSIF (u.link_info # NIL) THEN
      IF (u.library = NIL) THEN
        BadFile ("non-library unit without source", u);
      END;
      IF (u.link_info.interface) THEN
        Merge (s, u);
      ELSE (* defer this guy as long as possible *)
        s.pending_impls := NEW (M3Unit.TList, head := u, tail := s.pending_impls);
      END;
    ELSE
      BadFile ("missing source file", u);
    END;

    IF u.imported THEN
      (* might as well inhale the exporting units now... *)
      VAR ex := u.exporters; BEGIN
        WHILE (ex # NIL) DO
          CompileOne (s, ex.unit);
          ex := ex.next;
        END;
      END;
    END;
  END CompileOne;

PROCEDURE FlushPending (s: State) =
  VAR u: M3Unit.T;
  BEGIN
    WHILE (s.pending_impls # NIL) DO
      u := s.pending_impls.head;
      s.pending_impls := s.pending_impls.tail;
      Merge (s, u);
    END;
  END FlushPending;

PROCEDURE CompileM3X (s: State; u: M3Unit.T) =
(* PRE: u.kind IN {UK.PGMX, UK.LIBX} *) 
  VAR units: Mx.UnitList;
  BEGIN
    IF (u.link_info = NIL) THEN
      DebugF ("reading link info from ", u);
      units := GetUnitLinkInfo (u, imported := FALSE);
      IF (units = NIL) THEN BadFile ("missing link info", u); END;
      u.link_info := units.unit;
      <*ASSERT units.next = NIL*>
    END;
    Merge (s, u);
  END CompileM3X;

PROCEDURE CompileO (s: State; u: M3Unit.T) =
(* PRE: u.kind IN {UK.IO, UK.MO, UK.O} *) 
  BEGIN
    IF (u.kind # UK.O) THEN Merge (s, u) END;

    IF s.bootstrap_mode THEN
      Msg.Explain ("new object -> copying ", u.object);
      PullForBootstrap (u);
    END;
    EVAL Utils.NoteModification (u.object);
  END CompileO;

PROCEDURE CompileS (s: State; u: M3Unit.T) =
(* PRE: u.kind IN {UK.IS, UK.MS, UK.S} *) 
  BEGIN
    IF (u.kind # UK.S) THEN Merge (s, u) END;

    IF (u.object = NIL) OR Text.Equal (u.object, UnitPath (u)) THEN
      (* already done *)
      DebugF ("assemble ", u, " -> object = source");
      EVAL Utils.NoteModification (u.object);
    ELSIF NOT ObjectIsStale (u) THEN
      (* already done *)
    ELSIF s.bootstrap_mode THEN
      Msg.Explain ("new assembly -> copying ", u.object);
      PullForBootstrap (u);
      EVAL Utils.NoteModification (u.object);
    ELSIF (u.kind = UK.S) THEN
      EVAL RunCC (s, UnitPath (u), u.object, u.debug, u.optimize, s.include_path_empty);
      Utils.NoteNewFile (u.object);
    ELSE (* UK.IS or UK.MS *)
      EVAL RunAsm (s, UnitPath (u), u.object);
      Utils.NoteNewFile (u.object);
    END;
  END CompileS;

PROCEDURE CompileC (s: State; u: M3Unit.T) = 
(* PRE: u.kind = UK.C *) 
  BEGIN
    IF (u.object = NIL) OR Text.Equal (u.object, UnitPath (u)) THEN
      (* already done *)
      DebugF ("cc ", u, " -> object = source");
      EVAL Utils.NoteModification (u.object);
    ELSIF NOT ObjectIsStale (u) THEN
      (* already done *)
    ELSE
      IF s.bootstrap_mode THEN 
        PullForBootstrap (u);
(* Check: why not:
        EVAL Utils.NoteModification (u.object);
   instead of: 
        Utils.NoteNewFile (u.object);
*) 
      ELSE
        EVAL RunCC (s, UnitPath (u), u.object, u.debug, u.optimize, s.include_path);
        Utils.NoteNewFile (u.object);
      END; 
    END;
  END CompileC;

PROCEDURE CompileM3cc (s: State; u: M3Unit.T) = 
(* PRE: u.kind IN {UK.IC, UK.MC} *) 
  TYPE Mode_t = Target.M3BackendMode_t;
  VAR tmpS: TEXT := NIL;
      keep := s.keep_files;
      mode := s.m3backend_mode;
      boot := s.bootstrap_mode;
  BEGIN
    Merge (s, u);

    IF (u.object = NIL) OR Text.Equal (u.object, UnitPath (u)) THEN
      (* already done *)
      DebugF ("m3cc ", u, " -> object = source");
      EVAL Utils.NoteModification (u.object);
    ELSIF NOT ObjectIsStale (u) THEN
      (* already done *)
    ELSIF boot THEN 
      IF mode = Mode_t.ExternalAssembly THEN 
        EVAL RunM3Back (s, UnitPath (u), u.object, u.debug, u.optimize);
        Utils.NoteNewFile (u.object);
      ELSE 
        Msg.FatalError 
          (NIL, "Compiler mode " & Target.BackendModeStrings [ mode ] 
                & " cannot compile frontend output (.ic or .mc) files to assembly code.");
      END (*IF*) 
    ELSE 
      CASE mode OF
      | Mode_t.ExternalObject =>
        (* Currently, there are no such back ends. *) 
          EVAL RunM3Back (s, UnitPath (u), u.object, u.debug, u.optimize);
          Utils.NoteNewFile (u.object);
      | Mode_t.ExternalAssembly =>
          tmpS := AsmNameForUnit (u);
          IF NOT keep THEN Utils.NoteTempFile (tmpS) END;
          IF  RunM3Back (s, UnitPath (u), tmpS, u.debug, u.optimize)
              AND RunAsm (s, tmpS, u.object) THEN
          END;
          IF NOT keep THEN Utils.Remove (tmpS) END;
          Utils.NoteNewFile (u.object);
      ELSE
        Msg.FatalError 
          (NIL, "Compiler mode " & Target.BackendModeStrings [ mode ] 
                & " cannot compile frontend output (.ic or .mc) files");
      END (*CASE*);
    END;
  END CompileM3cc; 

PROCEDURE CompileM3llvm (s: State; u: M3Unit.T) = 
(* PRE: u.kind IN {UK.IC, UK.MC} *) 
  TYPE Mode_t = Target.M3BackendMode_t;
  VAR tmpS: TEXT := NIL;
      keep := s.keep_files;
      mode := s.m3backend_mode;
      boot := s.bootstrap_mode;
  BEGIN
    Merge (s, u);

    IF (u.object = NIL) OR Text.Equal (u.object, UnitPath (u)) THEN
      (* already done *)
      DebugF ("m3llvm ", u, " -> object = source");
(* TODO        ^This name should not be hard-coded here. *) 
      EVAL Utils.NoteModification (u.object);
    ELSIF NOT ObjectIsStale (u) THEN
      (* already done *)
    ELSIF boot THEN 
      IF mode = Mode_t.StAloneLlvmAsm THEN 
        EVAL RunM3Llvm (s, UnitPath (u), u.object, u.debug, u.optimize);
        Utils.NoteNewFile (u.object);
      ELSE 
        Msg.FatalError 
          (NIL, "Compiler mode " & Target.BackendModeStrings [ mode ] 
                & " cannot compile frontend output (.ic or .mc) files to assembly code.");
      END (*IF*) 
    ELSE 
      CASE mode OF
      | Mode_t.StAloneLlvmObj =>
        (* Currently, there are no such back ends. *) 
          EVAL RunM3Llvm (s, UnitPath (u), u.object, u.debug, u.optimize);
          Utils.NoteNewFile (u.object);
      | Mode_t.StAloneLlvmAsm =>
          tmpS := AsmNameForUnit (u);
          IF NOT keep THEN Utils.NoteTempFile (tmpS) END;
          IF RunM3Llvm (s, UnitPath (u), tmpS, u.debug, u.optimize)
             AND RunAsm (s, tmpS, u.object) THEN
          END;
          IF NOT keep THEN Utils.Remove (tmpS) END;
          Utils.NoteNewFile (u.object);
      ELSE
        Msg.FatalError 
          (NIL, "Compiler mode " & Target.BackendModeStrings [ mode ] 
                & " cannot compile frontend output (.ic or .mc) files");
      END (*CASE*);
    END;
  END CompileM3llvm; 

PROCEDURE CompileLlc (s: State; u: M3Unit.T) =
(* PRE: u.kind IN {UK.IB, UK.MB, UK.B} *) 
  TYPE Mode_t = Target.M3BackendMode_t;
  VAR tmpS: TEXT := NIL;
      mode := s.m3backend_mode;
  BEGIN
    IF u.kind # UK.B THEN Merge (s, u) END;

    IF u.object = NIL OR Text.Equal (u.object, UnitPath (u)) THEN
      (* already done *)
      DebugF ("llc ", u, " -> object = source");
      EVAL Utils.NoteModification (u.object);
    ELSIF NOT ObjectIsStale (u) THEN
      (* already done *)
    ELSE 
      CASE mode OF
      | Mode_t.ExtLlvmObj =>
        IF s.bootstrap_mode THEN 
          PullForBootstrap (u);
          EVAL Utils.NoteModification (u.object);
(* CHECK: This doesn't seem right: 
          Utils.NoteNewFile (u.object);
*) 
        ELSE
          EVAL RunLlcBack (s, UnitPath (u), u.object, u.debug, u.optimize, Asm:=FALSE);
          Utils.NoteNewFile (u.object);
        END; 
      | Mode_t.ExtLlvmAsm =>
        IF s.bootstrap_mode THEN 
          EVAL RunLlcBack (s, UnitPath (u), u.object, u.debug, u.optimize, Asm:=TRUE);
          Utils.NoteNewFile (u.object);
        ELSE 
          tmpS := AsmNameForUnit (u);
          IF NOT s.keep_files THEN Utils.NoteTempFile (tmpS) END;
          IF  RunLlcBack (s, UnitPath (u), tmpS, u.debug, u.optimize, Asm:=TRUE)
              AND RunAsm (s, tmpS, u.object) THEN
          END;
          IF NOT s.keep_files THEN Utils.Remove (tmpS) END;
          Utils.NoteNewFile (u.object);
        END (*IF*); 
      ELSE 
        Msg.FatalError 
          (NIL, "Compiler mode " & Target.BackendModeStrings [ mode ] 
                & " cannot compile llvm bitcode (.bc, .ib or .mb) files");
      END (*CASE*);
    END (*IF*);
  END CompileLlc;

PROCEDURE CompileH (s: State; u: M3Unit.T) =
(* PRE: u.kind = UK.H. *) 
  BEGIN
    IF NOT s.bootstrap_mode THEN
      (* already done *)
    ELSIF (u.object = NIL) OR Text.Equal (u.object, UnitPath (u)) THEN
      (* already done *)
      DebugF (".h  ", u, " -> object = source");
      EVAL Utils.NoteModification (u.object);
    ELSIF NOT ObjectIsStale (u) THEN
      (* already done *)
    ELSE
      PullForBootstrap (u);
      EVAL Utils.NoteModification (u.object);
    END;
  END CompileH;

PROCEDURE CompileM3 (s: State; u: M3Unit.T) =
(* PRE: u.kind IN {UK.I3, UK.M3} *) 
  BEGIN
    IF (u.library # NIL) THEN
      <*ASSERT u.link_info # NIL*>
      DebugF ("compile ", u, " -> from library");
      Merge (s, u);
    ELSIF (u.object = NIL) OR Text.Equal (u.object, UnitPath (u)) THEN
      (* already done *)
      EVAL Utils.NoteModification (u.object);
      MarkExportsUsed (u);
      DebugF ("compile ", u, " -> object = source");
      RETURN;
    ELSIF NOT M3isStale (s, u) THEN
      (* already done *)
      MarkExportsUsed (u);
      DebugF ("compile ", u, " -> not stale");
      RETURN;
    ELSE
      DebugF ("compile ", u);
      IF PushOneM3 (s, u) THEN
        Merge (s, u);
      END; 
    END;
  END CompileM3;

TYPE
  NotePromise = QPromise.T OBJECT
    nam : Pathname.T;
  OVERRIDES
    fulfil := FulfilNP;
  END;

  RemovePromise = QPromise.T OBJECT
    nam : Pathname.T;
  OVERRIDES
    fulfil := FulfilRP;
  END;

VAR utilsMu := NEW(MUTEX);  (* Utils.* fiddles with a table *)

PROCEDURE FulfilNP(np : NotePromise) : QPromise.ExitCode = 
  BEGIN LOCK utilsMu DO Utils.NoteTempFile(np.nam) END; RETURN 0 END FulfilNP;

PROCEDURE FulfilRP(rp : RemovePromise) : QPromise.ExitCode = 
  BEGIN LOCK utilsMu DO Utils.Remove(rp.nam) END; RETURN 0 END FulfilRP;

TYPE
  Temps_t = RECORD
    count := 0;
    names := ARRAY [0..3] OF TEXT { NIL, .. };
  END;

PROCEDURE Temps_Add (VAR temps: Temps_t; s: State; name: TEXT) =
  BEGIN
    IF name = NIL OR s.keep_files THEN RETURN END;
    <* ASSERT temps.count <= LAST(temps.names) *>
    temps.names[temps.count] := name;
    INC(temps.count);
 
    IF s.delayBackend THEN
      s.machine.promises.addhi(NEW(NotePromise, nam := name));
    ELSE
      Utils.NoteTempFile (name);
    END;
  END Temps_Add;

PROCEDURE Temps_Remove (VAR temps: Temps_t; s: State) =
  VAR count := temps.count;
    name: TEXT;
  BEGIN
    IF count = 0 THEN RETURN END;
    FOR i := 0 TO count - 1 DO
      name := temps.names[i];
      IF s.delayBackend THEN
        s.machine.promises.addhi(NEW(RemovePromise, nam := name));
      ELSE
        Utils.Remove (name);
      END;
    END;
  END Temps_Remove;

PROCEDURE PushOneM3 (s: State;  u: M3Unit.T): BOOLEAN =
(* PRE: u.kind IN {UK.I3, UK.M3} *) 
  TYPE Mode_t = Target.M3BackendMode_t;
  VAR
    temps := Temps_t { };

    (* Output file to be produced by cm3 executable -- "integrated" codegen. *) 
    cm3OutName: TEXT := NIL;

    (* Output file to be produced by any "external" code generator,
       but actually only LLVM-based ones in the current factoring
       of this messy functionality. *) 
    codeGenOutName: TEXT := NIL;

    cm3IRName: TEXT := NIL;
    llvmIRName: TEXT := NIL;
    llvmIROptName: TEXT := NIL;
    CCodeName: TEXT := NIL;
    asmName: TEXT := NIL;
    mode := s.m3backend_mode;
    boot := s.bootstrap_mode;
    need_merge := FALSE;
    DoRunM3cc : BOOLEAN := FALSE; 
    DoRunM3llvm : BOOLEAN := FALSE; 
    DoRunLlc : BOOLEAN := FALSE; 
    DoWriteAsm : BOOLEAN := FALSE; (* Pass asm option to code generator. *)  
    DoRunAsm := mode IN Target.BackendAsmSet; 
    DoRunC : BOOLEAN := FALSE; 
    ok := FALSE;
  BEGIN
    (* ASSERT mode # Mode_t.ExternalObject *)     (* mostly nonexistant, untested, but for m3cgcat *)
    <* ASSERT mode # Mode_t.IntegratedAssembly *> (* nonexistant, untested *)

(* The idea here is to push along the representation in one of a few sequences.
    u.object is where we stop.
    Where we stop and what sequence we take depends on the mode of the backend,
    and if we are bootstrapping. Bootstrapping runs later phases separately.
    Such as assembler or C compiler.
    "tmpC" is sometimes C (foo.m3.c, foo.i3.c), sometimes binary representation
    of m3cg for input to m3cc backend ("foo.mc" or "foo.ic").
    Options include:
      IntegratedObject        : m3 => o                            (NT/x86)
      IntegratedAssembly      : m3 => asm => o                     (no such backends)
      IntegratedAssembly boot : m3 => asm                          (no such backends)
      ExternalObject          : m3 => mc => o                      (no such backends)
      ExternalAssembly        : m3 => mc =>(using m3cc) asm => o
      ExternalAssembly boot   : m3 => mc =>(using m3cc) asm
      C                       : m3 => c => o
      C boot                  : m3 => c
      IntLlvmObj              : m3 =>(llvm-derived) o              (no such backends)
      IntLlvmAsm              : m3 =>(llvm-derived) asm => o       (no such backends)
      IntLlvmAsm boot         : m3 =>(llvm-derived) asm            (no such backends)
      ExtLlvmObj              : m3 => mb =>(using llc) o           (no such backends)
      ExtLlvmAsm              : m3 => mb =>(using llc) asm => o    (no such backends)
      ExtLlvmAsm boot         : m3 => mb =>(using llc) asm         (no such backends)
      StAloneLlvmObj          : m3 => mc =>(using m3llvm) mb =>(using llc) o 
      StAloneLlvmAsm          : m3 => mc =>(using m3llvm) mb =>(using llc) asm => o 
      StAloneLlvmAsm boot     : m3 => mc =>(using m3llvm) mb =>(using llc) asm  
*)
    u.link_info := NIL;
    ResetExports (s, u);

    (* Plan what to run, what files to produce and remove. *)
    (* Always run cm3, which reads UnitPath(u) and writes cm3OutName *)  
    CASE s.m3backend_mode OF 
    | Mode_t.IntegratedObject => 
        cm3OutName := u.object; 
        (* boot has no effect on this mode. *)          
    | Mode_t.IntegratedAssembly => (* not used *)
        asmName := AsmNameForUnit (u); 
        cm3OutName := asmName;
        DoRunAsm := NOT boot; 
    | Mode_t.ExternalObject =>
        codeGenOutName := u.object; 
        DoRunM3cc := TRUE; 
        (* boot has no effect on this mode. *)
    | Mode_t.ExternalAssembly => 
        asmName := AsmNameForUnit (u);
        codeGenOutName := asmName; 
        DoRunM3cc := TRUE; 
        DoRunAsm := NOT boot; 
    | Mode_t.C => 
        CCodeName := M3Unit.FileName (u) & ".c"; (* ?FUTURE: .cpp *)
        cm3OutName := CCodeName;
        DoRunC := NOT boot;
        (* C compiler takes care of assembling. *) 
    | Mode_t.IntLlvmObj =>  
        cm3OutName := u.object; 
        (* boot has no effect on this mode. *) 
    | Mode_t.IntLlvmAsm =>  
        asmName := AsmNameForUnit (u);
        cm3OutName := asmName; 
        DoRunAsm := NOT boot; 
    | Mode_t.ExtLlvmObj =>  
        llvmIRName := LlvmIRNameForUnit (u);
        llvmIROptName := LlvmIROptNameForUnit (u);
        cm3OutName := llvmIRName; 
        codeGenOutName := u.object; 
        DoRunLlc := TRUE; 
        (* boot has no effect on this mode. *) 
    | Mode_t.ExtLlvmAsm =>  
        llvmIRName := LlvmIRNameForUnit (u);
        llvmIROptName := LlvmIROptNameForUnit (u);
        cm3OutName := llvmIRName; 
        codeGenOutName := AsmNameForUnit (u);
        DoWriteAsm := TRUE; 
        DoRunLlc := TRUE; 
        DoRunAsm := NOT boot; 
        asmName := codeGenOutName; 
    | Mode_t.StAloneLlvmObj => 
        llvmIRName := LlvmIRNameForUnit (u);  
        llvmIROptName := LlvmIROptNameForUnit (u);
        DoRunM3llvm := TRUE; 
        codeGenOutName := u.object; 
        DoRunLlc := TRUE; 
        (* boot has no effect on this mode. *) 
    | Mode_t.StAloneLlvmAsm => 
        llvmIRName := LlvmIRNameForUnit (u);  
        llvmIROptName := LlvmIROptNameForUnit (u);
        DoRunM3llvm := TRUE; 
        codeGenOutName := AsmNameForUnit (u);  
        DoWriteAsm := TRUE; 
        DoRunLlc := TRUE; 
        DoRunAsm := NOT boot; 
        asmName := codeGenOutName; 
    END;
    
    (* External code generators always consume cm3IR. *)
    IF codeGenOutName # NIL THEN
      cm3IRName := Cm3IRNameForUnit (u);
      cm3OutName := cm3IRName; 
    END;
    
    (* IR is currently always temporary.
       The generalization is -keep-x or -stop-at-x.
       "Boot" is a builtin -stop-at-keep-c-or-assembly
       where the rest of the tools are on the target instead
       of the host, i.e. in the presence of native assembler/C compiler/linker
       and the absence of cross assembler/C compiler/linker.
    *)
    Temps_Add (temps, s, cm3IRName);
    Temps_Add (temps, s, llvmIRName);
    Temps_Add (temps, s, llvmIROptName); 

    (* C, assembly are always temporary except for "boot", per above. *)
    IF NOT boot THEN
      Temps_Add (temps, s, asmName);
      Temps_Add (temps, s, CCodeName);
    END;

    ok := RunM3Front (s, u, cm3OutName);
    IF NOT ok THEN 
      Msg.Error (NIL, "m3front failed compiling: ", UnitPath (u));
    ELSE (* Front succeeded. *) 
      IF s.delayBackend THEN (* parallel/delayed version of back-end code *)
        s.machine.record(TRUE);
      END;
      TRY
        IF ok AND DoRunM3cc THEN
          ok := RunM3Back (s, cm3IRName, codeGenOutName, u.debug, u.optimize);
        END; 
        IF ok AND DoRunM3llvm THEN
          ok := RunM3Llvm (s, cm3IRName, llvmIRName, u.debug, u.optimize);
        END; 
        IF ok AND DoRunLlc THEN
          IF u.optimize THEN
            EVAL RunLlvmOpt(s, llvmIRName, llvmIROptName);
          END;
          ok := RunLlcBack 
                 (s, llvmIRName, codeGenOutName, u.debug, u.optimize, 
                  Asm := DoWriteAsm);
        END; 
        IF ok AND DoRunC THEN 
          ok := RunCC (s, CCodeName, u.object, TRUE, FALSE, s.include_path_empty);
        END;
        IF ok AND DoRunAsm THEN 
          ok := RunAsm (s, asmName, u.object);
        END;
      FINALLY
        IF s.delayBackend THEN
          s.machine.record(FALSE);
        END;
        need_merge := TRUE;
      END;
    END;

    Temps_Remove (temps, s);

    Utils.NoteNewFile (u.object);

    RETURN need_merge;
  END PushOneM3;

PROCEDURE RecompileI3 (s: State;  u: M3Unit.T) =
  BEGIN
    ExplainF ("new exporters -> recompiling ", u);
    IF PushOneM3 (s, u) THEN Remerge (s, u) END;
  END RecompileI3;

PROCEDURE RecompileM3 (s: State;  u: M3Unit.T) =
  BEGIN
    IF PushOneM3 (s, u) THEN Remerge (s, u) END;
  END RecompileM3;

PROCEDURE CouldBeImproved (s: State;  u: M3Unit.T): BOOLEAN =
  VAR ref: REFANY;
  BEGIN
    IF (u.library # NIL) OR (u.link_info = NIL) THEN
      (* can't improve the code we didn't compile... *)
      RETURN FALSE;
    ELSIF (u.kind # UK.M3) THEN
      (* can only improve executable Modula-3... *)
      RETURN FALSE;
    ELSIF (u.object = NIL) OR Text.Equal (u.object, UnitPath (u)) THEN
      (* can't improve the code we didn't compile... *)
      RETURN FALSE;
    ELSE
      (* check for a wish that could be fulfilled. *)
      WITH z = u.link_info.wishes DO
        FOR i := z.start TO z.start + z.cnt - 1 DO
          IF s.magic.get (u.link_info.info[i], ref) THEN
            ExplainF ("new opaque info -> recompiling ", u);
            RETURN TRUE;
          END;
        END;
      END;
      RETURN FALSE;
    END;
  END CouldBeImproved;

PROCEDURE ObjectIsStale (u: M3Unit.T): BOOLEAN =
  VAR objTime: INTEGER;
  BEGIN
    ETimer.Push (M3Timers.staleobj);

    (* check if the source is newer than the object *)
    objTime := Utils.LocalModTime (u.object);

    (*********************************************************
      ---- too many people thought that "missing object" was
           an error, so we just won't distinguish a missing
           object from an old one.  I guess "new source" is
           a cheery, more positive message...  -----
    *********************************************************)

    IF (objTime = Utils.NO_TIME)
      OR (objTime < Utils.ModificationTime (UnitPath (u))) THEN
      IF (u.kind = UK.I3) OR (u.kind = UK.M3)
        THEN u.stale_src := TRUE; (* defer the message for a moment *)
        ELSE ExplainF ("new source -> compiling ", u);
      END;
      ETimer.Pop ();
      RETURN TRUE;
    END;

    (* object exists and is newer than the source... *)
    ETimer.Pop ();
    RETURN FALSE;
  END ObjectIsStale;

PROCEDURE M3isStale (s: State;  u: M3Unit.T): BOOLEAN =
  BEGIN
    IF ObjectIsStale (u) THEN RETURN TRUE END;

    ETimer.Push (M3Timers.stalem3);

    IF (u.link_info = NIL) THEN
      u.missing_info := TRUE; (* defer the message for a moment *)
      ETimer.Pop ();
      RETURN TRUE;
    END;

    (* check my imports first *)
    CheckImports (s, u.link_info);

    (* check for new generics *)
    IF NewGenerics (s, u) THEN
      ExplainF ("new generic source -> compiling ", u);
      ETimer.Pop ();
      RETURN TRUE;
    END;

    (* finally, add my self to the set *)
    DebugF ("merging initial link info for ", u);
    IF NOT MergeUnit (s, u.link_info, optional := TRUE) THEN
      ExplainF ("stale imports -> compiling ", u);
      ETimer.Pop ();
      RETURN TRUE;
    END;

    DebugF ("ok ", u);
    ETimer.Pop ();
    RETURN FALSE;
  END M3isStale;

PROCEDURE Merge (s: State;  u: M3Unit.T) =
  BEGIN
    ETimer.Push (M3Timers.merge);

    IF (u.link_info = NIL) THEN BadFile ("missing link info", u); END;

    CheckImports (s, u.link_info);

    DebugF ("merging final link info for ", u);
    EVAL MergeUnit (s, u.link_info, optional := FALSE);

    ETimer.Pop ();
  END Merge;

PROCEDURE Remerge (s: State;  u: M3Unit.T) =
  BEGIN
    ETimer.Push (M3Timers.merge);

    IF (u.link_info = NIL) THEN BadFile ("missing link info", u); END;

    DebugF ("adding new magic for ", u);
    AddMagic (s, u.link_info);

    ETimer.Pop ();
  END Remerge;

PROCEDURE CheckImports (s: State;  u: Mx.Unit) =
  BEGIN
    CheckImp (s, u, u.imported_units,  UK.I3);
    CheckImp (s, u, u.exported_units,  UK.I3);
    (****  not needed with the new sort order...
    CheckImp (s, u, u.used_interfaces, UK.I3);
    CheckImp (s, u, u.used_modules,    UK.M3);
    ****)
  END CheckImports;

PROCEDURE CheckImp (s: State;  u: Mx.Unit;  READONLY z: Mx.InfoList;  kind: UK) =
  VAR unit: M3Unit.T;
  BEGIN
    FOR i := z.start TO z.start + z.cnt - 1 DO
      unit := M3Unit.Get (s.units, u.info[i], kind);
      IF (unit # NIL) THEN CompileOne (s, unit) END;
    END;
  END CheckImp;

PROCEDURE NewGenerics (s: State;  u: M3Unit.T): BOOLEAN =
  VAR
    uu := u.link_info;
    obj_time: INTEGER;
    generic_time: INTEGER;
    nm: TEXT;
  BEGIN
    IF (uu.imported_generics.cnt <= 0) THEN RETURN FALSE END;
    obj_time := Utils.LocalModTime (u.object);

    WITH z = uu.imported_generics DO
      FOR i := z.start TO z.start + z.cnt - 1 DO
        nm := M3ID.ToText (uu.info[i]);
        generic_time := FindGeneric (s, nm, uu.interface);
        IF (obj_time < generic_time) THEN RETURN TRUE END;
      END;
    END;
    RETURN FALSE;
  END NewGenerics;

PROCEDURE FindGeneric (s: State;  name: TEXT;  interface: BOOLEAN): INTEGER =
  CONST Map = ARRAY BOOLEAN OF UK { UK.MG, UK.IG };
  VAR
    kind := Map [interface];
    unit := M3Unit.Get (s.units, M3ID.Add (name), kind);
  BEGIN
    IF (unit = NIL) THEN
      Msg.FatalError (NIL, "cannot find generic source: ",
                      M3Path.Join (NIL, name, kind));
      RETURN Utils.NO_TIME;
    ELSE
      RETURN Utils.ModificationTime (UnitPath (unit));
    END;
  END FindGeneric;

(*------------------------------------------------------------ first pass ---*)

TYPE
  InfoList = RECORD
    cnt : INTEGER    := 0;
    info: Mx.InfoVec := NIL;
  END;

TYPE
  Env = M3Front.Environment OBJECT
    globals           : State;
    source_unit       : M3Unit.T;
    source            : TEXT;
    object            : TEXT;
    output            : Wr.T;
    cg                : M3CG.T;
    unit              : Mx.Unit;
    imports           : IntSet.T;
    exports           : IntSet.T;
    used_intfs        : IntSet.T;
    used_impls        : IntSet.T;
    wish_map          : IntSet.T;
    used_magic        : IntSet.T;
    exported_units    : InfoList; (* of M3ID.Ts *)
    imported_units    : InfoList; (* of M3ID.Ts *)
    imported_generics : InfoList; (* of M3ID.Ts *)
    used_interfaces   : InfoList; (* of M3ID.Ts *)
    used_modules      : InfoList; (* of M3ID.Ts *)
    import_def_syms   : InfoList; (* of MxVS.Ts *)
    import_use_syms   : InfoList; (* of MxVS.Ts *)
    export_def_syms   : InfoList; (* of MxVS.Ts *)
    export_use_syms   : InfoList; (* of MxVS.Ts *)
    imported_types    : InfoList; (* of TypeNames *)
    exported_types    : InfoList; (* of TypeNames *)
    wishes            : InfoList; (* of TypeNames *)
  OVERRIDES
    report_error       := Pass0_Error;
    find_source        := Pass0_Open;
    note_unit          := Pass0_NoteUnit;
    note_comment       := Pass0_Comment;
    note_interface_use := Pass0_NoteInterface;
    note_generic_use   := Pass0_NoteGeneric;
    note_version_stamp := Pass0_NoteVS;
    note_opaque        := Pass0_NoteOpaque;
    note_revelation    := Pass0_NoteRevelation;
    note_opaque_magic  := Pass0_AddMagic;
    find_opaque_magic  := Pass0_FindMagic;
    note_ast           := Pass0_NoteAST;
    find_ast           := Pass0_FindAST;
    note_type          := Pass0_NoteType;
    init_code_generator:= Pass0_InitCodeGenerator;
    note_webinfo       := Pass0_NoteWebInfo;
    get_implementations:= Pass0_GetImplementations;
  END;

PROCEDURE ResetEnv (s: State;  u: M3Unit.T;  source, object: TEXT) =
  VAR env := s.m3env;
  BEGIN
    env.globals               := s;
    env.source_unit           := u;
    env.source                := source;
    env.object                := object;
    env.output                := NIL;
    env.cg                    := NIL;
    env.unit                  := NIL;
    env.imports               := NIL;
    env.exports               := NIL;
    env.used_intfs            := NIL;
    env.used_impls            := NIL;
    env.wish_map              := NIL;
    env.used_magic            := NIL;
    env.exported_units.cnt    := 0;
    env.imported_units.cnt    := 0;
    env.imported_generics.cnt := 0;
    env.used_interfaces.cnt   := 0;
    env.used_modules.cnt      := 0;
    env.import_def_syms.cnt   := 0;
    env.import_use_syms.cnt   := 0;
    env.export_def_syms.cnt   := 0;
    env.export_use_syms.cnt   := 0;
    env.imported_types.cnt    := 0;
    env.exported_types.cnt    := 0;
    env.wishes.cnt            := 0;
  END ResetEnv;

PROCEDURE RunM3Front (s: State;  u: M3Unit.T;  object: TEXT)
  : BOOLEAN (* Success. *) =
  VAR
    ok      : BOOLEAN;
    source  : M3Front.SourceFile;
    options : REF ARRAY OF TEXT;
    input   : File.T      := NIL;
  BEGIN
    ETimer.Push (M3Timers.pass_0);

    VAR xx := Arg.NewList ();  BEGIN
      Arg.AppendL (xx, s.m3_front_flags);
      Arg.AppendL (xx, s.m3_options);
      options := Arg.Flatten (xx, NIL);
    END;

    (* open the input file *)
    source.name := UnitPath (u);
    
    IF IfDebug () THEN
      DoDebug ("RunM3Front source:" & NilText(source.name) & " object:" & NilText(object) & " ");
    END; 
 
    input  := Utils.OpenReader (source.name, fatal := FALSE);
    ok := (input # NIL);
    IF NOT ok THEN
      Msg.Error (NIL, "open failed on: ", source.name);
    END;
    source.contents := input;

    IF (ok) AND ((u.stale_src) OR (u.missing_info)) THEN
      Pass0_CheckImports (s, source);
      FlushPending (s);
      (* finally, generate the deferred message *)
      IF (u.missing_info) THEN
        u.missing_info := FALSE;
        ExplainF ("missing version stamps -> compiling ", u);
      ELSE
        u.stale_src := FALSE;
        ExplainF ("new source -> compiling ", u);
      END;
    END;

    (* do the compilation *)
    IF (ok) THEN
      ResetEnv (s, u, UnitPath (u), object);
      Pass0_Trace (UnitPath (u), s.m3_front_flags, s.m3_options);
      ok := M3Front.Compile (source, s.m3env, options^);
    END;
    IF (ok) AND (s.m3env.unit # NIL) THEN
      s.new_link_info := TRUE;
      u.link_info := FinishUnitInfo (s.m3env);
    ELSE
      IF (u.link_info # NIL) THEN s.new_link_info := TRUE; END;
      u.link_info := NIL;
    END;

    (* dump the generated code *)
    IF (s.m3env.cg # NIL) THEN M3Backend.Close (s.m3env.cg); END;

    (* flush and close the files *)
    Utils.CloseReader (input, UnitPath (u));
    Utils.CloseWriter (s.m3env.output, s.m3env.object);
    ResetEnv (s, NIL, NIL, NIL);

    IF NOT ok THEN
      s.compile_failed := TRUE;
      IF NOT s.keep_files THEN Utils.Remove (object); END;
    END;

    ETimer.Pop ();
    RETURN ok;
  END RunM3Front;

PROCEDURE Pass0_InitCodeGenerator (env: Env): M3CG.T =
  BEGIN
    env.cg     := NIL;
    env.output := Utils.OpenWriter (env.object, fatal := FALSE);
    IF (env.output # NIL) THEN
      env.cg := M3Backend.Open (env.output, env.object, env.globals.m3backend_mode);
    END;
    RETURN env.cg;
  END Pass0_InitCodeGenerator;

PROCEDURE Pass0_CheckImports (s: State;  VAR source: M3Front.SourceFile) =
  VAR ids: M3Front.IDList;  unit: M3Unit.T;
  BEGIN
    ResetEnv (s, NIL, source.name, NIL);
    ids := M3Front.ParseImports (source, s.m3env);
    WHILE (ids # NIL) DO
      unit := M3Unit.Get (s.units, ids.interface, UK.I3);
      IF (unit # NIL) THEN CompileOne (s, unit) END;
      ids := ids.next;
    END;
    Utils.RewindReader (source.contents, source.name);
  END Pass0_CheckImports;

PROCEDURE Pass0_Trace (source: TEXT;  config, user: Arg.List) =
  VAR x: Arg.T;
  BEGIN
    IF (Msg.level < Msg.Level.Commands) THEN RETURN END;

    Msg.Out ("m3front ", source);

    IF (Msg.level >= Msg.Level.Verbose) THEN
      x := config.head;
      WHILE (x # NIL) DO
        Msg.Out (" ", x.arg);
        x := x.next;
      END;
    END;

    x := user.head;
    WHILE (x # NIL) DO
      Msg.Out (" ", x.arg);
      x := x.next;
    END;
    Msg.Out (Wr.EOL);
  END Pass0_Trace;

PROCEDURE Pass0_Error (<*UNUSED*>env: Env;  file: TEXT;  line: INTEGER;
                       msg: TEXT) =
  BEGIN
    IF (file # NIL)
      THEN Msg.Out ("\"", file, "\", line ", Fmt.Int (line), ": ", msg,Wr.EOL);
      ELSE Msg.Out (msg, Wr.EOL);
    END;
  END Pass0_Error;

PROCEDURE Pass0_Open (env: Env;  name: M3ID.T;
                      interface, generic: BOOLEAN): M3Front.SourceFile =
  TYPE  GMap = ARRAY BOOLEAN OF UK;
  CONST KMap = ARRAY BOOLEAN OF GMap{ GMap{ UK.M3, UK.MG }, GMap{ UK.I3, UK.IG }};
  VAR
    file : M3Compiler.SourceFile;
    kind := KMap [interface][generic];
    unit := M3Unit.Get (env.globals.units, name, kind);
  BEGIN
    IF (unit # NIL) THEN
      file.name := UnitPath (unit);
      file.contents := Utils.OpenReader (file.name, fatal := TRUE);
    ELSE
      file.name := M3Path.Join (NIL, M3ID.ToText (name), kind);
      file.contents := NIL;
    END;
    RETURN file;
  END Pass0_Open;

PROCEDURE Pass0_NoteUnit (env: Env;  name: M3ID.T;  interface: BOOLEAN) =
  BEGIN
    env.unit := NEW (Mx.Unit, name := name, interface := interface,
                     file := NEW (Mx.File, name := env.source));
    env.imports    := NEW (IntSet.Default).init ();
    env.exports    := NEW (IntSet.Default).init ();
    env.used_intfs := NEW (IntSet.Default).init ();
    env.used_impls := NEW (IntSet.Default).init ();
    env.wish_map   := NEW (IntSet.Default).init ();
    env.used_magic := NEW (IntSet.Default).init ();
  END Pass0_NoteUnit;

PROCEDURE Pass0_NoteInterface (env: Env;  name: M3ID.T;  imported: BOOLEAN) =
  BEGIN
    EVAL env.used_intfs.put (name, 0);
    IF imported THEN
      IF NOT env.imports.put (name, 0) THEN
        AddInfo (env.imported_units, name);
      END;
    ELSE
      IF NOT env.exports.put (name, 0) THEN
        AddInfo (env.exported_units, name);
        NoteExporter (env.globals, name, env.source_unit);
      END;
    END;
  END Pass0_NoteInterface;

PROCEDURE Pass0_NoteGeneric (env: Env;  name: M3ID.T) =
  BEGIN
    AddInfo (env.imported_generics, name);
  END Pass0_NoteGeneric;

PROCEDURE Pass0_NoteVS (env: Env;  intf, name: M3ID.T;
                        READONLY fp: Fingerprint.T;
                        imported, implemented: BOOLEAN) =
  VAR info: MxVS.Info;   vs: MxVS.T;
  BEGIN
    info.source := intf;
    info.symbol := name;
    info.stamp  := fp;
    vs := MxVS.Put (info);
    Pass0_NoteInterface (env, intf, imported);
    IF (imported) THEN
      IF (implemented)
        THEN AddInfo (env.import_def_syms, vs);
        ELSE AddInfo (env.import_use_syms, vs);
      END;
    ELSE (*exported*)
      IF (implemented)
        THEN AddInfo (env.export_def_syms, vs);
        ELSE AddInfo (env.export_use_syms, vs);
      END;
    END;
  END Pass0_NoteVS;

PROCEDURE Pass0_NoteRevelation (env: Env;  source: M3ID.T;  interface: BOOLEAN;
                          lhs, rhs: INTEGER;  full, imported: BOOLEAN) =
  VAR r := NEW (Mx.Revelation, source := source, lhs := lhs, rhs := rhs,
                partial := NOT full, export := NOT imported);
  BEGIN
    Pass0_AddUnit (env, source, interface);
    r.next := env.unit.revelations;
    env.unit.revelations := r;
  END Pass0_NoteRevelation;

PROCEDURE Pass0_Comment (<*UNUSED*> env: Env;  t: TEXT) =
  BEGIN
    Msg.Verbose (t);
  END Pass0_Comment;

PROCEDURE Pass0_NoteOpaque (env: Env;  type, super_type: INTEGER;
                            Name: M3ID.T) =
  BEGIN
    env.unit.opaques := NEW (Mx.OpaqueType, type := type,
                             super_type := super_type,
                             TypeName := Name,
                             next := env.unit.opaques);
  END Pass0_NoteOpaque;

PROCEDURE Pass0_AddUnit (env: Env;  nm: M3ID.T;  interface: BOOLEAN) =
  BEGIN
    IF (interface) THEN
      IF NOT env.used_intfs.put (nm, 0) THEN
        AddInfo (env.used_interfaces, nm);
      END;
    ELSE (*module*)
      IF NOT env.used_impls.put (nm, 0) THEN
        AddInfo (env.used_modules, nm);
      END;
    END;
  END Pass0_AddUnit;

PROCEDURE Pass0_AddMagic (env         : Env;
                          type        : INTEGER;
                          super_type  : INTEGER;
                          data_size   : INTEGER;
                          data_align  : INTEGER;
                          method_size : INTEGER) =
  VAR obj := Pass0_NoteObject (env, env.unit.name, env.unit.interface,
                               FALSE, type, super_type, data_size,
                               data_align, method_size);
  BEGIN
    EVAL env.used_magic.put (type, 0);
    EVAL env.globals.magic.put (type, obj);
  END Pass0_AddMagic;

PROCEDURE Pass0_FindMagic (env         : Env;
                           type        : INTEGER;
                VAR(*OUT*) super_type  : INTEGER;
                VAR(*OUT*) data_size   : INTEGER;
                VAR(*OUT*) data_align  : INTEGER;
                VAR(*OUT*) method_size : INTEGER): BOOLEAN =
  VAR ref: REFANY;  obj: Mx.ObjectType;
  BEGIN
    IF NOT env.globals.magic.get (type, ref) THEN
      IF NOT env.wish_map.put (type, 0) THEN
        AddInfo (env.wishes, type);
      END;
      RETURN FALSE;
    END;
    obj := ref;

    IF NOT env.used_magic.put (type, 0) THEN
      EVAL Pass0_NoteObject (env, obj.source, NOT obj.from_module, TRUE,
                             obj.type, obj.super_type, obj.data_size,
                             obj.data_align, obj.method_size);
    END;

    super_type  := obj.super_type;
    data_size   := obj.data_size;
    data_align  := obj.data_align;
    method_size := obj.method_size;
    RETURN TRUE;
  END Pass0_FindMagic;

PROCEDURE Pass0_NoteObject (env: Env;  source: M3ID.T;
                      interface, imported: BOOLEAN;
                      type, super_type: INTEGER;
                      data_size, data_align, method_size: INTEGER
                      ): Mx.ObjectType =
  VAR obj := NEW (Mx.ObjectType, source := source, type := type,
                  super_type := super_type, data_size := data_size,
                  data_align := data_align, method_size := method_size,
                  export := NOT imported, from_module := NOT interface);
  BEGIN
    IF (NOT imported) THEN
      obj.next := env.unit.exported_objects;
      env.unit.exported_objects := obj;
    ELSE
      Pass0_AddUnit (env, source, interface);
      obj.next := env.unit.imported_objects;
      env.unit.imported_objects := obj;
    END;
    RETURN obj;
  END Pass0_NoteObject;

PROCEDURE ExpandInfo (VAR x: InfoList) =
  VAR n := NUMBER (x.info^);  new := NEW (Mx.InfoVec, n + n);
  BEGIN
    SUBARRAY (new^, 0, n) := x.info^;
    x.info := new;
  END ExpandInfo;

PROCEDURE Pass0_NoteAST (env: Env;  intf: M3ID.T;  ast: REFANY) =
  BEGIN
    EVAL env.globals.ast_cache.put (intf, ast);
  END Pass0_NoteAST;

PROCEDURE Pass0_FindAST (env: Env;  intf: M3ID.T): REFANY =
  VAR ref: REFANY;
  BEGIN
    IF env.globals.ast_cache.get (intf, ref)
      THEN RETURN ref;
      ELSE RETURN NIL;
    END;
  END Pass0_FindAST;

PROCEDURE Pass0_NoteType (env: Env;  type: INTEGER;  imported: BOOLEAN) =
  BEGIN
    IF (imported)
      THEN AddInfo (env.imported_types, type);
      ELSE AddInfo (env.exported_types, type);
    END;
  END Pass0_NoteType;

PROCEDURE AddInfo (VAR x: InfoList;  i: INTEGER) =
  BEGIN
    IF (x.info = NIL) THEN
      x.info := NEW (Mx.InfoVec, 40);
    ELSIF (x.cnt >= NUMBER (x.info^)) THEN
      ExpandInfo (x);
    END;
    x.info [x.cnt] := i;
    INC (x.cnt);
  END AddInfo;

PROCEDURE FinishUnitInfo (env: Env): Mx.Unit =
  VAR n: INTEGER;  info: Mx.InfoVec;   u := env.unit;
  BEGIN
    n := env.exported_units.cnt + env.imported_units.cnt
       + env.imported_generics.cnt + env.used_interfaces.cnt
       + env.used_modules.cnt + env.import_def_syms.cnt
       + env.import_use_syms.cnt + env.export_def_syms.cnt
       + env.export_use_syms.cnt + env.imported_types.cnt
       + env.exported_types.cnt + env.wishes.cnt;

    info := NEW (Mx.InfoVec, n);
    n := FinishInfo (info, 0, env.exported_units,    u.exported_units);
    n := FinishInfo (info, n, env.imported_units,    u.imported_units);
    n := FinishInfo (info, n, env.imported_generics, u.imported_generics);
    n := FinishInfo (info, n, env.used_interfaces,   u.used_interfaces);
    n := FinishInfo (info, n, env.used_modules,      u.used_modules);
    n := FinishInfo (info, n, env.import_def_syms,   u.import_def_syms);
    n := FinishInfo (info, n, env.import_use_syms,   u.import_use_syms);
    n := FinishInfo (info, n, env.export_def_syms,   u.export_def_syms);
    n := FinishInfo (info, n, env.export_use_syms,   u.export_use_syms);
    n := FinishInfo (info, n, env.imported_types,    u.imported_types);
    n := FinishInfo (info, n, env.exported_types,    u.exported_types);
    n := FinishInfo (info, n, env.wishes,            u.wishes);

    u.info := info;
    RETURN u;
  END FinishUnitInfo;

PROCEDURE FinishInfo (info: Mx.InfoVec;  n: INTEGER;
                      READONLY x: InfoList;  VAR z: Mx.InfoList): INTEGER=
  BEGIN
    z.start := n;
    z.cnt := x.cnt;
    FOR i := 0 TO x.cnt - 1 DO
      info [n] := x.info[i];
      INC (n);
    END;
    RETURN n;
  END FinishInfo;

PROCEDURE Pass0_NoteWebInfo (env: Env;  info: TEXT) =
  BEGIN
    WebFile.Update (env.source, info);
  END Pass0_NoteWebInfo;

PROCEDURE Pass0_GetImplementations (env: Env;  intf: M3ID.T): M3Compiler.ImplList =
  BEGIN
    IF (env.source_unit = NIL) THEN RETURN NIL; END;
    IF (env.source_unit.kind # UK.I3) OR (env.source_unit.name # intf) THEN
      env.globals.compile_failed := TRUE;
      Msg.Error (NIL, "!!! UNEXPECTED GetImplementations(",
                 M3ID.ToText (intf), ")  unit = ",
                 M3Unit.FileName (env.source_unit));
      RETURN NIL;
    END;
    RETURN GetExporters (env.source_unit);
  END Pass0_GetImplementations;

(*------------------------------------------------ compilations and links ---*)

PROCEDURE NilText(t: TEXT): TEXT = 
BEGIN
  IF t = NIL THEN t := "<NIL>" END;
  RETURN t;
END NilText;

PROCEDURE RunCC (s: State;  source, object: TEXT;  debug, optimize: BOOLEAN;
                 include_path: Arg.List): BOOLEAN (* Success. *) =
  VAR failed: BOOLEAN;
  BEGIN
    IF IfDebug () THEN DoDebug ("RunCC " & NilText(source) & " " & NilText(object)); END;

    ETimer.Push (M3Timers.pass_1);
    s.machine.timer := M3Timers.pass_1;
    StartCall (s, s.c_compiler);
    PushText  (s, source);
    PushText  (s, object);
    PushArray (s, include_path);
    PushBool  (s, optimize);
    PushBool  (s, debug);
    failed := CallProc (s, s.c_compiler);
    IF failed THEN
      s.compile_failed := TRUE;
      Msg.Error (NIL, "C compiler failed compiling: ", source);
      IF NOT s.keep_files THEN Utils.Remove (object); END; 
    END;
    ETimer.Pop ();
    RETURN NOT failed;
  END RunCC;

PROCEDURE RunM3Back (s: State;  source, object: TEXT;
                     debug, optimize: BOOLEAN): BOOLEAN (* Success. *) =
  VAR failed: BOOLEAN;
  BEGIN
    ETimer.Push (M3Timers.pass_6);
    s.machine.timer := M3Timers.pass_6;
    StartCall (s, s.m3backend);
    PushText (s, source);
    PushText (s, object);
    PushBool (s, optimize);
    PushBool (s, debug);
    failed := CallProc (s, s.m3backend);
    IF failed THEN
      s.compile_failed := TRUE;
      Msg.Error (NIL, "m3cc (aka cm3cg) failed compiling: ", source);
      IF NOT s.keep_files THEN Utils.Remove (object); END;
    END;
    ETimer.Pop ();
    RETURN NOT failed;
  END RunM3Back;

PROCEDURE RunM3Llvm (s: State;  source, object: TEXT;
                     debug, optimize: BOOLEAN): BOOLEAN (* Success. *) =
  VAR failed: BOOLEAN;
  BEGIN
    ETimer.Push (M3Timers.m3llvm);
    s.machine.timer := M3Timers.m3llvm;
    StartCall (s, s.m3llvm);
    PushText (s, source);
    PushText (s, object);
    PushBool (s, optimize);
    PushBool (s, debug);
    failed := CallProc (s, s.m3llvm);
    IF failed THEN
      s.compile_failed := TRUE;
      Msg.Error (NIL, "m3llvm failed compiling: ", source);
      IF NOT s.keep_files THEN Utils.Remove (object); END;
    END;
    ETimer.Pop ();
    RETURN NOT failed;
  END RunM3Llvm;

PROCEDURE RunLlcBack 
  (s: State;  source, object: TEXT; debug, optimize: BOOLEAN; Asm: BOOLEAN)
: BOOLEAN (* Success. *) =
  VAR failed: BOOLEAN;
  VAR filetype: TEXT; 
  BEGIN
    ETimer.Push (M3Timers.llc);
    s.machine.timer := M3Timers.llc;
    StartCall (s, s.llvmbackend);
    PushText (s, source);
    PushText (s, object);
    PushBool (s, optimize);
    PushBool (s, debug);
    IF Asm THEN filetype := "asm" 
    ELSE filetype := "obj" 
    END (*IF*);
    PushText (s, filetype); 
    failed := CallProc (s, s.llvmbackend);
    IF failed THEN
      s.compile_failed := TRUE;
      Msg.Error (NIL, "llvm compiler (llc) failed compiling: ", source);
      IF NOT s.keep_files THEN Utils.Remove (object); END;
    END;
    ETimer.Pop ();
    RETURN NOT failed;
  END RunLlcBack; 

PROCEDURE RunLlvmOpt (s: State;  source, dest: TEXT) : BOOLEAN (* Success. *) =
  VAR failed: BOOLEAN;
  BEGIN
    ETimer.Push (M3Timers.llvmopt);
    s.machine.timer := M3Timers.llvmopt;
    StartCall (s, s.llvmopt);
    PushText (s, source);
    PushText (s, dest);
    failed := CallProc (s, s.llvmopt);
    IF failed THEN
      s.compile_failed := TRUE;
      Msg.Error (NIL, "llvm compiler (opt) failed optimizing: ", source);
      IF NOT s.keep_files THEN Utils.Remove (dest); END;
    END;
    ETimer.Pop ();
    RETURN NOT failed;
  END RunLlvmOpt; 

PROCEDURE RunAsm (s: State;  source, object: TEXT): BOOLEAN (* Success. *) =
  VAR failed: BOOLEAN;
  BEGIN
    ETimer.Push (M3Timers.pass_7);
    s.machine.timer := M3Timers.pass_7;
    StartCall (s, s.assembler);
    PushText (s, source);
    PushText (s, object);
    failed := CallProc (s, s.assembler);
    IF failed THEN
      s.compile_failed := TRUE;
      Msg.Error (NIL, "assembler failed assembling: ", source);
      IF NOT s.keep_files THEN Utils.Remove (object); END; 
    END;
    ETimer.Pop ();
    RETURN NOT failed;
  END RunAsm;

(*---------------------------------------------------- _m3main generation ---*)

CONST
  M3Main = "_m3main";

PROCEDURE GenerateCMain (s: State;  Main_O: TEXT) =
  VAR
    Main_C   := M3Path.Join (NIL, M3Main, UK.C);
    Main_XX  := M3Main & ".new";
    init_code: TEXT := NIL;
    time_O   : INTEGER;
    time_C   : INTEGER;
    wr       : Wr.T;
  BEGIN
    (* check for an up-to-date Main_O *)
    time_O := Utils.LocalModTime (Main_O);
    time_C := Utils.LocalModTime (Main_C);
    IF (time_O < time_C) OR (time_C = Utils.NO_TIME) THEN
      (* we must compile the linker generated code *)
      init_code := Main_C;
    ELSE
      init_code := Main_XX;
      Utils.NoteTempFile (Main_XX);
    END;

    (* produce the module init list *)
    ETimer.Push (M3Timers.genMain);
    Msg.Commands ("generate ", init_code);
    wr := Utils.OpenWriter (init_code, fatal := TRUE);
    MxGen.GenerateMain (s.link_base, wr, NIL, Msg.level >= Msg.Level.Debug,
                        (* Use of target_oskind needs work:
                           NT386GNU can generate Windowed apps. *)
                        s.gui AND (s.target_oskind = M3Path.OSKind.Win32),
                        s.lazy_init);
    Utils.CloseWriter (wr, init_code);
    ETimer.Pop ();

    IF (init_code = Main_XX) AND Utils.IsEqual (Main_XX, Main_C) THEN
      (* we don't need to compile! *)
      Utils.Remove (Main_XX);
    ELSE
      IF (init_code = Main_XX) THEN
        Utils.Copy (Main_XX, Main_C);
        Utils.Remove (Main_XX);
      END;
      Msg.Debug ("compiling ", Main_C, " ...", Wr.EOL);
      EVAL RunCC (s, Main_C, Main_O, debug := TRUE, optimize := FALSE, include_path := s.include_path_empty);
      IF (s.compile_failed) THEN
        Msg.FatalError (NIL, "cc ", Main_C, " failed!!");
      END;
      Utils.NoteNewFile (Main_O);
      Utils.NoteNewFile (Main_C);
    END;
  END GenerateCMain;

(*------------------------------------------------ compilations and links ---*)

PROCEDURE BuildCProgram (s: State;  shared: BOOLEAN) =
  VAR
    name        := M3Path.Parse (s.result_name);
    pgm_file    := M3Path.ProgramName (name.base);
    pgmTime     : INTEGER;
    pgmValid    : BOOLEAN;
    pgm_objects : Arg.List;
    import_libs : Arg.List;
  BEGIN
    IF (s.bootstrap_mode) THEN RETURN; END;

    IF (s.compile_failed) THEN
      DontLink (s, name.base, shared);
      Msg.Explain ("compilation failed => not building program \"",pgm_file,"\"");
      RETURN;
    END;

    pgmTime := Utils.LocalModTime (pgm_file);
    pgmValid := (pgmTime # Utils.NO_TIME);
    IF NOT pgmValid AND NOT s.skip_link THEN
      Msg.Explain (" -> linking ", pgm_file);
    END;

    IF s.skip_link
      THEN pgm_objects := GetObjects (s, pgmTime, pgmValid, NIL, NIL);
      ELSE pgm_objects := GetObjects (s, pgmTime, pgmValid, "linking ", pgm_file);
    END;

    IF (s.do_coverage) THEN
      Arg.Append (pgm_objects, s.link_coverage);
    END;

    IF s.skip_link
      THEN import_libs := GetLibraries (s, pgmTime, pgmValid, NIL, NIL, FALSE, shared);
      ELSE import_libs := GetLibraries (s, pgmTime, pgmValid, "linking ", pgm_file,
                                        NOT shared AND s.broken_linker, shared);
    END;

    IF pgmValid OR s.skip_link THEN
      DontLink (s, name.base, shared);
      RETURN;
    END;

    ETimer.Push (M3Timers.pass_2);
      StartCall (s, s.linker);
      PushText  (s, name.base);
      PushArray (s, Arg.NewList ());
      PushArray (s, pgm_objects);
      PushArray (s, import_libs);
      PushBool  (s, shared);
      IF CallProc (s, s.linker) THEN
        s.compile_failed := TRUE;
        Msg.Error (NIL, "linker failed linking: ", name.base);
      END;
    ETimer.Pop ();
  END BuildCProgram;


PROCEDURE BuildProgram (s: State;  shared: BOOLEAN) =
  CONST Desc_file = ".M3LINK";
  VAR
    name        := M3Path.Parse (s.result_name);
    pgm_file    := M3Path.ProgramName (name.base);
    pgmTime     : INTEGER;
    pgmValid    : BOOLEAN;
    pgm_objects : Arg.List;
    import_libs : Arg.List;
    Main_O      := M3Path.Join (NIL, M3Main, UK.O);
  BEGIN
    <*ASSERT NOT s.bootstrap_mode *>

    IF (s.compile_failed) THEN
      DontLink (s, name.base, shared);
      Msg.Explain ("compilation failed => not building program \"",pgm_file,"\"");
      IF s.has_loader THEN Utils.Remove (Desc_file); END;
      RETURN;
    END;

    pgmTime := Utils.LocalModTime (pgm_file);
    pgmValid := (pgmTime # Utils.NO_TIME);
    IF NOT pgmValid AND NOT s.skip_link THEN
      Msg.Explain (" -> linking ", pgm_file);
    END;

    IF s.skip_link
      THEN pgm_objects := GetObjects (s, pgmTime, pgmValid, NIL, NIL);
      ELSE pgm_objects := GetObjects (s, pgmTime, pgmValid, "linking ", pgm_file);
    END;
    Arg.Prepend (pgm_objects, Main_O);

    IF (s.do_coverage) THEN
      Arg.Append (pgm_objects, s.link_coverage);
    END;

    IF s.skip_link
      THEN import_libs := GetLibraries (s, pgmTime, pgmValid, NIL, NIL, FALSE, shared);
      ELSE import_libs := GetLibraries (s, pgmTime, pgmValid, "linking ", pgm_file,
                                        NOT shared AND s.broken_linker, shared);
    END;

    IF pgmValid THEN
      DontLink (s, name.base, shared);
      RETURN;
    END;

    ETimer.Push (M3Timers.chkpgm);
    IF NOT MxCheck.IsProgram (s.link_base, Stdio.stdout) THEN
      IF s.has_loader THEN Utils.Remove (Desc_file); END;
      Msg.FatalError (NIL, "incomplete program");
    END;
    ETimer.Pop ();

    (* produce the module init list & program entry point *)
    GenerateCMain (s, Main_O);

    IF s.has_loader THEN WriteProgramDesc (s, Desc_file, Main_O); END;

    IF s.skip_link THEN
      DontLink (s, name.base, shared);
      RETURN;
    END;

    ETimer.Push (M3Timers.pass_2);
      StartCall (s, s.linker);
      PushText  (s, name.base);
      PushArray (s, Arg.NewList ());
      PushArray (s, pgm_objects);
      PushArray (s, import_libs);
      PushBool  (s, shared);
      IF CallProc (s, s.linker) THEN
        s.compile_failed := TRUE;
        Msg.Error (NIL, "linker failed linking: ", name.base);
      END;
    ETimer.Pop ();
  END BuildProgram;

PROCEDURE DontLink (s: State;  name: TEXT;  shared: BOOLEAN) =
  BEGIN
    StartCall (s, s.skip_linker);
    PushText  (s, name);
    PushBool  (s, shared);
    EVAL CallProc (s, s.skip_linker);
  END DontLink;

PROCEDURE GetObjects (s: State;  result_time: INTEGER;
                      VAR valid: BOOLEAN;  verb, result: TEXT): Arg.List =
  VAR u := s.units.head;  objs := Arg.NewList ();
  BEGIN
    WHILE (u # NIL) DO
      IF (u.object # NIL) THEN
        IF valid AND (Utils.LocalModTime (u.object) > result_time) THEN
          IF (verb # NIL) THEN
            Msg.Explain ("new \"",u.object,"\" -> ", verb & result);
          END;
          valid := FALSE;
        END;
        Arg.Append (objs, u.object);
      END;
      u := u.next;
    END;
    RETURN objs;
  END GetObjects;

(* Original: 
PROCEDURE GetLibraries (s: State;  result_time: INTEGER;
                         VAR valid: BOOLEAN;  verb, result: TEXT;
                         use_links: BOOLEAN; shared: BOOLEAN): Arg.List =
  VAR
    u := s.units.head;
    libs := Arg.NewList ();
    lib_file : TEXT;
    lib_link : TEXT;
    lib_path := NEW (IntRefTbl.Default).init();
    link_dir : TEXT := NIL;
  BEGIN
    (* NOTE: we build the m3 library list in reverse order since
       they're discovered in bottom-up order and Unix linkers prefer
       them in top-down order... *)
     WHILE (u # NIL) DO
      IF (u.imported AND u.kind = UK.M3LIB) OR (u.kind = UK.LIB) THEN
        lib_file := UnitPath (u);
        IF valid AND (Utils.ModificationTime (lib_file) > result_time) THEN
          IF (verb # NIL) THEN
            Msg.Explain ("new \"",lib_file,"\" -> ", verb & result);
          END;
          valid := FALSE;
        END;
        IF use_links THEN
          IF link_dir = NIL THEN
            link_dir := result & ".libs";
            IF NOT M3File.IsDirectory (link_dir) THEN
              Dirs.MkDir (link_dir);
            END;
          END;
          IF (u.loc.path # NIL) THEN
            lib_link := M3Path.New (link_dir, M3Unit.FileName (u));
            Utils.SymbolicLinkFile (lib_file, lib_link);
          END;
          Arg.Prepend (libs, "-l" & M3ID.ToText (u.name));
        ELSIF (NOT shared OR s.keep_resolved) THEN
          Arg.Prepend (libs, lib_file);
        ELSE
          Arg.Prepend (libs, "-l" & M3ID.ToText (u.name));
          IF (u.loc.path # NIL)
            AND NOT lib_path.put (M3ID.Add (u.loc.path), NIL) THEN
            Arg.Prepend (libs, "-L" & u.loc.path);
            IF (s.Rpath_flag # NIL) AND (Text.Length (s.Rpath_flag) > 0) THEN
              (* For shared libs, augment the run-time library search path. *)
              Arg.Prepend (libs, s.Rpath_flag & u.loc.path)
            END;
          END;
        END;
      END;
      u := u.next;
    END;
    IF link_dir # NIL THEN Arg.Prepend (libs, "-L" & link_dir); END;
    Arg.AppendL (libs, s.sys_libs);
    RETURN libs;
  END GetLibraries;
*) 

PROCEDURE GetLibraries (s: State;  result_time: INTEGER;
                         VAR valid: BOOLEAN;  verb, result: TEXT;
                         use_links: BOOLEAN; shared: BOOLEAN): Arg.List =
    (* NOTE: we build the m3 library list in reverse order since
       they're discovered in bottom-up order and Unix linkers prefer
       them in top-down order.  We still have to build the list 
       left-to-right, so that if there are multiple libs in the same
       path, they all follow the -L and Rpath options for the path.  
       So we reverse library order without altering s.units by recursing. *)
  VAR
    libs := Arg.NewList ();
    lib_path := NEW (IntRefTbl.Default).init();
    link_dir : TEXT := NIL;

  PROCEDURE Recurse (u: M3Unit.T) =
    VAR 
      lib_file : TEXT;
      lib_link : TEXT;
    BEGIN 
      IF u # NIL THEN 
        Recurse (u.next);
        IF (u.imported AND u.kind = UK.M3LIB) OR (u.kind = UK.LIB) THEN
          lib_file := UnitPath (u);
          IF valid AND (Utils.ModificationTime (lib_file) > result_time) THEN
            IF (verb # NIL) THEN
              Msg.Explain ("new \"",lib_file,"\" -> ", verb & result);
            END;
            valid := FALSE;
          END;
          IF use_links THEN
            IF link_dir = NIL THEN
              link_dir := result & ".libs";
              IF NOT M3File.IsDirectory (link_dir) THEN
                Dirs.MkDir (link_dir);
              END;
            END;
            IF (u.loc.path # NIL) THEN
              lib_link := M3Path.New (link_dir, M3Unit.FileName (u));
              Utils.SymbolicLinkFile (lib_file, lib_link);
            END;
            Arg.Append (libs, "-l" & M3ID.ToText (u.name));
          ELSIF (NOT shared OR s.keep_resolved) THEN
            Arg.Append (libs, lib_file);
          ELSE
            IF (u.loc.path # NIL)
               AND NOT lib_path.put (M3ID.Add (u.loc.path), NIL) THEN
            (* A path we have not seen before. *) 
              IF (s.Rpath_flag # NIL) AND (Text.Length (s.Rpath_flag) > 0) THEN
                (* For shared libs, augment the run-time library search path. *)
                Arg.Append (libs, s.Rpath_flag & u.loc.path)
              END;
              Arg.Append (libs, "-L" & u.loc.path);
            END;
            Arg.Append (libs, "-l" & M3ID.ToText (u.name));
          END;
        END;
      END; 
    END Recurse; 
 
  BEGIN
    Recurse (s.units.head); 
    IF link_dir # NIL THEN Arg.Prepend (libs, "-L" & link_dir); END;
    Arg.AppendL (libs, s.sys_libs);
    RETURN libs;
  END GetLibraries;

PROCEDURE WriteProgramDesc (s: State;  desc_file, main_o: TEXT) =
  VAR u: M3Unit.T;  lib_file: TEXT;

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      (* Use of target_oskind needs work. *)
      IF (s.target_oskind = M3Path.OSKind.Win32) THEN
        Wr.PutText (wr, "-out:");
        Wr.PutText (wr, s.result_name);
        Wr.PutText (wr, ".exe");
        Wr.PutText (wr, Wr.EOL);
        IF (s.gui)
          THEN Wr.PutText (wr, "-subsystem:windows");
          ELSE Wr.PutText (wr, "-subsystem:console");
        END;
        Wr.PutText (wr, Wr.EOL);
      ELSE
        Wr.PutText (wr, "-o ");
        Wr.PutText (wr, s.result_name);
        Wr.PutText (wr, Wr.EOL);
      END;

      (* write the library timestamps *)
      u := s.units.head;
      WHILE (u # NIL) DO
        IF (u.imported) AND ((u.kind = UK.M3LIB) OR (u.kind = UK.LIB)) THEN
          lib_file := UnitPath (u);
          Wr.PutText (wr, lib_file);
          Wr.PutChar (wr, ' ');
          Wr.PutText (wr, Fmt.Int (Utils.ModificationTime (lib_file)));
          Wr.PutText (wr, Wr.EOL);
        END;
        u := u.next;
      END;

      IF (s.do_coverage) THEN
        Wr.PutText (wr, s.link_coverage);
        Wr.PutChar (wr, ' ');
        Wr.PutText (wr, Fmt.Int (Utils.ModificationTime (s.link_coverage)));
        Wr.PutText (wr, Wr.EOL);
      END;

      (* write the object timestamps *)
      u := s.units.head;
      WHILE (u # NIL) DO
        IF (u.object # NIL) THEN
          Wr.PutText (wr, u.object);
          Wr.PutChar (wr, ' ');
          Wr.PutText (wr, Fmt.Int (Utils.LocalModTime (u.object)));
          Wr.PutText (wr, Wr.EOL);
        END;
        u := u.next;
      END;

      (* add the linker generated main body *)
      Wr.PutText (wr, main_o);
      Wr.PutChar (wr, ' ');
      Wr.PutText (wr, Fmt.Int (Utils.LocalModTime (main_o)));
      Wr.PutText (wr, Wr.EOL);
    END Emit;

  BEGIN
    ETimer.Push (M3Timers.genLink);
    Utils.WriteFile (desc_file, Emit, append := FALSE);
    ETimer.Pop ();
  END WriteProgramDesc;

PROCEDURE BuildBootProgram (s: State) =
  VAR Main_C: TEXT;  makefile := "make." & s.result_name;

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      Wr.PutText (wr, "# objects for program " & s.result_name);
      Wr.PutText (wr, Wr.EOL);
      Wr.PutText (wr, Wr.EOL);
      GenObjectList (s, wr, M3Path.Join (NIL, "_m3main", UK.O));
      Wr.PutText (wr, Wr.EOL);

      Wr.PutText (wr, "# libraries for program " & s.result_name);
      Wr.PutText (wr, Wr.EOL);
      Wr.PutText (wr, Wr.EOL);
      GenLibraryList (s, wr);
      Wr.PutText (wr, Wr.EOL);
    END Emit;

  PROCEDURE EmitMain (wr: Wr.T) RAISES {} =
    BEGIN
      MxGen.GenerateMain (s.link_base, wr, NIL, Msg.level >=Msg.Level.Debug,
                          (* Use of target_oskind needs work:
                             NT386GNU can generate Windowed apps. *)
                          s.gui AND (s.target_oskind = M3Path.OSKind.Win32),
                          s.lazy_init);
    END EmitMain;

  BEGIN
    <*ASSERT s.bootstrap_mode *>

    IF (s.compile_failed) THEN
      Msg.Explain ("compilation failed => not building program \"",
                   s.result_name,"\"");
      Utils.Remove (makefile);
      RETURN;
    END;

    ETimer.Push (M3Timers.chkpgm);
    IF NOT MxCheck.IsProgram (s.link_base, Stdio.stdout) THEN
      Msg.FatalError (NIL, "incomplete program");
    END;
    ETimer.Pop ();

    (* produce the module init list *)
    ETimer.Push (M3Timers.genMain);
    Main_C := M3Path.Join (NIL, "_m3main", UK.C);
    Msg.Commands ("generate ", Main_C);
    Utils.WriteFile (Main_C, EmitMain, append := FALSE);
    ETimer.Pop ();

    Msg.Explain ("building makefile -> ", makefile);
    Utils.WriteFile (makefile, Emit, append := FALSE);
  END BuildBootProgram;

PROCEDURE GenLibraryList (s: State;  wr: Wr.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR u: M3Unit.T;  x: Arg.T;
  BEGIN
    Wr.PutText (wr, s.result_name & "_LIBS = \134");
    Wr.PutText (wr, Wr.EOL);

    (* emit the imported libraries *)
    u := s.units.head;
    WHILE (u # NIL) DO
      IF (u.imported) AND (u.kind = UK.M3LIB OR u.kind = UK.LIB) THEN
        Wr.PutText (wr, "  ");
        IF (u.loc.path = NIL) THEN
          Wr.PutText (wr, "-l" & M3ID.ToText (u.name));
        ELSE
          Wr.PutText (wr, M3Path.Convert (
                              M3Path.Join (u.loc.path, M3ID.ToText (u.name),
                                           u.kind)));
        END;
        IF (u.next # NIL) OR (s.sys_libs.cnt > 0) THEN
          Wr.PutText (wr, "\134");
        END;
        Wr.PutText (wr, Wr.EOL);
      END;
      u := u.next;
    END;

    (* emit the system library goo *)
    x := s.sys_libs.head;
    WHILE (x # NIL) DO
      Wr.PutText (wr, "  ");
      Wr.PutText (wr, x.arg);
      IF (x.next # NIL) THEN
        Wr.PutText (wr, "\134");
      END;
      Wr.PutText (wr, Wr.EOL);
      x := x.next;
    END;

    Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, Wr.EOL);
  END GenLibraryList;

PROCEDURE BuildLibrary (s: State;  shared: BOOLEAN) =
  VAR
    name        := M3Path.Parse (s.result_name);
    lib_file    := M3Path.LibraryName (name.base);
    lib_time    : INTEGER;
    libValid    : BOOLEAN;
    lib_objects : Arg.List;
    import_libs : Arg.List;
  BEGIN
    <*ASSERT NOT s.bootstrap_mode *>

    IF (s.compile_failed) THEN
      DontBuildLibrary (s, name.base, shared);
      Msg.Explain ("compilation failed => not building library \"",
                   lib_file, "\"");
      RETURN;
    END;

    lib_time := Utils.LocalModTime (lib_file);
    libValid := (lib_time # Utils.NO_TIME);
    IF (lib_time = Utils.NO_TIME) THEN
      Msg.Explain (" -> archiving ", lib_file);
      libValid := FALSE;
    END;

    lib_objects := GetObjects   (s, lib_time, libValid, "archiving ", lib_file);
    import_libs := GetLibraries (s, lib_time, libValid, "archiving ", lib_file,
                                 FALSE, shared);

    IF libValid THEN
      DontBuildLibrary (s, name.base, shared);
      RETURN;
    END;

    ETimer.Push (M3Timers.chkpgm);
    IF NOT MxCheck.IsLibrary (s.link_base, Stdio.stdout) THEN
      Msg.FatalError (NIL, "incomplete library");
    END;
    ETimer.Pop ();

    Msg.Debug ("building the library...", Wr.EOL);
    Utils.Remove (lib_file);

    IF (s.target_oskind = M3Path.OSKind.Win32) THEN
      GenLibDef (name.base);
    END;

    ETimer.Push (M3Timers.pass_3);
      StartCall (s, s.librarian);
      PushText  (s, name.base);
      PushArray (s, Arg.NewList ());
      PushArray (s, lib_objects);
      PushArray (s, import_libs);
      PushBool  (s, shared);
      IF CallProc (s, s.librarian) THEN
        s.compile_failed := TRUE;
        Msg.Error (NIL, "librarian failed building: ", name.base);
      END;
    ETimer.Pop ();
  END BuildLibrary;

PROCEDURE DontBuildLibrary (s: State;  name: TEXT;  shared: BOOLEAN) =
  BEGIN
    StartCall (s, s.skip_lib);
    PushText  (s, name);
    PushBool  (s, shared);
    EVAL CallProc (s, s.skip_lib);
  END DontBuildLibrary;

PROCEDURE BuildBootLibrary (s: State) =
  VAR makefile := "make." & s.result_name;

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      Wr.PutText (wr, "% objects for Modula-3 library " & s.result_name);
      Wr.PutText (wr, Wr.EOL);
      Wr.PutText (wr, Wr.EOL);
      GenObjectList (s, wr, NIL);
      Wr.PutText (wr, Wr.EOL);
      Wr.PutText (wr, Wr.EOL);
    END Emit;

  BEGIN
    <*ASSERT s.bootstrap_mode *>

    IF (s.compile_failed) THEN
      Msg.Explain ("compilation failed => not building library \"",
                   s.result_name,"\"");
      Utils.Remove (makefile);
      RETURN;
    END;

    ETimer.Push (M3Timers.chkpgm);
    IF NOT MxCheck.IsLibrary (s.link_base, Stdio.stdout) THEN
      Msg.FatalError (NIL, "incomplete library");
    END;
    ETimer.Pop ();

    Msg.Explain ("building makefile -> ", makefile);
    Utils.WriteFile (makefile, Emit, append := FALSE);
  END BuildBootLibrary;

PROCEDURE GenLibDef (libname: TEXT) =

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      Wr.PutText (wr, "LIBRARY ");
      Wr.PutText (wr, libname);
      Wr.PutText (wr, Wr.EOL);
    END Emit;

  BEGIN
    Utils.WriteFile (libname & ".def", Emit, append := FALSE);
  END GenLibDef;

PROCEDURE GenObjectList (s: State;  wr: Wr.T;  extra: TEXT)
  RAISES {Wr.Failure, Thread.Alerted} =
  CONST MaxChunk = 30;
  VAR cnt := 0;  u: M3Unit.T;  n_chunks := 0;  width := 0;  subunit := 0;

  PROCEDURE Out (nm: TEXT) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      IF (width > 65) THEN
        Wr.PutText (wr, " \134");
        Wr.PutText (wr, Wr.EOL);
        Wr.PutText (wr, "  ");
        width := 0;
      END;
      Wr.PutText (wr, " ");
      Wr.PutText (wr, nm);
      INC (width, Text.Length (nm));
    END Out;

  BEGIN
    (* see how many we got... *)
    u := s.units.head;
    WHILE (u # NIL) DO
      IF (u.object # NIL) THEN INC (cnt); END;
      u := u.next;
    END;
    IF (extra # NIL) THEN INC (cnt); END;

    IF (cnt < MaxChunk) THEN
      (* this is the easy case, there's just one list *)
      Wr.PutText (wr, s.result_name & "_OBJECTS = \134");
      Wr.PutText (wr, Wr.EOL);
      Wr.PutText (wr, "  ");
      u := s.units.head;
      WHILE (u # NIL) DO
        IF (u.object # NIL) THEN Out (u.object); END;
        u := u.next;
      END;
      IF (extra # NIL) THEN Out (extra); END;
      Wr.PutText (wr, Wr.EOL);
      Wr.PutText (wr, Wr.EOL);
      RETURN;
    END;

    (* too many items => we need to build sublists *)
    n_chunks := (cnt + MaxChunk - 1) DIV MaxChunk;

    u := s.units.head;
    WHILE (u # NIL) DO
      Wr.PutText (wr, Wr.EOL);
      Wr.PutText (wr, s.result_name & "_OBJ_" & Fmt.Int (subunit) & " = \134");
      Wr.PutText (wr, Wr.EOL);
      Wr.PutText (wr, "  ");
      width := 0; cnt := 0;
      WHILE (cnt < MaxChunk) AND (u # NIL) DO
        IF (u.object # NIL) THEN Out (u.object); INC (cnt); END;
        u := u.next;
      END;
      Wr.PutText (wr, Wr.EOL);
      Wr.PutText (wr, Wr.EOL);
      INC (subunit);
    END;
    IF (extra # NIL) THEN Out (extra); END;
    Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, Wr.EOL);

    width := 0;
    Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, s.result_name & "_OBJECTS = \134");
    Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, "  ");
    FOR i := 0 TO n_chunks-1 DO
      Out (s.result_name & "_OBJ_" & Fmt.Int (i));
    END;
    Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, Wr.EOL);
    Wr.PutText (wr, Wr.EOL);
  END GenObjectList;

(*--------------------------------------------------------- version stamps --*)

PROCEDURE GetUnitLinkInfo (u: M3Unit.T;  imported: BOOLEAN): Mx.UnitList =
  VAR info: TEXT;  kind := UK.LIBX;
  BEGIN
    CASE u.kind OF
    | UK.M3LIB, UK.LIBX =>  kind := UK.LIBX;
    | UK.PGM, UK.PGMX   =>  kind := UK.PGMX;
    ELSE Msg.FatalError (NIL, "Builder.GetUnitLinkInfo:  mysterious unit type");
    END;
    info := M3Path.Join (u.loc.path, M3ID.ToText (u.name), kind);
    RETURN GetLinkUnits (info, UnitPath (u), imported);
  END GetUnitLinkInfo;

PROCEDURE GetLinkUnits (info, file: TEXT;  imported: BOOLEAN): Mx.UnitList =
  VAR
    rd: File.T;
    wr: Wr.T;
    units: Mx.UnitList;
    start, stop: INTEGER;
  BEGIN
    IF (Msg.level >= Msg.Level.Verbose) THEN start := ROUND (Time.Now ()) END;

    (* try to open file's link info file *)
    TRY
      rd := FS.OpenFileReadonly (info);
    EXCEPT OSError.E (args) =>
      Msg.Debug ("unable to open link info file: ",
                 info, Msg.OSErr (args), Wr.EOL);
      RETURN NIL;
    END;

    IF (Msg.level < Msg.Level.Verbose)
      THEN  wr := NIL;
      ELSE  wr := Stdio.stdout;
    END;

    (* try to read the file *)
    TRY
      units := MxIn.ReadUnits (rd, file, imported, wr);
    FINALLY
      Utils.CloseReader (rd, info);
    END;
    IF (units = NIL) THEN
      IF (imported)
        THEN Msg.FatalError (NIL, "bad link info file: ", info);
        ELSE Msg.Debug ("bad link info file: ", info, Wr.EOL);
      END;
      RETURN NIL;
    END;

    IF (Msg.level >= Msg.Level.Verbose) THEN
      stop := ROUND (Time.Now ());
      Msg.Verbose ("reading \"", info, "\": ", Fmt.Int(stop-start), " seconds");
    END;
    RETURN units;
  END GetLinkUnits;

PROCEDURE MergeUnit (s: State;  u: Mx.Unit;  optional := TRUE): BOOLEAN =
  CONST KMap = ARRAY BOOLEAN OF UK { UK.M3, UK.I3 };
  VAR
    wr := Stdio.stdout;
    bad, ux: Mx.UnitList;
    x: Mx.Unit;
    ok := TRUE;
    unit: M3Unit.T;
    kind: UK;
  BEGIN
    IF (u = NIL) THEN RETURN TRUE END;
    IF (optional) AND (Msg.level < Msg.Level.Debug) THEN wr := NIL END;

    bad := MxMerge.MergeUnit (u, s.link_base, wr);

    (* add u's magic info if it was ok *)
    ux := bad;
    LOOP
      IF (ux = NIL) THEN  AddMagic (s, u);  EXIT  END;
      IF (ux.unit = u) THEN EXIT END;
      ux := ux.next;
    END;

    IF (bad = NIL) THEN RETURN TRUE END;

    (* try to fix as many units as possible *)
    WHILE (bad # NIL) DO
      x := bad.unit;
      kind := KMap [x.interface];
      unit := M3Unit.Get (s.units, x.name, kind);
      IF (x # u) AND (unit # NIL) THEN
        CompileOne (s, unit);
      ELSE
        IF (NOT optional) THEN
          Msg.FatalError (NIL, "bad version stamps: ",
                          M3Path.Join (NIL, M3ID.ToText (x.name), kind));
        END;
        ok := FALSE
      END;
      bad := bad.next;
    END;

    RETURN ok;
  END MergeUnit;

PROCEDURE AddMagic (s: State;  u: Mx.Unit) =
  VAR o := u.exported_objects;
  BEGIN
    WHILE (o # NIL) DO
      EVAL s.magic.put (o.type, o);
      o := o.next;
    END;
  END AddMagic;

(*----------------------------------------------------------- file names ---*)

PROCEDURE UnitPath (u: M3Unit.T): TEXT =
  VAR path := M3Unit.FullPath (u);
  BEGIN
    IF    M3Path.MakeRelative (path, Dirs.source,  Dirs.to_source ) THEN
    ELSIF M3Path.MakeRelative (path, Dirs.derived, ".") THEN
    ELSIF M3Path.MakeRelative (path, Dirs.package, Dirs.to_package) THEN
    END;
    RETURN path;
  END UnitPath;

PROCEDURE Cm3IRNameForUnit (u: M3Unit.T): TEXT =
  VAR ext := u.kind;
  BEGIN
    CASE ext OF
    | UK.I3, UK.IC => ext := UK.IC;
    | UK.IS        => ext := UK.IS;
    | UK.M3, UK.MC => ext := UK.MC;
    | UK.MS        => ext := UK.MS;
    ELSE <* ASSERT FALSE *>
    END;
    RETURN M3Path.Join (NIL, M3ID.ToText (u.name), ext);
  END Cm3IRNameForUnit;

PROCEDURE LlvmIRNameForUnit (u: M3Unit.T): TEXT =

(* TODO: This is duplicated by code in LLGen.BitcodeFileNameTOfUnitText,
         which must agree with it.  Instead, copy this value to there.
         The call chain starts with the call on M3Front.Compile, in RunM3Front.
*) 
  VAR ext := u.kind;
  BEGIN
    CASE ext OF
    | UK.I3, UK.IC => ext := UK.IB;
    | UK.IS        => ext := UK.IS;
    | UK.M3, UK.MC => ext := UK.MB;
    | UK.MS        => ext := UK.MS;
    ELSE <* ASSERT FALSE *>
    END;
    RETURN M3Path.Join (NIL, M3ID.ToText (u.name), ext);
  END LlvmIRNameForUnit;

  PROCEDURE LlvmIROptNameForUnit (u: M3Unit.T): TEXT =
  VAR ext := u.kind;
  BEGIN
    CASE ext OF
    | UK.I3, UK.IC => ext := UK.IB;
    | UK.M3, UK.MC => ext := UK.MB;
    ELSE <* ASSERT FALSE *>
    END;
    RETURN M3Path.Join (NIL, M3ID.ToText (u.name) & "_opt", ext);
  END LlvmIROptNameForUnit;

PROCEDURE AsmNameForUnit (u: M3Unit.T): TEXT =
  VAR ext := u.kind;
  BEGIN
    CASE ext OF
    | UK.I3, UK.IC => ext := UK.IS;
    | UK.M3, UK.MC => ext := UK.MS;
    ELSE <* ASSERT FALSE *>
    END;
    RETURN M3Path.Join (NIL, M3ID.ToText (u.name), ext);
  END AsmNameForUnit;

PROCEDURE FinalNameForUnitInternal (s: State; u: M3Unit.T; boot: BOOLEAN): TEXT =
(* Name of final file to be produced. *) 
  VAR ext := u.kind;
      mode := s.m3backend_mode;
      asm := mode IN Target.BackendAsmSet; 
      C := mode IN Target.BackendCSet;
      m3cc := mode IN Target.BackendM3ccSet; 
      Integrated := mode IN Target.BackendIntegratedSet; 
  BEGIN
    IF NOT boot THEN
      (* Final output is object modules. *)
      CASE ext OF 
      | UK.I3, UK.IC, UK.IB, UK.IS => ext :=  UK.IO;
      | UK.M3, UK.MC, UK.MB, UK.MS => ext :=  UK.MO;
      | UK.C, UK.S                 => ext :=  UK.O;
      | UK.IO, UK.MO, UK.O         => RETURN M3Unit.FileName (u);
      ELSE RETURN NIL;
      END;
      
    ELSIF C THEN 
      (* Bootstrap with C integrated backend, final boot output is .c files. *) 
      CASE ext OF
      | UK.I3, UK.M3 => RETURN M3Unit.FileName (u) & ".c"; (* ?FUTURE: .cpp *)
      | UK.IO, UK.IB, 
        UK.MO, UK.MB, 
        UK.C, UK.H, UK.S, UK.O => RETURN M3Unit.FileName (u);
      ELSE RETURN NIL;
      END;

    ELSIF asm THEN
      (* Backend mode can produce assembly code, it will be final boot output. *)
      CASE ext OF
      | UK.I3, UK.IC, UK.IB        =>  ext :=  UK.IS;
      | UK.M3, UK.MC, UK.MB        =>  ext :=  UK.MS;
      | UK.B                       =>  ext :=  UK.MS;
      | UK.IS, UK.IO, 
        UK.MS, UK.MO, 
        UK.H, UK.S, UK.O, UK.C     => RETURN M3Unit.FileName (u);
      ELSE RETURN NIL;
      END;

    ELSIF Integrated THEN (* Integrated backend produces object files. *)
      CASE ext OF 
      | UK.I3, UK.IC, UK.IB, UK.IS => ext :=  UK.IO;
      | UK.M3, UK.MC, UK.MB, UK.MS => ext :=  UK.MO;
      | UK.C, UK.S, UK.H,
        UK.IO, UK.MO, UK.O         => RETURN M3Unit.FileName (u);
      ELSE RETURN NIL;
      END;

    ELSIF m3cc THEN (* External m3cc back end produces object. *)  
      (* Doesn't produce assembly, final boot output is input to m3cc. *)
      CASE ext OF
      | UK.I3                       =>  ext :=  UK.IC;
      | UK.M3                       =>  ext :=  UK.MC;
      | UK.IC, UK.IB, UK.IS, UK.IO, 
        UK.MC, UK.MB, UK.MS, UK.MO,
        UK.C, UK.H, UK.S, UK.O      => RETURN M3Unit.FileName (u); 
      ELSE RETURN NIL;
      END;

    ELSE (* External llvm backend produces object. *) 
      (* Doesn't produce assembly, final boot output is input to llvm. *)
      CASE ext OF
      | UK.I3                      =>  ext :=  UK.IB;
      | UK.M3                      =>  ext :=  UK.MB;
      | UK.IC, UK.IB, UK.IS, UK.IO, 
        UK.MC, UK.MB, UK.MS, UK.MO,
        UK.C, UK.H, UK.S, UK.O     => RETURN M3Unit.FileName (u); 
      ELSE RETURN NIL;
      END;
    END;

    RETURN M3Path.Join (NIL, M3ID.ToText (u.name), ext);
  END FinalNameForUnitInternal;

PROCEDURE FinalNameForUnit (s: State;  u: M3Unit.T) =
(* Name of final file or files to be produced. *) 
  VAR boot := s.bootstrap_mode; (* typically FALSE *)
      object := FinalNameForUnitInternal (s, u, FALSE);
  BEGIN
    IF NOT boot THEN
      u.object := object;
      RETURN;
    END;
    u.boot_makefile_object := object;
    u.object := FinalNameForUnitInternal (s, u, TRUE);
  END FinalNameForUnit;

(*------------------------------------------------------------------ misc ---*)

PROCEDURE PullForBootstrap (u: M3Unit.T) =
  VAR path := UnitPath (u);
  BEGIN
    IF NOT Text.Equal (path, u.object) THEN
      Utils.Remove (u.object);
      Utils.Copy (path, u.object);
    END;
  END PullForBootstrap;

(*------------------------------------------------------- quake utilities ---*)

PROCEDURE StartCall (s: State;  READONLY p: ConfigProc) =
  BEGIN
    IF (p.binding = NIL) THEN
      Msg.FatalError (NIL, "procedure \"", p.name,
                      "\" was not defined in \"", s.config_file & "\"");
    END;
    TRY
      s.machine.start_call (p.binding.value);
    EXCEPT Quake.Error (msg) =>
      Msg.Out (msg, Wr.EOL);
      Msg.FatalError (NIL, "procedure \"", p.name,
                      "\" defined in \"" & s.config_file,
                      "\" failed.");
    END;
  END StartCall;

PROCEDURE CallProc (s: State;  READONLY p: ConfigProc): BOOLEAN =
  VAR v: QValue.T;  sav: BOOLEAN;  exit_code := 0;
  BEGIN
    TRY
      sav := s.machine.exec_echo (Msg.level >= Msg.Level.Commands);
      s.machine.call_proc (p.n_args, TRUE);
      s.machine.pop (v);
      EVAL s.machine.exec_echo (sav);
      exit_code := QVal.ToInt (s.machine, v);
      IF exit_code # 0 THEN
        Msg.Error (NIL, "  ", p.name, " => ", Fmt.Int (exit_code));
      END;
    EXCEPT
    | Quake.Error (msg) =>
        Msg.Out (msg, Wr.EOL);
        Msg.FatalError (NIL, "procedure \"", p.name,
                        "\" defined in \"" & s.config_file,
                        "\" failed.");
        exit_code := LAST(INTEGER);
    | Thread.Alerted =>
        Msg.FatalError (NIL, "interrupted while calling \"", p.name,
                        "\" defined in \"" & s.config_file, "\"");
        exit_code := LAST(INTEGER)-1;
    END;
    Msg.Verbose ("  ", p.name, " => ", Fmt.Int (exit_code));
    RETURN (exit_code # 0);
  END CallProc;

PROCEDURE PushBool (s: State;  bool: BOOLEAN) =
  BEGIN
    QMachine.PushBool (s.machine, bool);
  END PushBool;

PROCEDURE PushText (s: State;  txt: TEXT) =
  BEGIN
    QMachine.PushText (s.machine, txt);
  END PushText;

PROCEDURE PushArray (s: State;  args: Arg.List) =
  VAR v: QValue.T;  arr := NEW (QVSeq.T).init (args.cnt);  x := args.head;
  BEGIN
    v.kind := QValue.Kind.String;
    v.ref  := NIL;
    WHILE (x # NIL) DO
      v.int := M3ID.Add (x.arg);
      arr.addhi (v);
      x := x.next;
    END;
    v.kind := QValue.Kind.Array;
    v.int  := 0;
    v.ref  := arr;
    s.machine.push (v);
  END PushArray;

(*---------------------------------------------------------------- errors ---*)

PROCEDURE BadFile (msg: TEXT;  u: M3Unit.T) =
  BEGIN
    Msg.FatalError (NIL, msg, ": ", FName (u));
  END BadFile;

PROCEDURE IfDebug (): BOOLEAN =
  BEGIN
    RETURN Msg.level >= Msg.Level.Debug;
  END IfDebug;

PROCEDURE DoDebug (msg: TEXT) =
  BEGIN
    IF IfDebug () AND msg # NIL THEN Msg.Debug (msg, Wr.EOL); END;
  END DoDebug;

PROCEDURE DebugF (msg0: TEXT;  u: M3Unit.T;  msg1: TEXT := NIL) =
  BEGIN
    IF (Msg.level >= Msg.Level.Debug) THEN
      Msg.Debug (msg0, FName (u), msg1, Wr.EOL);
    END;
  END DebugF;

PROCEDURE ExplainF (msg: TEXT;  u: M3Unit.T) =
  BEGIN
    IF (Msg.level >= Msg.Level.Explain) THEN
      Msg.Explain (msg, M3Unit.FileName (u));
    END;
  END ExplainF;

PROCEDURE VerboseF (msg: TEXT;  u: M3Unit.T) =
  BEGIN
    IF (Msg.level >= Msg.Level.Verbose) THEN
      Msg.Verbose (msg, FName (u));
    END;
  END VerboseF;

PROCEDURE FName (u: M3Unit.T): TEXT =
  BEGIN
    IF (M3Unit.FileName (u) # NIL) AND (u.library # NIL) THEN
      RETURN M3Unit.FileName (u) & " in library " & M3Unit.FullPath (u.library);
    ELSIF (M3Unit.FileName (u) # NIL) THEN
      RETURN M3Unit.FileName (u);
    ELSIF (u.library # NIL) THEN
      RETURN M3Path.Join (u.loc.path, M3ID.ToText (u.name), u.kind)
             & " in library " & M3Unit.FullPath (u.library);
    ELSIF (M3ID.ToText (u.name) # NIL) THEN
      RETURN M3Path.Join (u.loc.path, M3ID.ToText (u.name), u.kind)
    ELSE
      RETURN "???";
    END;
  END FName;

BEGIN
END Builder.
