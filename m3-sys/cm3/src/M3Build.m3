(* Copyright 1996-2000 Critical Mass, Inc. All rights reserved.    *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE M3Build;

IMPORT Env, IntArraySort, IntRefTbl, M3ID, Pathname, Text, TextList, Thread, Wr;
IMPORT Quake, QValue, QCode, QMachine, QVal, QVSeq, QVTbl, M3Timers;
IMPORT Arg, Builder, M3Loc, M3Options, M3Path, M3Unit, Msg, Utils;
FROM QMachine IMPORT PushBool, PushText, PopText, PopID, PopBool;
IMPORT MxConfig;
IMPORT OSError, Process, Dirs, TextUtils;

TYPE
  UK = M3Unit.Kind;
  MM = M3Options.Mode;

REVEAL
  T = Quake.Machine BRANDED "M3Build.T" OBJECT
    (* READONLY state that is fixed for the build *)
    build_pkg         : M3ID.T; (* the name of the package we're building *)
    build_pkg_dir     : M3ID.T; (* full path to the package we're building *)
    build_dir         : M3ID.T; (* name of the derived directory *)
    text_build_dir    : TEXT;   (* " *)

    (* READONLY state that is fixed by the configuration file *)
    install_root      : TEXT;     (* Root directory *)
    pkg_use           : TEXT;     (* Root directory for public packages *)
    pkg_install       : TEXT;     (* Root directory for public packages *)
    bin_install       : TEXT;     (* Directory to install binaries *)
    lib_install       : TEXT;     (* Directory to install libraries *)
    emacs_install     : TEXT;     (* Directory to install emacs e-lisp code *)
    doc_install       : TEXT;     (* Directory to install documents *)
    man_install       : TEXT;     (* Directory to install man pages *)
    html_install      : TEXT;     (* Directory to install HTML files *)

    (* with "forward slash" alternatives for noM3ShipResolution *)
    install_root_alt  : TEXT;    (* Root directory *)
    pkg_use_alt       : TEXT;    (* Root directory for public packages *)
    pkg_install_alt   : TEXT;    (* Root directory for public packages *)
    bin_install_alt   : TEXT;    (* Directory to install binaries *)
    lib_install_alt   : TEXT;    (* Directory to install libraries *)
    emacs_install_alt : TEXT;    (* Directory to install emacs e-lisp code *)
    doc_install_alt   : TEXT;    (* Directory to install documents *)
    man_install_alt   : TEXT;    (* Directory to install man pages *)
    html_install_alt  : TEXT;    (* Directory to install HTML files *)

    have_pkgtools     : BOOLEAN;  (* Using the SRC package tools ? *)
    at_SRC            : BOOLEAN;  (* include SRC-only packages ? *)
    system_libs       : QVTbl.T;  (* Available system libraries *)
    system_liborder   : QVSeq.T;  (* link order for the system libraries *)

    (* major mode, fixed at the beginning of the run *)
    mode              : MM;

    (* per processed package state *)
    cur_pkg           : M3ID.T; (* the name of the package we're processing *)
    cur_pkg_dir       : M3ID.T; (* full path of the package we're processing *)

    (* cache of package name -> directory *)
    pkg_cache         : IntRefTbl.T; (* pkg name -> current path to package *)
    pkg_overrides     : IntRefTbl.T; (* pkg name -> override path *)
    already_warned    : BOOLEAN;     (* warned about overriding build pkg? *)

    (* cache for PathOf() and PkgSubdir() *)
    path_of_path      : TEXT; (* records result of last path() call in PathOf *)
    path_of_base      : TEXT; (* records result of normalize in PathOf *)
    pkg_subdir_path   : TEXT; (* result of last path() call in PkgSubdir *)
    pkg_subdir_base   : TEXT; (* result of last normalize in PkgSubdir *)

    (* maps of imported packages *)
    imports           : IntRefTbl.T;  (* imported package names -> versions *)

    (* various sources *)
    units             : M3Unit.Set;   (* source units *)
    tfile_args        : TxtList;
    derived           : IntRefTbl.T;  (* derived files *)
    sys_libs          : IntRefTbl.T;  (* imported system libraries *)
    ship_units        : M3Unit.Set;   (* explicitly shipped units *)

    (* ship & install cache *)
    listing_width     : INTEGER;
    last_src_dir      : TEXT;
    last_install_dir  : TEXT;
    last_ship_dir     : TEXT;
    all_ship_dirs     : IntRefTbl.T;

    (* current compiler options *)
    current_options   : CompilerOptions;
    build_shared      : BOOLEAN;
  END;

TYPE
  CompilerOptions = REF RECORD
    debug, optimize : BOOLEAN;
    next : CompilerOptions;
  END;

CONST
  M3Exports   = ".M3EXPORTS";   (* file of exported quake commands *)
  M3Ship      = ".M3SHIP";      (* ship commands *)
  M3Overrides = ".M3OVERRIDES"; (* marker to indicate override use *)
  M3Web       = ".M3WEB";       (* compiler support for browser *)
  M3TFile     = ".M3IMPTAB";    (* import table for external compilers *)
  ModeGroupR  = "0644";
  ModeGroupW  = "0664";
  ModeXGroupR = "0755";
  ModeXGroupW = "0775";
  RPCR  = ")" & Wr.EOL;   (* right paren, carriage return *)
  QRPCR = "\")" & Wr.EOL; (* quote, right paren, carriage return *)
  C     = ", ";           (* comma *)
  CQ    = ", \"";         (* comma, quote *)
  QC    = "\", ";         (* quote, comma *)
  QCQ   = "\", \"";       (* quote, comma, quote *)

VAR
  ModeF := ModeGroupR;
  ModeX := ModeXGroupR;

TYPE
  TxtList = RECORD
    head, tail: TextList.T;
  END;

CONST
  IntfExtensions = ARRAY OF TEXT { ".ic", ".is", ".io", "_i.o" };
  ImplExtensions = ARRAY OF TEXT { ".mc", ".ms", ".mo", "_m.o" };
  CExtensions    = ARRAY OF TEXT { ".s", ".o", ".obj" };
  SExtensions    = ARRAY OF TEXT { ".o", ".obj" };
  NoExtension    = ARRAY OF TEXT { "" };

(*-------------------------------------------------- external entry points --*)

PROCEDURE NewMachine (): T =
  VAR
    map := Quake.NewIDMap (Str2ID, Txt2ID, ID2Txt);
    t := NEW (T).init (map);
  BEGIN
    InitBuiltins (t);
    RETURN t;
  END NewMachine;

PROCEDURE SetUp (t: T;  pkg, to_pkg, build_dir: TEXT)
  RAISES {Quake.Error} =
  BEGIN
    (* some more config dependent backward compatibility hacks... *)
    Quake.Define (t, "M3SEARCH_TABLES", "-T" & M3TFile);
    Quake.Define (t, "DEFAULT_BUILD_DIR", GetConfig (t, "BUILD_DIR"));
    Quake.Define (t, "M3", M3Path.New (GetConfigPath (t, "BIN_USE"), "cm3"));
    Quake.Define (t, "PACKAGE_DIR", pkg);

    t.build_pkg       := M3ID.Add (Pathname.Last (pkg));
    t.build_pkg_dir   := M3ID.Add (pkg);
    t.build_dir       := M3ID.Add (build_dir);
    t.text_build_dir  := build_dir;

    t.pkg_use         := GetConfigPath (t, "PKG_USE");
    t.pkg_install     := GetConfigPath (t, "PKG_INSTALL");
    t.install_root    := GetConfigPath (t, "INSTALL_ROOT");
    t.bin_install     := GetConfigPath (t, "BIN_INSTALL");
    t.lib_install     := GetConfigPath (t, "LIB_INSTALL");
    t.emacs_install   := GetConfigPath (t, "EMACS_INSTALL");
    t.doc_install     := GetConfigPath (t, "DOC_INSTALL");
    t.man_install     := GetConfigPath (t, "MAN_INSTALL");
    t.html_install    := GetConfigPath (t, "HTML_INSTALL");
    t.have_pkgtools   := GetConfigBool (t, "HAVE_PKGTOOLS");
    t.at_SRC          := GetConfigBool (t, "AT_SRC");
    t.system_liborder := QVal.ToArray (t, ConfigDefn (t, "SYSTEM_LIBORDER").value);
    t.system_libs     := QVal.ToTable (t, ConfigDefn (t, "SYSTEM_LIBS").value);

    (* alternate path form for noM3ShipResolution
     * Multiple options here:
     * We could replace \ with /.
     * We could replace M3Path.SlashText with /.
     * We could replace M3Path.SlashText with \.
     * We could replace \ with M3Path.SlashText.
     * We could replace / with M3Path.SlashText.
     * We could even replace / with \.
     * Or use some other strange value for the replacement (>255?).
     * M3Path.SlashText is \ or /, depending on the host (not the target).
     * We just have to be consistent with DoUnresolve, and we should be aware
     * that certain options disallow certain usages, for example if we
     * unconditionally replace \ with /, then Posix users can't use \ as a
     * "normal" character in their paths. Of course that is problematic anyway,
     * since it will be easily confused as an escape character and the usage
     * won't be portable, so ok. The limitation also only holds if you use
     * noM3ShipResolution, though it is also reasonable to make that the default
     * and only option.
     *)

    t.pkg_use_alt       := TextUtils.Substitute(t.pkg_use, "\\", "/");
    t.pkg_install_alt   := TextUtils.Substitute(t.pkg_install, "\\", "/");
    t.install_root_alt  := TextUtils.Substitute(t.install_root, "\\", "/");
    t.bin_install_alt   := TextUtils.Substitute(t.bin_install, "\\", "/");
    t.lib_install_alt   := TextUtils.Substitute(t.lib_install, "\\", "/");
    t.emacs_install_alt := TextUtils.Substitute(t.emacs_install, "\\", "/");
    t.doc_install_alt   := TextUtils.Substitute(t.doc_install, "\\", "/");
    t.man_install_alt   := TextUtils.Substitute(t.man_install, "\\", "/");
    t.html_install_alt  := TextUtils.Substitute(t.html_install, "\\", "/");

    t.cur_pkg         := t.build_pkg;
    t.cur_pkg_dir     := t.build_pkg_dir;

    t.pkg_cache       := NewMap ();
    t.pkg_overrides   := NewMap ();
    t.already_warned  := FALSE;
    EVAL t.pkg_cache.put (t.build_pkg, to_pkg);

    t.path_of_path    := "";
    t.path_of_base    := "";
    t.pkg_subdir_path := "";
    t.pkg_subdir_base := "";

    t.listing_width   := 0;
    t.last_src_dir    := "";
    t.last_install_dir:= "";
    t.last_ship_dir   := "";
    t.all_ship_dirs   := NewMap ();

    t.current_options := NEW (CompilerOptions);

    InitGlobals (t);
  END SetUp;

PROCEDURE Run (t: T;  makefile: TEXT)
  RAISES {Quake.Error, Thread.Alerted} =
  BEGIN
    IF groupWritable THEN
      ModeF := ModeGroupW;
      ModeX := ModeXGroupW;
    END;
    t.mode := M3Options.major_mode;

    IF (t.mode = MM.Build) THEN
      (* let m3ship know that we've built at least once... *)
      Utils.Remove (M3Ship);
      Utils.Remove (M3Overrides);
      TouchFile (M3Ship);
    END;
    DeleteDeriveds (t, M3Ship, NoExtension);
    DeleteDeriveds (t, M3Overrides, NoExtension);

    Quake.Run (t, makefile);
    MakeRoom (t, 999999);  (* finish any partial listings *)
  END Run;

PROCEDURE RealClean () =
  VAR
    targetdir, base, parent: Pathname.T;
  BEGIN
    (* We are already in the TARGET build directory *)
    TRY
      targetdir := Process.GetWorkingDirectory ();
    EXCEPT OSError.E(ec) =>
      Msg.FatalError (ec, "unable to get full directory path: ", targetdir);
    END;
    base   := Pathname.Last (targetdir);
    parent := Pathname.Prefix (targetdir);
    TRY
      Process.SetWorkingDirectory (parent);
    EXCEPT OSError.E(ec) =>
      Msg.FatalError (ec, "unable to move to directory: ", parent);
    END;
    Dirs.RemoveRecursively(base);
  END RealClean;

PROCEDURE NewMap (): IntRefTbl.T =
  BEGIN
    RETURN NEW (IntRefTbl.Default).init ();
  END NewMap;

(*-------------------------------------------- "global" var initialization --*)

PROCEDURE InitGlobals (t: T) =
  (* initializes the global state for a new build.  That is, forget
     everything we know about source files and imports... *)
  BEGIN
    t.imports := NewMap ();
    M3Unit.InitSet (t.units);
    InitTxtList (t.tfile_args);
    t.derived := NewMap ();
    t.sys_libs := NewMap ();
    M3Unit.InitSet (t.ship_units);
    EVAL t.sys_libs.put (M3ID.Add ("LIBC"), NIL);
    t.current_options.debug := FALSE;
    t.current_options.optimize := FALSE;
    t.build_shared := TRUE;
  END InitGlobals;

(*------------------------------------------------------------------ units --*)

PROCEDURE AddSource (t: T;  nm: M3ID.T;  kind: UK;  hidden: BOOLEAN)
  RAISES {Quake.Error} =
  VAR
    name := M3ID.ToText (nm);
    dir  := M3Path.New (PkgSubdir (t), Pathname.Prefix (name));
    base := M3ID.Add (Pathname.Last (name));
    loc  := Location (t, t.cur_pkg, M3ID.Add (dir));
    unit := M3Unit.New (base, kind, loc, hidden := hidden, imported := FALSE);
  BEGIN
    unit.debug := t.current_options.debug;
    unit.optimize := t.current_options.optimize;
    M3Unit.Add (t.units, unit);
    DeleteObjects (t, base, kind);
  END AddSource;

PROCEDURE AddDerived (t: T;  name: M3ID.T;  kind: UK;  hidden: BOOLEAN) =
  VAR
    loc  := Location (t, t.build_pkg, t.build_dir);
    unit := M3Unit.New (name, kind, loc, hidden := hidden, imported := FALSE);
    file := M3Unit.FileName (unit);
  BEGIN
    unit.debug := t.current_options.debug;
    unit.optimize := t.current_options.optimize;
    EVAL t.derived.put (M3ID.Add (file), NIL);
    M3Unit.Add (t.units, unit);
    DeleteDeriveds (t, file, NoExtension);
    DeleteObjects (t, name, kind);
  END AddDerived;

PROCEDURE DeleteObjects (t: T;  nm: M3ID.T;  kind: UK) =
  VAR name := M3ID.ToText (nm);
  BEGIN
    CASE kind OF
    | UK.I3 => DeleteDeriveds (t, name, IntfExtensions);
    | UK.M3 => DeleteDeriveds (t, name, ImplExtensions);
    | UK.C  => DeleteDeriveds (t, name, CExtensions);
    | UK.S  => DeleteDeriveds (t, name, SExtensions);
    ELSE (* skip *)
    END;
  END DeleteObjects;

(*------------------------------------------------------------- text lists --*)

PROCEDURE InitTxtList (VAR x: TxtList) =
  BEGIN
    x.head := NIL;  x.tail := NIL;
  END InitTxtList;

PROCEDURE AddText (VAR x: TxtList;  txt: TEXT) =
  VAR z := NEW (TextList.T, head := txt, tail := NIL);
  BEGIN
    IF (x.head = NIL)
      THEN x.head      := z;
      ELSE x.tail.tail := z;
    END;
    x.tail := z;
  END AddText;

PROCEDURE AddTexts (VAR x: TxtList;  a, b, c: TEXT := NIL) =
  BEGIN
    IF (a # NIL) THEN AddText (x, a); END;
    IF (b # NIL) THEN AddText (x, b); END;
    IF (c # NIL) THEN AddText (x, c); END;
  END AddTexts;

(*------------------------------------------------------ builtin functions --*)

TYPE
  Builtin = RECORD
    name   : TEXT;
    proc   : QCode.BuiltinProc;
    n_args : INTEGER;
    isFunc : BOOLEAN;
  END;

CONST
  Builtins = ARRAY OF Builtin {
(*------------------------------------------------- feature/version probes --*)
    Builtin {"HasCBackend",                   HasCBackend,       0, TRUE },
    (* packages & locations *)
    Builtin {"Pkg",                           DoPkg,             1, TRUE},
    Builtin {"override",                      DoOverride,        2, FALSE},
    Builtin {"path_of",                       DoPathOf,          1, TRUE},
    Builtin {"pkg_subdir",                    DoPkgSubdir,       0, TRUE},

    (* names *)
    Builtin {"program_name",                  DoProgramName,     1, TRUE},
    Builtin {"library_name",                  DoLibraryName,     1, TRUE},

    (* calls in the generated .M3EXPORT files *)
    Builtin {"_define_lib",                   DoDefineLib,       1, FALSE},
    Builtin {"_define_pgm",                   DoDefinePgm,       1, FALSE},
    Builtin {"_import_template",              DoImportTmpl,      3, FALSE},
    Builtin {"_import_m3lib",                 DoImportM3Lib,     3, FALSE},
    Builtin {"_import_otherlib",              DoImportOLib,      3, FALSE},
    Builtin {"_map_add_interface",            DoMapInterface,    4, FALSE},
    Builtin {"_map_add_generic_interface",    DoMapGenIntf,      4, FALSE},
    Builtin {"_map_add_module",               DoMapModule,       4, FALSE},
    Builtin {"_map_add_generic_module",       DoMapGenImpl,      4, FALSE},
    Builtin {"_map_add_c",                    DoMapCSource,      4, FALSE},
    Builtin {"_map_add_h",                    DoMapHSource,      4, FALSE},
    Builtin {"_map_add_s",                    DoMapSSource,      4, FALSE},
    
    (* compiler options *)
    Builtin {"m3_debug",                      DoDebug,           1, FALSE},
    Builtin {"m3_optimize",                   DoOptimize,        1, FALSE},
    Builtin {"build_shared",                  DoShared,          0, FALSE},
    Builtin {"build_standalone",              DoStandalone,      0, FALSE},

    (* derived files *)
    Builtin {"m3_compile_only",               DoCompileOnly,     0, FALSE},
    Builtin {"m3_finish_up",                  DoFinishUp,        0, FALSE},

    (* predefined system libraries *)
    Builtin {"import_sys_lib",                DoImportSysLib,    1, FALSE},

    (* options *)
    Builtin {"m3_option",                     DoM3Option,        1, FALSE},
    Builtin {"remove_m3_option",              DoRemoveM3Option,  1, FALSE},

    (* deleting *)
    Builtin {"deriveds",                      DoDeriveds,        2, FALSE},

    (* imports *)
    Builtin {"include_dir",                   DoIncludeDir,      1, FALSE},
    Builtin {"include_pkg",                   DoIncludePkg,      1, FALSE},
    Builtin {"import",                        DoImport,          1, FALSE},
    Builtin {"import_version",                DoImportVersion,   2, FALSE},
    Builtin {"import_obj",                    DoImportObj,       1, FALSE},
    Builtin {"import_lib",                    DoImportLib,       2, FALSE},

    (* objects *)
    Builtin {"pgm_object",                    DoPgmObject,       2, FALSE},

    (* sources *)
    Builtin {"source",                        DoSource,          1, FALSE},
    Builtin {"pgm_source",                    DoPgmSource,       1, FALSE},
    Builtin {"interface",                     DoIntf,            1, FALSE},
    Builtin {"Interface",                     DoIntfX,           1, FALSE},
    Builtin {"implementation",                DoImpl,            1, FALSE},
    Builtin {"module",                        DoModule,          1, FALSE},
    Builtin {"Module",                        DoModuleX,         1, FALSE},
    Builtin {"h_source",                      DoHSource,         1, FALSE},
    Builtin {"c_source",                      DoCSource,         1, FALSE},
    Builtin {"s_source",                      DoSSource,         1, FALSE},
    Builtin {"ship_source",                   DoShipSource,      1, FALSE},

    (* generics *)
    Builtin {"generic_interface",             DoGenIntf,         1, FALSE},
    Builtin {"Generic_interface",             DoGenIntfX,        1, FALSE},
    Builtin {"generic_implementation",        DoGenImpl,         1, FALSE},
    Builtin {"Generic_implementation",        DoGenImplX,        1, FALSE},
    Builtin {"generic_module",                DoGenModule,       1, FALSE},
    Builtin {"Generic_module",                DoGenModuleX,      1, FALSE},
    Builtin {"build_generic_intf",            DoBuildGenIntf,    4, FALSE},
    Builtin {"build_generic_impl",            DoBuildGenImpl,    3, FALSE},
 
    (* derived sources *)
    Builtin {"derived_interface",             DoDerivedIntf,     2, FALSE},
    Builtin {"derived_implementation",        DoDerivedImpl,     1, FALSE},
    Builtin {"derived_c",                     DoDerivedC,        1, FALSE},
    Builtin {"derived_h",                     DoDerivedH,        1, FALSE},

    (* hiding/exporting *)
    Builtin {"hide_interface",                DoHideIntf,        1, FALSE},
    Builtin {"hide_generic_interface",        DoHideGenIntf,     1, FALSE},
    Builtin {"hide_generic_implementation",   DoHideGenImpl,     1, FALSE},
    Builtin {"export_interface",              DoExportIntf,      1, FALSE},
    Builtin {"export_generic_interface",      DoExportGenIntf,   1, FALSE},
    Builtin {"export_generic_implementation", DoExportGenImpl,   1, FALSE},

    (* templates *)
    Builtin {"template",                      DoTemplate,        1, FALSE},

    (* library building *)
    Builtin {"library",                       DoLibrary,         1, FALSE},
    Builtin {"Library",                       DoLibrary,         1, FALSE},

    (* program building *)
    Builtin {"program",                       DoProgram,         1, FALSE},
    Builtin {"Program",                       DoProgramX,        1, FALSE},
    Builtin {"c_program",                     DoCProgram,        1, FALSE},
    Builtin {"C_program",                     DoCProgramX,       1, FALSE},

    (* man pages *)
    Builtin {"manPage",                       DoManPage,         2, FALSE},
    Builtin {"ManPage",                       DoManPageX,        2, FALSE},

    (* emacs *)
    Builtin {"Gnuemacs",                      DoGnuEmacs,        1, FALSE},
    Builtin {"CompiledGnuemacs",              DoCompiledEmacs,   1, FALSE},

    (* "-find" support *)
    Builtin {"find_unit",                     DoFindUnit,        2, FALSE},
    Builtin {"enum_units",                    DoEnumUnits,       1, FALSE},
    
    (* export functions *)
    Builtin {"install_sources",               DoInstallSources,  0, FALSE},
    Builtin {"install_derived",               DoInstallDerived,  1, FALSE},
    Builtin {"install_derived_link",          DoInstallDerivedSymbolicLink,   2, FALSE},
    Builtin {"install_derived_symbolic_link", DoInstallDerivedSymbolicLink,   2, FALSE},
    Builtin {"install_derived_hard_link",     DoInstallDerivedHardLink,   2, FALSE},
    Builtin {"install_link_to_derived",       DoInstallSymbolLinkToDerived, 2, FALSE},
    Builtin {"install_symbolic_link_to_derived", DoInstallSymbolLinkToDerived, 2, FALSE},
    Builtin {"install_hard_link_to_derived",  DoInstallHardLinkToDerived, 2, FALSE},
    Builtin {"install_symbolic_link",         DoInstallSymbolLink, 2, FALSE},
    Builtin {"install_file",                  DoInstallFile,     3, FALSE},

    (* installation functions *)
    Builtin {"BindExport",                    DoBindExport,      1, FALSE},
    Builtin {"BinExport",                     DoBinExport,       1, FALSE},
    Builtin {"LibdExport",                    DoLibdExport,      1, FALSE},
    Builtin {"LibExport",                     DoLibExport,       1, FALSE},
    Builtin {"EmacsdExport",                  DoEmacsdExport,    1, FALSE},
    Builtin {"EmacsExport",                   DoEmacsExport,     1, FALSE},
    Builtin {"DocdExport",                    DoDocdExport,      1, FALSE},
    Builtin {"DocExport",                     DoDocExport,       1, FALSE},
    Builtin {"MandExport",                    DoMandExport,      2, FALSE},
    Builtin {"ManExport",                     DoManExport,       2, FALSE},
    Builtin {"HtmlExport",                    DoHtmlExport,      1, FALSE},
    Builtin {"RootExport",                    DoRootExport,      2, FALSE},
    Builtin {"RootdExport",                   DoRootdExport,     2, FALSE},

    (* misc *)
    Builtin {"gen_m3exports",                 DoGenM3Exports,    1, FALSE},
    Builtin {"generate_tfile",                DoGenTFile,        0, FALSE},
    Builtin {"delete_file",                   DoDeleteFile,      1, FALSE},
    Builtin {"link_file",                     DoSymbolicLinkFile, 2, FALSE},
    Builtin {"symbolic_link_file",            DoSymbolicLinkFile, 2, FALSE},
    Builtin {"hard_link_file",                DoHardLinkFile,    2, FALSE}
  };

PROCEDURE InitBuiltins (t: T) =
  VAR v: QValue.T;  info: QCode.ProcInfo;  bind: QValue.Binding;
  BEGIN
    TRY
      FOR i := FIRST (Builtins) TO LAST (Builtins) DO
        WITH z = Builtins[i] DO
          info         := NEW (QCode.ProcInfo);
          info.name    := M3ID.Add (z.name);
          info.n_args  := z.n_args;
          info.builtin := TRUE;
          info.isFunc  := z.isFunc;
          info.handler := z.proc;
  
          v.kind := QValue.Kind.Proc;
          v.ref  := NEW (QValue.Proc, info := info, env := NIL);
          v.int  := 0;
          t.put (info.name, v);
  
          (* make the definition "readonly" *)
          bind := t.lookup (info.name);
          bind.readonly := TRUE;
        END;
      END;

      (* defined for backward compatibility *)
      Quake.Define (t, "LOCAL",    "local");
      Quake.Define (t, "IMPORTED", "");
      Quake.Define (t, "HIDDEN",   "hidden");
      Quake.Define (t, "VISIBLE",  "");
      Quake.Define (t, "CR",       Wr.EOL);
      Quake.Define (t, "SL",       M3Path.SlashText);

      DefineArray (t, "intf_extensions", IntfExtensions);
      DefineArray (t, "impl_extensions", ImplExtensions);
      DefineArray (t, "c_extensions",    CExtensions);
      DefineArray (t, "s_extensions",    SExtensions);
      DefineArray (t, "no_extension",    NoExtension);

    EXCEPT Quake.Error (msg) =>
      Msg.FatalError (NIL, "unable to define M3/quake builtins: ", msg);
    END;
  END InitBuiltins;

PROCEDURE DefineArray (t: T;  nm: TEXT;  READONLY arr: ARRAY OF TEXT)
  RAISES {Quake.Error} =
  VAR v: QValue.T;  seq := NEW (QVSeq.T).init (NUMBER (arr));
  BEGIN
    FOR i := FIRST (arr) TO LAST (arr) DO
      v.kind := QValue.Kind.String;
      v.int  := M3ID.Add (arr[i]);
      v.ref  := NIL;
      seq.addhi (v);
    END;
    v.kind := QValue.Kind.Array;
    v.int  := 0;
    v.ref  := seq;
    t.put (M3ID.Add (nm), v);
  END DefineArray;

(*------------------------------------------------- feature/version probes --*)

PROCEDURE HasCBackend (m: QMachine.T;  <*UNUSED*> n_args: INTEGER) =
  VAR t := Self (m);
  BEGIN
    PushBool (t, TRUE);
  END HasCBackend;

(*------------------------------------------------------ package locations --*)

PROCEDURE DoPkg (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    PushText (t, Pkg (t, PopID (t)));
  END DoPkg;

PROCEDURE Pkg (t: T;  nm: M3ID.T): TEXT =
  (* returns the path currently used to reach package 'x' *)
  VAR ref: REFANY;  dir: TEXT;
  BEGIN
    IF    t.pkg_cache.get (nm, ref)     THEN  RETURN NARROW (ref, TEXT);
    ELSIF t.pkg_overrides.get (nm, ref) THEN  dir := NARROW (ref, TEXT);
    ELSE                                      dir := t.pkg_use;
    END;
    dir := M3Path.New (dir, M3ID.ToText (nm));
    EVAL t.pkg_cache.put (nm, dir);
    RETURN dir;
  END Pkg;

PROCEDURE DoOverride (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  pkg: M3ID.T;  dir: TEXT;
  BEGIN
    dir := PopText (t);
    pkg := PopID (t);
    Override (t, pkg, dir);
  END DoOverride;

(* Host and Target paths sometimes get confused, leading to many warnings such
   as: package "libm3" is already overridden to C:\dev2\cm3.2/m3-libs,
   ignoring new override to C:/dev2/cm3.2/m3-libs. PathEqual is defined as
   Text.Equal or Text.Equal(replacing backward slashes with forward slashes).
   It would also be reasonable to be case insensitive if the path contains any
   backward slashes, or if they both contain colon as the second character. *)
PROCEDURE OverrideEqual(a: TEXT; b: TEXT): BOOLEAN =
  BEGIN
    IF Text.Length(a) # Text.Length(b) THEN
      RETURN FALSE;
    END;
    IF Text.Equal(a, b) THEN
      RETURN TRUE;
    END;
    IF (Text.FindChar(a, '\\') = -1) AND (Text.FindChar(b, '\\') = -1) THEN
      RETURN FALSE;
    END;
    a := TextUtils.SubstChar(a, '\\', '/');
    b := TextUtils.SubstChar(b, '\\', '/');
    RETURN Text.Equal(a, b);
  END OverrideEqual;

PROCEDURE Override (t: T;  pkg: M3ID.T;  dir: TEXT) =
  (* establish an override for the location of package "pkg" *)
  VAR  ref: REFANY;  pkg_txt := M3ID.ToText (pkg);
  BEGIN
    IF t.build_pkg = pkg THEN
      IF t.already_warned THEN RETURN; END;
      IF M3Options.major_mode = MM.Depend THEN
        Msg.Verbose ("ignoring override(\"", pkg_txt, QCQ, dir, QRPCR);
      ELSE
        Msg.Info ("ignoring override(\"", pkg_txt, QCQ, dir, QRPCR);
      END;
      t.already_warned := TRUE;
    ELSIF t.pkg_overrides.get (pkg, ref) THEN
      IF NOT OverrideEqual (dir, ref) THEN
        IF M3Options.major_mode = MM.Depend THEN
          Msg.Verbose ("package \"", pkg_txt, "\" is already overridden to ",
                       ref, ", ignoring new override to " & dir & Wr.EOL);
        ELSE
          Msg.Info ("package \"", pkg_txt, "\" is already overridden to ",
                    ref, ", ignoring new override to ", dir & Wr.EOL);
        END;
      END;
    ELSE
      EVAL t.pkg_overrides.put (pkg, dir);
      EVAL t.pkg_cache.put (pkg, M3Path.New (dir, pkg_txt));
    END;
  END Override;

PROCEDURE Include (t: T;  file: TEXT)
  RAISES {Quake.Error, Thread.Alerted} =
  BEGIN
    (* We cannot be sure that _any_ packages are there when we compute
       the package dependencies. If an appropriate .M3EXPORTS file cannot
       be found, we simply create a dummy unit representing the package
       import. *)
    IF t.mode # MM.Depend OR Utils.IsFile(file) THEN
      t.include (file);
    ELSE
      Msg.Debug ("simulating inclusion of ", file, Wr.EOL);
      VAR
        lib, pkg, subdir, last: TEXT;
        pkg_id, subdir_id, lib_id: M3ID.T;
        loc: M3Loc.T;
      BEGIN
        IF file # NIL THEN
          last := Pathname.Last(file);
          IF last # NIL AND Text.Equal(last, ".M3EXPORTS") THEN
            file := Pathname.Prefix(file);
            subdir := Pathname.Last(file);
            file := Pathname.Prefix(file);
            pkg := Pathname.Last(file);
            pkg_id := M3ID.Add(pkg);
            subdir_id := M3ID.Add(subdir);
            lib := pkg & "_unknown";
            lib_id := M3ID.Add(lib);
            loc := Location (t, pkg_id, subdir_id);
            M3Unit.AddNew (t.units, lib_id, UK.M3LIB, loc, hidden := TRUE,
                           imported := TRUE); 
          END;
        END;
      END;
    END;
  END Include;

(*------------------------------------------------------ general locations --*)

PROCEDURE Location (t: T;  pkg, subdir: M3ID.T): M3Loc.T =
  (* Return the full path that identifies the given subdirectory
     within the package. *)
  VAR pkg_dir: TEXT;
  BEGIN
    IF (pkg = M3Loc.noPkg)
      THEN pkg_dir := NIL;
      ELSE pkg_dir := Pkg (t, pkg);
    END;
    RETURN M3Loc.New (pkg, subdir, pkg_dir);
  END Location;

(*--------------------------------------------------------- relative paths --*)

PROCEDURE DoPathOf (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    PushText (t, PathOf (t, PopText (t)));
  END DoPathOf;

PROCEDURE PathOf (t: T;  x: TEXT): TEXT
  (* Returns the path needed to reach 'x' from the build directory *)
  RAISES {Quake.Error} =
  VAR ref: REFANY;  p: TEXT;
  BEGIN
    IF t.derived.get (M3ID.Add (x), ref) THEN
      (* This initial lookup in derived_sources allows packages (such as
      ** netobj) that were written without derived sources in mind to work
      ** without modification.
      *)
      RETURN x;
    END;

    p := t.cur_path ();
    IF NOT Text.Equal (p, t.path_of_path) THEN
      t.path_of_path := p;
      t.path_of_base := M3Path.New (Pkg (t, t.cur_pkg), PkgSubdir (t));
    END;
    RETURN M3Path.New (t.path_of_base, x);
  END PathOf;

PROCEDURE DoPkgSubdir (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    PushText (t, PkgSubdir (t));
  END DoPkgSubdir;

PROCEDURE PkgSubdir (t: T): TEXT
  (* returns the path needed to reach the current directory from
     the current package *)
  RAISES {Quake.Error} =
  VAR p := t.cur_path ();
  BEGIN
    IF NOT Text.Equal (p, t.pkg_subdir_path) THEN
      t.pkg_subdir_path := p;
      t.pkg_subdir_base := t.normalize (M3ID.ToText (t.cur_pkg_dir),
                                        t.pkg_subdir_path);
    END;
    RETURN t.pkg_subdir_base;
  END PkgSubdir;

(*------------------------------------------------------------------ names --*)

PROCEDURE DoProgramName (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    PushText (t, M3Path.ProgramName (PopText (t)));
  END DoProgramName;

PROCEDURE DoLibraryName (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    PushText (t, M3Path.LibraryName (PopText (t)));
  END DoLibraryName;

(*------------------------------------- calls used in generated files only --*)

PROCEDURE DoDefineLib (<*UNUSED*> m: QMachine.T;  <*UNUSED*> n_args: INTEGER) =
  BEGIN
  END DoDefineLib;

PROCEDURE DoDefinePgm (<*UNUSED*> m: QMachine.T;  <*UNUSED*> n_args: INTEGER) =
  BEGIN
  END DoDefinePgm;

PROCEDURE DoImportTmpl (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error, Thread.Alerted} =
  VAR t := Self (m);  pkg: M3ID.T;  file, subdir: TEXT;
  BEGIN
    subdir := PopText (t);
    pkg    := PopID (t);
    file   := PopText (t);
    Include (t, M3Path.New (Pkg (t, pkg), subdir, file));
  END DoImportTmpl;

PROCEDURE DoImportM3Lib (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  lib, pkg, subdir: M3ID.T;  loc: M3Loc.T;
  BEGIN
    subdir := PopID (t);
    pkg    := PopID (t);
    lib    := PopID (t);
    loc    := Location (t, pkg, subdir);
    M3Unit.AddNew (t.units, lib, UK.M3LIB, loc, hidden := TRUE, imported := TRUE); 
  END DoImportM3Lib;

PROCEDURE DoImportOLib (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  lib, path: M3ID.T;  imported: BOOLEAN;
  BEGIN
    imported := PopBool (t);
    path     := PopID (t);
    lib      := PopID (t);
    ImportOtherLib (t, lib, path, imported);
  END DoImportOLib;

PROCEDURE ImportOtherLib (t: T;  lib, path: M3ID.T;  imported: BOOLEAN) =
  VAR loc := Location (t, M3Loc.noPkg, path);
  BEGIN
    M3Unit.AddNew (t.units, lib, UK.LIB, loc,
                   hidden := FALSE, imported := imported);
  END ImportOtherLib;

PROCEDURE DoMapInterface (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  BEGIN
    MapSource (Self (m), UK.I3);
  END DoMapInterface;

PROCEDURE DoMapGenIntf (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  BEGIN
    MapSource (Self (m), UK.IG);
  END DoMapGenIntf;

PROCEDURE DoMapModule (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  BEGIN
    MapSource (Self (m), UK.M3);
  END DoMapModule;

PROCEDURE DoMapGenImpl (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  BEGIN
    MapSource (Self (m), UK.MG);
  END DoMapGenImpl;

PROCEDURE DoMapCSource (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  BEGIN
    MapSource (Self (m), UK.C);
  END DoMapCSource;

PROCEDURE DoMapHSource (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  BEGIN
    MapSource (Self (m), UK.H);
  END DoMapHSource;

PROCEDURE DoMapSSource (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  BEGIN
    MapSource (Self (m), UK.S);
  END DoMapSSource;

PROCEDURE MapSource (t: T;  kind: UK)
  RAISES {Quake.Error} =
  VAR name, pkg, subdir: M3ID.T;  vis: BOOLEAN;  loc: M3Loc.T;  zz: M3Path.T;
  BEGIN
    vis    := PopBool (t);
    subdir := PopID (t);
    pkg    := PopID (t);
    name   := PopID (t);
    loc    := Location (t, pkg, subdir);
    zz     := M3Path.Parse (M3ID.ToText (name));
    IF (zz.kind # kind) THEN
      Msg.FatalError (NIL, "imported file extension doesn't match expected type: ",
                      M3ID.ToText (name));
    END;
    M3Unit.AddNew (t.units, M3ID.Add (zz.base), kind, loc,
                   hidden := vis, imported := TRUE);
  END MapSource;

(*------------------------------------------------------------------ misc ---*)

PROCEDURE DoDebug (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    t.current_options.debug := PopBool (t);
  END DoDebug;

PROCEDURE DoOptimize (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    t.current_options.optimize := PopBool (t);
  END DoOptimize;

PROCEDURE DoShared (m: QMachine.T;  <*UNUSED*> n_args: INTEGER) =
  VAR t := Self (m);
  BEGIN
    t.build_shared := TRUE;
  END DoShared;

PROCEDURE DoStandalone (m: QMachine.T;  <*UNUSED*> n_args: INTEGER) =
  VAR t := Self (m);
  BEGIN
    t.build_shared := FALSE;
  END DoStandalone;

PROCEDURE DoCompileOnly (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    GenM3Exports (t, "%% compile only");
    IF (t.mode = MM.Build) THEN
      Builder.JustCompile (t.units, SysLibs (t), t);
      InstallDerived (t, M3Web);
      InstallSources (t);
    END;
    DeleteDeriveds (t, M3Web, NoExtension);
    InitGlobals (t);  (* forget about the accumulated sources... *)
  END DoCompileOnly;

PROCEDURE DoFinishUp (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    IF (t.units.head # NIL) THEN
      GenM3Exports (t, "%% finish up");
      IF (t.mode = MM.Build) THEN
        Builder.JustCompile (t.units, SysLibs (t), t);
        InstallDerived (t, M3Web);
        InstallSources (t);
      END;
      DeleteDeriveds (t, M3Web, NoExtension);
      InitGlobals (t);  (* forget about the accumulated sources... *)
    END;
  END DoFinishUp;

(*-------------------------------------------- predefined system libraries --*)

PROCEDURE DoImportSysLib (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);  val: QValue.T;
  BEGIN
    IF NOT t.system_libs.get (nm, val) THEN
      ConfigErr (t, "SYSTEM_LIBS", "does not define a value for \""
                   & M3ID.ToText (nm) & "\"");
    END;
    IF NOT t.sys_libs.put (nm, NIL) THEN
      AddTexts (t.tfile_args, "import_sys_lib(\"", M3ID.ToText (nm), QRPCR);
    END;
  END DoImportSysLib;

PROCEDURE SysLibs (t: T): Arg.List
  RAISES {Quake.Error} =
  VAR
    libs := Arg.NewList ();
    ref  : REFANY;
    val  : QValue.T;
    args : QVSeq.T;
    nm   : M3ID.T;
  BEGIN
    FOR i := 0 TO t.system_liborder.size() - 1 DO
      nm := M3ID.Add (QVal.ToText (t, t.system_liborder.get (i)));
      IF t.sys_libs.get (nm, ref) AND t.system_libs.get (nm, val) THEN
        args := QVal.ToArray (t, val);
        FOR i := 0 TO args.size() - 1 DO
          Arg.Append (libs, QVal.ToText (t, args.get (i)));
        END;
      END;
    END;
    RETURN libs;
  END SysLibs;

(*---------------------------------------------------------------- options --*)
(* These are hacks to provide some backward compatibility with the old m3build *)

PROCEDURE DoM3Option (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  arg: TEXT;
  BEGIN
    arg := PopText (t);
    IF    Text.Equal (arg, "-silent")   THEN  Msg.SetLevel (Msg.Level.Silent);
    ELSIF Text.Equal (arg, "-why")      THEN  Msg.SetLevel (Msg.Level.Explain);
    ELSIF Text.Equal (arg, "-explain")  THEN  Msg.SetLevel (Msg.Level.Explain);
    ELSIF Text.Equal (arg, "-commands") THEN  Msg.SetLevel (Msg.Level.Commands);
    ELSIF Text.Equal (arg, "-verbose")  THEN  Msg.SetLevel (Msg.Level.Verbose);
    ELSIF Text.Equal (arg, "-debug")    THEN  Msg.SetLevel (Msg.Level.Debug);
    ELSIF Text.Equal (arg, "-times")     THEN
      M3Timers.Start ();
    ELSIF Text.Equal (arg, "-keep")     THEN
      Quake.Define (t, "M3_KEEP_FILES", "TRUE");
    ELSIF Text.Equal (arg, "-gui")     THEN
      Quake.Define (t, "M3_WINDOWS_GUI", "TRUE");
    ELSIF Text.Equal (arg, "-windows")     THEN
      Quake.Define (t, "M3_WINDOWS_GUI", "TRUE");
    ELSE  Msg.Error (NIL, "unsupported m3_option value: \"", arg, "\"");
    END;
  END DoM3Option;

PROCEDURE DoRemoveM3Option (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  arg: TEXT;
  BEGIN
    arg := PopText (t);
    Msg.Info ("remove_m3_option (\"", arg, "\") is being ignored.", Wr.EOL)
  END DoRemoveM3Option;

(*--------------------------------------------------------------- deleting --*)
(* We don't support "-clean".... *)

PROCEDURE DoDeriveds (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);   base: TEXT;  exts: QVSeq.T;
  BEGIN
    exts := PopArray (t);
    base := PopText (t);
    IF (t.mode = MM.Clean) THEN
      DeleteDeriveds (t, base, SeqToTexts (t, exts)^);
    END;
  END DoDeriveds;

PROCEDURE DeleteDeriveds (t: T;  base: TEXT;   READONLY exts: ARRAY OF TEXT) =
  BEGIN
    IF (t.mode = MM.Clean) THEN
      FOR i := FIRST (exts) TO LAST (exts) DO
        Utils.Remove (base & exts[i]);
      END;
    END;
  END DeleteDeriveds;

(*---------------------------------------------------------------- imports --*)

PROCEDURE DoIncludeDir (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error, Thread.Alerted} =
  VAR t := Self (m);  dir: TEXT;
  BEGIN
    dir := PopText (t);
    Include (t, M3Path.New (t.cur_path(), dir, "m3makefile"));
  END DoIncludeDir;

PROCEDURE DoIncludePkg (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error, Thread.Alerted} =
  VAR t := Self (m);  pkg, save_pkg, save_dir: M3ID.T;
  BEGIN
    pkg := PopID (t);

    save_pkg := t.cur_pkg;
    save_dir := t.cur_pkg_dir;
    t.cur_pkg     := pkg;
    t.cur_pkg_dir := M3ID.Add (Pkg (t, pkg));
    TRY
      Include (t, M3Path.New (M3ID.ToText (t.cur_pkg_dir), "src", "m3makefile"));
    FINALLY
      t.cur_pkg     := save_pkg;
      t.cur_pkg_dir := save_dir;
    END;
  END DoIncludePkg;

PROCEDURE DoImport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error, Thread.Alerted} =
  VAR t := Self (m);
  BEGIN
    ImportVersion (t, PopID (t), t.build_dir);
  END DoImport;

PROCEDURE DoImportVersion (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error, Thread.Alerted} =
  VAR t := Self (m);  pkg, vers: M3ID.T;
  BEGIN
    vers := PopID (t);
    pkg  := PopID (t);
    ImportVersion (t, pkg, vers);
  END DoImportVersion;

PROCEDURE ImportVersion (t: T;  pkg, vers: M3ID.T)
  RAISES {Quake.Error, Thread.Alerted} =
  VAR ref: REFANY;  vers_txt := M3ID.ToText (vers);
  BEGIN
    IF (pkg = t.build_pkg) THEN
      t.error ("cannot import package \"" & M3ID.ToText (pkg) & "\" into itself.");
    END;
    IF NOT t.imports.get (pkg, ref) THEN
      EVAL t.imports.put (pkg, vers_txt);
      Include (t, M3Path.New (Pkg (t, pkg), vers_txt, M3Exports));
    END;
  END ImportVersion;

PROCEDURE DoImportObj (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR 
    t := Self (m);
    file := PathOf (t, PopText (t));
    dir  := M3Path.New (PkgSubdir (t), Pathname.Prefix (file));
    base := M3ID.Add (Pathname.Last (file));
    loc  := Location (t, t.cur_pkg, M3ID.Add (dir));
    unit := M3Unit.New (base, UK.O, loc, hidden := FALSE, imported := FALSE);
    fn   := M3Unit.FileName(unit);
    pn   := Pathname.Join(loc.path, fn, NIL);
  BEGIN
    M3Unit.AddNew (t.units, base, UK.O, loc,
                   imported := FALSE, hidden := TRUE);
    M3Unit.Add (t.units, unit);
    Utils.SymbolicLinkFile (pn, fn);
  END DoImportObj;

PROCEDURE DoImportLib (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  lib, path: M3ID.T;
  BEGIN
    path := PopID (t);
    lib  := PopID (t);
    ImportOtherLib (t, lib, path, imported := FALSE);
  END DoImportLib;

(*---------------------------------------------------------------- objects --*)

PROCEDURE DoPgmObject (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  v: QValue.T;
  BEGIN
    t.pop (v);  (* extension *)
    t.pop (v);  (* filename base *)
  END DoPgmObject;

(*---------------------------------------------------------------- sources --*)

PROCEDURE DoSource (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  v: QValue.T;
  BEGIN
    t.pop (v); (* source file name, ignored *)
  END DoSource;

PROCEDURE DoPgmSource (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR
    t    := Self (m);
    nm   := PopID (t);
    name := M3ID.ToText (nm);
    dir  := M3Path.New (PkgSubdir (t), Pathname.Prefix (name));
    zz   := M3Path.Parse (Pathname.Last (name));
    base := M3ID.Add (zz.base);
    loc  := Location (t, t.cur_pkg, M3ID.Add (dir));
    unit := M3Unit.New (base, zz.kind, loc, hidden := TRUE, imported := FALSE);
  BEGIN
    M3Unit.Add (t.units, unit);
  END DoPgmSource;

PROCEDURE DoIntf (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    AddSource (t, nm, UK.I3, hidden := TRUE);
  END DoIntf;

PROCEDURE DoIntfX (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    AddSource (t, nm, UK.I3, hidden := FALSE);
  END DoIntfX;

PROCEDURE DoImpl (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    AddSource (t, nm, UK.M3, hidden := TRUE);
  END DoImpl;

PROCEDURE DoModule (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    AddSource (t, nm, UK.I3, hidden := TRUE);
    AddSource (t, nm, UK.M3, hidden := TRUE);
  END DoModule;

PROCEDURE DoModuleX (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    AddSource (t, nm, UK.I3, hidden := FALSE);
    AddSource (t, nm, UK.M3, hidden := TRUE);
  END DoModuleX;

PROCEDURE DoHSource (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    AddSource (t, PopID (t), UK.H, hidden := FALSE);
  END DoHSource;

PROCEDURE DoCSource (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    AddSource (t, nm, UK.C, hidden := TRUE);
  END DoCSource;

PROCEDURE DoSSource (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    AddSource (t, nm, UK.S, hidden := TRUE);
  END DoSSource;

PROCEDURE DoShipSource (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR
    t    := Self (m);
    nm   := PopID (t);
    name := M3ID.ToText (nm);
    dir  := M3Path.New (PkgSubdir (t), Pathname.Prefix (name));
    base := M3ID.Add (Pathname.Last (name));
    loc  := Location (t, t.cur_pkg, M3ID.Add (dir));
    unit := M3Unit.New (base, UK.Unknown, loc, hidden := TRUE, imported := FALSE);
  BEGIN
    M3Unit.Add (t.ship_units, unit);
  END DoShipSource;

(*--------------------------------------------------------------- generics --*)

PROCEDURE DoGenIntf (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    AddSource (t, PopID (t), UK.IG, hidden := TRUE);
  END DoGenIntf;

PROCEDURE DoGenIntfX (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    AddSource (t, PopID (t), UK.IG, hidden := FALSE);
  END DoGenIntfX;

PROCEDURE DoGenImpl (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    AddSource (t, PopID (t), UK.MG, hidden := TRUE);
  END DoGenImpl;

PROCEDURE DoGenImplX (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    AddSource (t, PopID (t), UK.MG, hidden := FALSE);
  END DoGenImplX;

PROCEDURE DoGenModule (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    AddSource (t, nm, UK.IG, hidden := TRUE);
    AddSource (t, nm, UK.MG, hidden := TRUE);
  END DoGenModule;

PROCEDURE DoGenModuleX (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    AddSource (t, nm, UK.IG, hidden := FALSE);
    AddSource (t, nm, UK.MG, hidden := FALSE);
  END DoGenModuleX;

PROCEDURE DoBuildGenIntf (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR
    t := Self (m);
    nm: M3ID.T;
    name, generic: TEXT;
    args: QVSeq.T;
    vis: BOOLEAN;
  BEGIN
    vis     := PopBool (t);
    args    := PopArray (t);
    generic := PopText (t);
    nm      := PopID (t);
    name    := M3ID.ToText (nm);
    BuildGeneric (t, "INTERFACE ", name & ".i3", name, generic, args);
    AddDerived (t, nm, UK.I3, hidden := vis);
  END DoBuildGenIntf;

PROCEDURE DoBuildGenImpl (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);   nm: M3ID.T;  name, generic: TEXT;  args: QVSeq.T;
  BEGIN
    args    := PopArray (t);
    generic := PopText (t);
    nm      := PopID (t);
    name    := M3ID.ToText (nm);
    BuildGeneric (t, "MODULE ", name & ".m3", name, generic, args);
    AddDerived (t, nm, UK.M3, hidden := TRUE);
  END DoBuildGenImpl;

PROCEDURE BuildGeneric (t: T;  kind, file, name, generic: TEXT;  args: QVSeq.T)
  RAISES {Quake.Error} =
  CONST tmp = ".generic.tmp";
  VAR txts: TextVector;

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    VAR comma: TEXT := NIL;
    BEGIN
      Out (wr, "(*generated by m3build*)", Wr.EOL, Wr.EOL);
      Out (wr, kind, " ", name, " = ");
      Out (wr, generic, " (");
      FOR i := 0 TO LAST (txts^) DO
        Out (wr, comma, txts[i]);
        comma := ", ";
      END;
      Out (wr, ") END ", name, ".", Wr.EOL);
    END Emit;

  BEGIN
    IF (t.mode = MM.Build) THEN
      txts := SeqToTexts (t, args);
      Utils.WriteFile (tmp, Emit, append := FALSE);
      t.cp_if (tmp, file);
      Utils.Remove (tmp);
    END;
  END BuildGeneric;

(*-------------------------------------------------------- derived sources --*)

PROCEDURE DoDerivedIntf (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  file: M3ID.T;  vis: BOOLEAN;
  BEGIN
    vis  := PopBool (t);
    file := PopID (t);
    AddDerived (t, file, UK.I3, hidden := vis);
    DeleteDeriveds (t, M3ID.ToText (file), IntfExtensions);
  END DoDerivedIntf;

PROCEDURE DoDerivedImpl (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  file := PopID (t);
  BEGIN
    AddDerived (t, file, UK.M3, hidden := TRUE);
    DeleteDeriveds (t, M3ID.ToText (file), ImplExtensions);
  END DoDerivedImpl;

PROCEDURE DoDerivedC (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  file := PopID (t);
  BEGIN
    AddDerived (t, file, UK.C, hidden := TRUE);
    DeleteDeriveds (t, M3ID.ToText (file), CExtensions);
  END DoDerivedC;

PROCEDURE DoDerivedH (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    AddDerived (t, PopID (t), UK.H, hidden := FALSE);
  END DoDerivedH;

(*------------------------------------------------------- hiding/exporting --*)
(* These are forwarded in the exports file *)

PROCEDURE SetVis (t: T;  nm: M3ID.T;  kind: UK;  hidden: BOOLEAN) =
  VAR u := M3Unit.Get (t.units, nm, kind);
  BEGIN
    IF (u # NIL) THEN
      u.hidden := hidden;
    ELSE
      Msg.FatalError (NIL, "set_visibility of unknown unit: ",
                      M3Path.Join (NIL, M3ID.ToText (nm), kind));
    END;
  END SetVis;

PROCEDURE DoHideIntf (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    SetVis (t, nm, UK.I3, hidden := TRUE);
    AddTexts (t.tfile_args, "hide_interface(\"",
              M3ID.ToText (nm), QRPCR);
  END DoHideIntf;

PROCEDURE DoHideGenIntf (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    SetVis (t, nm, UK.IG, hidden := TRUE);
    AddTexts (t.tfile_args, "hide_generic_interface(\"",
              M3ID.ToText (nm), QRPCR);
  END DoHideGenIntf;

PROCEDURE DoHideGenImpl (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    SetVis (t, nm, UK.MG, hidden := TRUE);
    AddTexts (t.tfile_args, "hide_generic_implementation(\"",
              M3ID.ToText (nm), QRPCR);
  END DoHideGenImpl;

PROCEDURE DoExportIntf (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    SetVis (t, nm, UK.I3, hidden := FALSE);
    AddTexts (t.tfile_args, "export_interface(\"",
              M3ID.ToText (nm), QRPCR);
  END DoExportIntf;

PROCEDURE DoExportGenIntf (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    SetVis (t, nm, UK.IG, hidden := FALSE);
    AddTexts (t.tfile_args, "export_generic_interface(\"",
              M3ID.ToText (nm), QRPCR);
  END DoExportGenIntf;

PROCEDURE DoExportGenImpl (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    SetVis (t, nm, UK.MG, hidden := FALSE);
    AddTexts (t.tfile_args, "export_generic_implementation(\"",
              M3ID.ToText (nm), QRPCR);
  END DoExportGenImpl;

(*-------------------------------------------------------------- templates --*)

PROCEDURE DoTemplate (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error, Thread.Alerted} =
  VAR t := Self (m);  nm := PopID (t);
  BEGIN
    AddSource (t, nm, UK.TMPL, hidden := FALSE);
    Include (t, M3Path.Join (NIL, M3ID.ToText (nm), UK.TMPL));
  END DoTemplate;

(*------------------------------------------------------- library building --*)

PROCEDURE DoLibrary (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm: M3ID.T;  name, lib_a, lib_m3x: TEXT;
  BEGIN
    nm := PopID (t);  name := M3ID.ToText (nm);
    AddDerived (t, nm, UK.M3LIB, hidden := TRUE);
    GenM3Exports (t, "_define_lib(\"" & name & QRPCR);
    lib_a   := M3Path.Join (NIL, name, UK.M3LIB);
    lib_m3x := M3Path.Join (NIL, name, UK.LIBX);
    IF (t.mode = MM.Build) THEN
      Builder.BuildLib (name, t.units, SysLibs (t), t.build_shared, t);
      InstallDerived (t, lib_a);
      InstallDerived (t, lib_m3x);
      InstallDerived (t, M3Web);
      InstallSources (t);
    END;
    IF (t.mode = MM.Find) THEN FindUnits (t); END;
    IF (t.mode = MM.Depend) THEN 
      Msg.Out (M3ID.ToText(t.build_pkg), ":");
      Builder.EmitPkgImports (t.units); 
      done := TRUE;
    END;
    DeleteDeriveds (t, "", ARRAY OF TEXT {lib_a, lib_m3x, M3Web, M3TFile, "_m3responsefile0.txt", "_m3responsefile1.txt"});
    DeleteDeriveds (t, name, ARRAY OF TEXT {".lst", ".def", ".dll", ".exp", ".lib.sa", ".pdb"});
    InitGlobals (t);  (* forget about the accumulated sources... *)
  END DoLibrary;

(*------------------------------------------------------- program building --*)

PROCEDURE DoProgramCommon (m: QMachine.T; bindExport: BOOLEAN) RAISES {Quake.Error} =
  VAR
    t    := Self (m);
    nm   := PopID (t);
    prog := M3Path.ProgramName (M3ID.ToText (nm));
  BEGIN
    BuildProgram (t, nm);
    IF bindExport THEN
      BindExport (t, prog);
    ELSIF (t.mode = MM.Build) THEN
      InstallDerived (t, prog);
    END;
  END DoProgramCommon;

PROCEDURE DoProgram (m: QMachine.T;  <*UNUSED*> n_args: INTEGER) RAISES {Quake.Error} =
  BEGIN
    DoProgramCommon (m, FALSE);
  END DoProgram;

PROCEDURE DoProgramX (m: QMachine.T;  <*UNUSED*> n_args: INTEGER) RAISES {Quake.Error} =
  BEGIN
    DoProgramCommon (m, TRUE);
  END DoProgramX;

PROCEDURE BuildProgram (t: T;  nm: M3ID.T)
  RAISES {Quake.Error} =
  CONST Extras = ARRAY OF TEXT { "_m3main.c","_m3main.o","_m3main.obj", "_m3responsefile0.txt", M3Web, M3TFile };
  CONST Junk = ARRAY OF TEXT { ".map", ".lst", ".pdb" };
  VAR name := M3ID.ToText (nm);
  BEGIN
    (*** AddDerived (t, nm, UK.EXE, hidden := TRUE);  -- not needed ***)
    GenM3Exports (t, "_define_pgm(\"" & name & QRPCR);
    IF (t.mode = MM.Build) THEN
      Builder.BuildPgm (name, t.units, SysLibs (t), t.build_shared, t);
      InstallDerived (t, M3Web);
      InstallSources (t);
    END;
    IF (t.mode = MM.Find) THEN FindUnits (t); END;
    IF (t.mode = MM.Depend) THEN 
      Msg.Out (M3ID.ToText(t.build_pkg), ":");
      Builder.EmitPkgImports (t.units);
      done := TRUE;
    END;
    DeleteDeriveds (t, M3Path.ProgramName (name), NoExtension);
    DeleteDeriveds (t, M3Path.Join (NIL, name, UK.PGMX), NoExtension);
    DeleteDeriveds (t, name, Junk);
    DeleteDeriveds (t, "", Extras);
    InitGlobals (t);  (* forget about the accumulated sources... *)
  END BuildProgram;

PROCEDURE DoCProgramCommon (m: QMachine.T; bindExport: BOOLEAN) RAISES {Quake.Error} =
  VAR t := Self (m);  name := PopText (t);  prog: TEXT;
  BEGIN
    IF (t.mode = MM.Build) THEN
      Builder.BuildCPgm (name, t.units, SysLibs (t), t.build_shared, t);
    END;
    IF (t.mode = MM.Find) THEN FindUnits (t); END;
    IF (t.mode = MM.Depend) THEN
      Msg.Out (M3ID.ToText(t.build_pkg), ":");
      Builder.EmitPkgImports (t.units);
      done := TRUE;
    END;
    prog := M3Path.ProgramName (name);
    IF bindExport THEN BindExport (t, prog); END;
    DeleteDeriveds (t, prog, NoExtension);
    InitGlobals (t);  (* forget about the accumulated sources... *)
  END DoCProgramCommon;

PROCEDURE DoCProgram (m: QMachine.T;  <*UNUSED*> n_args: INTEGER) RAISES {Quake.Error} =
  BEGIN
    DoCProgramCommon (m, FALSE);
  END DoCProgram;

(* DoCProgram and BindExport (program) *)
PROCEDURE DoCProgramX (m: QMachine.T;  <*UNUSED*> n_args: INTEGER) RAISES {Quake.Error} =
  BEGIN
    DoCProgramCommon (m, TRUE);
  END DoCProgramX;

(*-------------------------------------------------------------- man pages --*)

PROCEDURE DoManPage (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm, sec: TEXT;
  BEGIN
    sec := PopText (t);
    nm  := PopText (t);
    BuildManPage (t, nm, sec);
  END DoManPage;

PROCEDURE DoManPageX (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm, sec: TEXT;
  BEGIN
    sec := PopText (t);
    nm  := PopText (t);
    BuildManPage (t, nm, sec);
    InstallMan (t, nm, sec, derived := TRUE);
  END DoManPageX;

PROCEDURE BuildManPage (t: T;  nm, sec: TEXT)
  RAISES {Quake.Error} =
  VAR
    dest := nm & "." & sec;
    src  := PathOf (t, dest);
  BEGIN
    IF (t.mode = MM.Build) THEN
      IF IsStale (dest, src) THEN
        Utils.Copy (src, dest);
      END;
    END;
    DeleteDeriveds (t, dest, NoExtension);
  END BuildManPage;

(*------------------------------------------------------------------ emacs --*)

PROCEDURE DoGnuEmacs (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  nm := PopText (t);
  BEGIN
    EmacsExport (t, PathOf (t, nm & ".el"));
  END DoGnuEmacs;

PROCEDURE DoCompiledEmacs (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error, Thread.Alerted} =
  VAR t := Self (m);  nm := PopText (t);  src, el, elc: TEXT;
  BEGIN
    IF t.lookup (M3ID.Add ("emacs_compile")) # NIL THEN
      el  := nm & ".el";
      elc := nm & ".elc";
      src := PathOf (t, el);
      IF (t.mode = MM.Build) THEN
        t.cp_if (src, el);
        IF IsStale (elc, el) THEN
          VAR b := t.lookup (M3ID.Add ("emacs_compile")); BEGIN
             IF (b # NIL) THEN
               t.start_call (b.value);
               PushText (t, el);
               t.call_proc (1, FALSE);
             END;
          END;
        END;
      END;
      EmacsdExport (t, el);
      EmacsdExport (t, elc);
      DeleteDeriveds (t, el, NoExtension);
      DeleteDeriveds (t, elc, NoExtension);
    ELSE
      EmacsExport (t, PathOf (t, nm & ".el"));
    END;
  END DoCompiledEmacs;

(*-------------------------------------------------------- m3where support --*)

PROCEDURE DoFindUnit (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  unit: TEXT;  any_unit: BOOLEAN;
  BEGIN
    any_unit := PopBool (t);
    unit     := PopText (t);
    FindUnit (t, unit, any_unit);
  END DoFindUnit;

PROCEDURE FindUnits (t: T)
  RAISES {Quake.Error} =
  VAR bind := t.lookup (M3ID.Add ("M3_FIND_UNITS"));  txts: TextVector;
  BEGIN
    IF (bind = NIL) THEN RETURN END;
    txts := SeqToTexts (t, QVal.ToArray (t, bind.value));
    FOR i := FIRST (txts^) TO LAST (txts^) DO
      FindUnit (t, txts[i], any_unit := TRUE);
    END;
  END FindUnits;

PROCEDURE FindUnit (t: T;  unit: TEXT;  any_unit: BOOLEAN) =
  VAR
    u: M3Unit.T;
    found := FALSE;
    unm  := M3ID.Add (unit);
    name : M3Path.T;
    base : M3ID.T;
  BEGIN
    name := M3Path.Parse (unit);
    base := M3ID.Add (name.base);
    
    u := t.units.head;
    WHILE (u # NIL) DO
      IF ((u.name = unm) OR (u.name = base AND u.kind = name.kind))
        AND (any_unit OR NOT u.hidden) THEN
        Msg.Out (M3Unit.FullPath (u), Wr.EOL);
        found := TRUE;
      END;
      u := u.next;
    END;

    IF NOT found THEN
      Msg.Out ("\"", unit, "\" not found.", Wr.EOL);
    END;
  END FindUnit;

PROCEDURE DoEnumUnits (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  any_unit := PopBool (t);  u: M3Unit.T;
  BEGIN
    u := t.units.head;
    WHILE (u # NIL) DO
      IF any_unit OR NOT u.hidden THEN
        Msg.Out (M3Unit.FullPath (u), Wr.EOL);
      END;
      u := u.next;
    END;
  END DoEnumUnits;

(*------------------------------------------------------- export functions --*)

PROCEDURE DoInstallSources (m: QMachine.T;  <*UNUSED*> n_args: INTEGER) =
  BEGIN
    InstallSources (Self (m));
  END DoInstallSources;

PROCEDURE InstallSources (t: T) =
  CONST UKSourceSet = SET OF UK { UK.I3, UK.M3, UK.IG, UK.MG, UK.C, UK.H, UK.TMPL }; 

  VAR
    n_local := 0;
    u : M3Unit.T;
    srcs: REF ARRAY OF M3Unit.T;
    map : REF ARRAY OF INTEGER;
    
  PROCEDURE CmpUnit (ax, bx: INTEGER): [-1 .. +1] =
    VAR a := srcs[ax];  b := srcs[bx];
    BEGIN
      IF a.loc.pkg < b.loc.pkg THEN RETURN -1; END;
      IF a.loc.pkg > b.loc.pkg THEN RETURN +1; END;
      IF a.loc.subdir < b.loc.subdir THEN RETURN -1; END;
      IF a.loc.subdir > b.loc.subdir THEN RETURN + 1; END;
      IF a.name < b.name THEN RETURN -1; END;
      IF a.name > b.name THEN RETURN +1; END;
      RETURN 0;
    END CmpUnit;

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    VAR uu: M3Unit.T;  last_loc : M3Loc.T := NIL;  src, dest: TEXT;
    BEGIN
      FOR i := 0 TO LAST (map^) DO
        uu := srcs [map[i]];
        IF (uu.loc # last_loc) THEN
          dest := M3Path.New (t.pkg_install, M3ID.ToText (uu.loc.pkg),
                              M3ID.ToText (uu.loc.subdir));
          InstallDir (t, dest, wr);
          last_loc := uu.loc;
        END;
        src := M3Unit.FullPath (uu);
        Out (wr, "install_file(\"", M3Path.Convert (src), QC);
        Out (wr, Unresolve (t, M3Path.Convert (dest)), CQ, ModeF, QRPCR);
      END;
    END Emit;

  BEGIN
    IF t.have_pkgtools THEN RETURN; END;

    (* count the local sources *)
    u := t.units.head;
    WHILE (u # NIL) DO
      IF NOT u.imported AND u.kind IN UKSourceSet THEN
        INC (n_local);
      END;
      u := u.next;
    END;
    u := t.ship_units.head;
    WHILE (u # NIL) DO
      INC (n_local);
      u := u.next;
    END;

    (* collect up the sources in an array *)
    srcs := NEW (REF ARRAY OF M3Unit.T, n_local);
    map  := NEW (REF ARRAY OF INTEGER,  n_local);
    n_local := 0;
    u := t.units.head;
    WHILE (u # NIL) DO
      IF NOT u.imported AND u.kind IN UKSourceSet THEN
        srcs [n_local] := u;
        map  [n_local] := n_local;
        INC (n_local);
      END;
      u := u.next;
    END;
    u := t.ship_units.head;
    WHILE (u # NIL) DO
      srcs [n_local] := u;
      map  [n_local] := n_local;
      INC (n_local);
      u := u.next;
    END;

    (* sort the local sources by directory *)
    IntArraySort.Sort (map^, CmpUnit);

    (* finally, dump the file *)
    Utils.WriteFile (M3Ship, Emit, append := TRUE);
  END InstallSources;

PROCEDURE DoInstallDerived (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    InstallDerived (t, PopText (t));
  END DoInstallDerived;

PROCEDURE DoInstallDerivedSymbolicLink (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
      to   := PopText (t);
      from := PopText (t);
  BEGIN
    InstallDerivedLink (t, from, to, "symbolic_link_file");
  END DoInstallDerivedSymbolicLink;

PROCEDURE DoInstallDerivedHardLink (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
      to   := PopText (t);
      from := PopText (t);
  BEGIN
    InstallDerivedLink (t, from, to, "hard_link_file");
  END DoInstallDerivedHardLink;

PROCEDURE DoInstallSymbolLinkToDerived (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
      dest := PopText (t);
      src  := PopText (t);
  BEGIN
    InstallLinkToDerived (t, src, dest, "symbolic_link_file");
  END DoInstallSymbolLinkToDerived;

PROCEDURE DoInstallHardLinkToDerived (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
      dest := PopText (t);
      src  := PopText (t);
  BEGIN
    InstallLinkToDerived (t, src, dest, "hard_link_file");
  END DoInstallHardLinkToDerived;

PROCEDURE DoInstallSymbolLink (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
      dest := PopText (t);
      src  := PopText (t);
  BEGIN
    InstallLink (t, src, dest, "symbolic_link_file");
  END DoInstallSymbolLink;

PROCEDURE DoInstallFile (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  CONST Pad = ARRAY [0..16] OF TEXT { "                ", "               ",
    "              ", "             ", "            ", "           ",
    "          ", "         ", "        ", "       ", "      ", "     ",
    "    ", "   ", "  ", " ", "" };
  VAR t := Self (m);  src, dest, mode, src_dir, src_file, prefix: TEXT;
  BEGIN
    mode := PopText (t);  (* ignored *)
    dest := PopText (t);
    src  := PopText (t);

    IF (Msg.level >= Msg.Level.Explain) THEN
      src_dir := Pathname.Prefix (src);
      src_file := Pathname.Last (src);
      IF (src_dir = NIL) OR Text.Equal (src_dir, "") THEN src_dir := "."; END;

      IF NOT Text.Equal (dest, t.last_install_dir)
        OR NOT Text.Equal (src_dir, t.last_src_dir) THEN
        MakeRoom (t, 99999);
        Msg.Explain (src_dir, " => ", M3Path.New(dest));
        t.last_install_dir := dest;
        t.last_src_dir := src_dir;
        t.listing_width := 0;
      END;

      MakeRoom (t, 2 + MAX (Text.Length (src_file), LAST (Pad)));
      Msg.Out ("  ", src_file, Pad [MIN (Text.Length (src_file), LAST (Pad))]);
    END;

    prefix := Env.Get("CM3_INSTALL_PREFIX");
    IF prefix # NIL THEN
      dest := prefix & dest;
    END;

    t.cp_if (src, dest);
  END DoInstallFile;

PROCEDURE MakeRoom (t: T;  space: INTEGER) =
  BEGIN
    INC (t.listing_width, space);
    IF (t.listing_width > 75) THEN
      (* this line will be too long... *)
      IF (t.listing_width > space) THEN
        (* the existing line is non-empty *)
        Msg.Explain ();  (* force a new-line *)
      END;
      t.listing_width := space;
    END;
  END MakeRoom;

(*---------------------------------------------- internal export utilities --*)

PROCEDURE DoUnresolve (t: T;  res: TEXT): TEXT =
(* This should "rewritten" as follows:
   for each foo => bar substitution:
     substitute /foo/ => /bar/ anywhere in string
     substitute /foo => /bar at end of string
     substitute foo/ => bar/ at start of string
   This could be deemed TextUtils.SubstituteWithSeparator. *) 
  BEGIN
    res := TextUtils.Substitute(res, "\\", "/");
    res := TextUtils.Substitute(res, t.bin_install_alt, "\" & BIN_INSTALL & \"");
    res := TextUtils.Substitute(res, t.lib_install_alt, "\" & LIB_INSTALL & \"");
    res := TextUtils.Substitute(res, t.doc_install_alt, "\" & DOC_INSTALL & \"");
    res := TextUtils.Substitute(res, t.man_install_alt, "\" & MAN_INSTALL & \"");
    res := TextUtils.Substitute(res, t.html_install_alt, "\" & HTML_INSTALL & \"");
    res := TextUtils.Substitute(res, t.emacs_install_alt, "\" & EMACS_INSTALL & \"");
    res := TextUtils.Substitute(res, t.pkg_install_alt, "\" & PKG_INSTALL & \"");
    res := TextUtils.Substitute(res, t.pkg_use_alt, "\" & PKG_USE & \"");
    
    (* INSTALL_ROOT needs to be relatively late since it is a prefix of others.
     * TARGET needs to be even later since it can occur arbitrarily, as in:
     * e.g. /Users/hudson/workspace/makedist-AMD64_DARWIN/bin
     *                                       ^--TARGET--^
     *      ^--------------------------- INSTALL_ROOT --^
     *      ^--------------------------- BIN_INSTALL  ------^
     *)
    res := TextUtils.Substitute(res, t.install_root_alt, "\" & INSTALL_ROOT & \"");
    res := TextUtils.Substitute(res, t.text_build_dir, "\" & TARGET & \"");

    res := TextUtils.Substitute(res, "\"\" & ", "");
    res := TextUtils.Substitute(res, " & \"\"", "");
    RETURN res;
  END DoUnresolve;

PROCEDURE Unresolve (t: T;  pn: TEXT): TEXT =
  BEGIN
    pn := "\"" & pn & "\"";
    IF noM3ShipResolution THEN
      pn := DoUnresolve(t, pn);
    END;
    RETURN pn;
  END Unresolve;

PROCEDURE InstallDerived (t: T;  name: TEXT) =

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    VAR dest := InstallDerivedDir (t);
    BEGIN
      InstallDir (t, dest, wr);
      Out (wr, "install_file(\"", M3Path.Convert (name));
      Out (wr, QC, Unresolve (t, M3Path.Convert (dest)), CQ, ModeF, QRPCR);
    END Emit;

  BEGIN
    IF t.have_pkgtools THEN RETURN; END;
    Utils.WriteFile (M3Ship, Emit, append := TRUE);
  END InstallDerived;

PROCEDURE InstallDerivedLink (t: T;  from, to: TEXT; ship_function: TEXT) =

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    VAR
      dest_dir  := InstallDerivedDir (t);
      to_file   := M3Path.New (dest_dir, to);
      from_file := M3Path.New (UseDerivedDir (t), from);
    BEGIN
      InstallDir (t, dest_dir, wr);
      Out (wr, ship_function, "(", Unresolve (t, M3Path.Convert (from_file)), C);
      Out (wr, Unresolve (t, M3Path.Convert (to_file)), RPCR);
    END Emit;

  BEGIN
    IF t.have_pkgtools THEN RETURN; END;
    Utils.WriteFile (M3Ship, Emit, append := TRUE);
  END InstallDerivedLink;

PROCEDURE InstallDerivedDir (t: T): TEXT =
  VAR pkg := M3ID.ToText (t.build_pkg);  dir := M3ID.ToText (t.build_dir);
  BEGIN
    RETURN M3Path.New (t.pkg_install, pkg, dir);
  END InstallDerivedDir;

PROCEDURE UseDerivedDir (t: T): TEXT =
  VAR pkg := M3ID.ToText (t.build_pkg);  dir := M3ID.ToText (t.build_dir);
  BEGIN
    RETURN M3Path.New (t.pkg_use, pkg, dir);
  END UseDerivedDir;

PROCEDURE InstallLink (t: T;   src, dest: TEXT; ship_function: TEXT)
  RAISES {Quake.Error} =

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    VAR dest_dir := Pathname.Prefix(dest);
    BEGIN
      InstallDir (t, dest_dir, wr);
      Out (wr, ship_function, "(", Unresolve (t, M3Path.Convert (src)), C);
      Out (wr, Unresolve (t, M3Path.Convert (dest)), RPCR)
    END Emit;

  BEGIN
    IF t.have_pkgtools THEN (* ? *)
      InstallFile (t, src, dest, ModeX, derived := TRUE);
    ELSE
      Utils.WriteFile (M3Ship, Emit, append := TRUE);
    END;
  END InstallLink;

PROCEDURE InstallLinkToDerived (t: T;   src, dest: TEXT; ship_function: TEXT)
  RAISES {Quake.Error} =
  VAR target := M3Path.New (UseDerivedDir (t), src);
      link   := M3Path.New (dest, src);
  BEGIN
    InstallLink(t, target, link, ship_function);
  END InstallLinkToDerived;

PROCEDURE InstallDir (t: T;  dir: TEXT;  wr: Wr.T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF NOT Text.Equal (dir, t.last_ship_dir) THEN
      IF t.have_pkgtools THEN
        Out (wr, "-l ", dir, Wr.EOL);
      ELSIF NOT t.all_ship_dirs.put (M3ID.Add (dir), NIL) THEN
        Out (wr, "make_dir(", Unresolve (t, M3Path.Convert (dir)), RPCR);
      END;
      t.last_ship_dir := dir;
    END;
  END InstallDir;

PROCEDURE InstallFile (t: T;  src, dest, mode: TEXT;  derived: BOOLEAN)
  RAISES {Quake.Error} =
  VAR src_path: TEXT;

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      (* make sure the install directory is built *)
      InstallDir (t, dest, wr);
  
      (* generate the code to install the file *)
      IF t.have_pkgtools THEN
        Out (wr, M3ID.ToText (t.build_dir), "/", src, Wr.EOL);
      ELSE
        Out (wr, "install_file(", Unresolve (t, M3Path.Convert (src)), C); (* Unresolve needed here? *)
        Out (wr, Unresolve(t, M3Path.Convert (dest)), CQ, mode, QRPCR);
      END;
    END Emit;

  BEGIN
    IF NOT derived THEN
      src_path := PathOf (t, src);
      IF t.have_pkgtools THEN
        (* Since the package tools refuse to export things that are outside
           the current package and in general we don't know where a source
           file resides, we make links to any exported source files. *)
        IF (t.cur_pkg # t.build_pkg) THEN
          IF (t.mode = MM.Build) THEN
            IF IsStale (src, src_path) THEN
              Utils.SymbolicLinkFile (src_path, src);
              src_path := src;
            END;
          END;
          DeleteDeriveds (t, src, NoExtension);
        END;
      END;
      src := src_path;
    END;

    IF NOT (t.mode = MM.Build) THEN RETURN; END;

    IF t.pkg_overrides.size () > 0 THEN
      (* There were overides => don't create any shipping information *)
      NoteOverrides (t);
      RETURN;
    END;

    Utils.WriteFile (M3Ship, Emit, append := TRUE);
  END InstallFile;

PROCEDURE InstallMan (t: T;  page, sec: TEXT;  derived: BOOLEAN)
  RAISES {Quake.Error} =
  VAR
   src  := page & "." & sec;
   dest := M3Path.New (t.man_install, "man" & sec);
  BEGIN
    InstallFile (t, src, dest, ModeF, derived);
  END InstallMan;

PROCEDURE InstallSource (t: T;  src, dest, mode: TEXT) =

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    BEGIN
      (* make sure the install directory is built *)
      InstallDir (t, dest, wr);
      IF t.have_pkgtools THEN
        Out (wr, src, Wr.EOL);
      ELSE
        Out (wr, "install_file(", Unresolve (t, M3Path.Convert (src)), C); (* Unresolve needed here? *)
        Out (wr, Unresolve (t, M3Path.Convert (dest)), CQ, mode, QRPCR);
      END;
    END Emit;
    
  BEGIN
    IF NOT (t.mode = MM.Build) THEN RETURN; END;

    IF t.pkg_overrides.size () > 0 THEN
      (* There were overides => don't create any shipping information *)
      NoteOverrides (t);
      RETURN;
    END;

    Utils.WriteFile (M3Ship, Emit, append := TRUE);
  END InstallSource;

PROCEDURE IsStale (obj, src: TEXT): BOOLEAN =
  BEGIN
    RETURN Utils.ModificationTime (obj) < Utils.ModificationTime (src);
  END IsStale;

PROCEDURE NoteOverrides (<*UNUSED*> t: T) =
  BEGIN
    Utils.Remove (M3Ship);
    TouchFile (M3Overrides);
  END NoteOverrides;

(*------------------------------------------ user callable export routines --*)

PROCEDURE DoBindExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    BindExport (t, PopText (t));
  END DoBindExport;

PROCEDURE BindExport (t: T;  file: TEXT)
  RAISES {Quake.Error} =
  BEGIN
    InstallFile (t, file, t.bin_install, ModeX, derived := TRUE);
  END BindExport;

PROCEDURE DoBinExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  file := PopText (t);
  BEGIN
    InstallFile (t, file, t.bin_install, ModeX, derived := FALSE);
  END DoBinExport;

PROCEDURE DoLibdExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  file := PopText (t);
  BEGIN
    InstallFile (t, file, t.lib_install, ModeX, derived := TRUE);
  END DoLibdExport;

PROCEDURE DoLibExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  file := PopText (t);
  BEGIN
    InstallFile (t, file, t.lib_install, ModeX, derived := FALSE);
  END DoLibExport;

PROCEDURE DoEmacsdExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    EmacsdExport (t, PopText (t));
  END DoEmacsdExport;

PROCEDURE EmacsdExport (t: T;  file: TEXT)
  RAISES {Quake.Error} =
  BEGIN
    InstallFile (t, file, t.emacs_install, ModeF, derived := TRUE);
  END EmacsdExport;

PROCEDURE DoEmacsExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    EmacsExport (t, PopText (t));
  END DoEmacsExport;

PROCEDURE EmacsExport (t: T;  file: TEXT)
  RAISES {Quake.Error} =
  BEGIN
    InstallFile (t, file, t.emacs_install, ModeF, derived := FALSE);
  END EmacsExport;

PROCEDURE DoDocdExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  file := PopText (t);
  BEGIN
    InstallFile (t, file, t.doc_install, ModeF, derived := TRUE);
  END DoDocdExport;

PROCEDURE DoDocExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  file := PopText (t);
  BEGIN
    InstallFile (t, file, t.doc_install, ModeF, derived := FALSE);
  END DoDocExport;

PROCEDURE DoMandExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  page, sec: TEXT;
  BEGIN
    sec := PopText (t);
    page := PopText (t);
    InstallMan (t, page, sec, derived := TRUE);
  END DoMandExport;

PROCEDURE DoManExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  page, sec: TEXT;
  BEGIN
    sec := PopText (t);
    page := PopText (t);
    InstallMan (t, page, sec, derived := FALSE);
  END DoManExport;

PROCEDURE DoHtmlExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  file := PopText (t);
  BEGIN
    InstallSource (t, file, t.html_install, ModeF);
  END DoHtmlExport;

PROCEDURE DoRootExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR 
    t := Self (m);  
    reldir := PopText (t);
    file := PopText (t);
  BEGIN
    IF Pathname.Absolute (reldir) THEN
      RAISE Quake.Error ("can only export relative to INSTALL_ROOT");
    END;
    WITH dir = Pathname.Join( t.install_root, reldir ) DO
      InstallFile (t, file, dir, ModeF, derived := FALSE);
    END;
  END DoRootExport; 

PROCEDURE DoRootdExport (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR 
    t := Self (m);  
    reldir := PopText (t);
    file := PopText (t);
  BEGIN
    IF Pathname.Absolute (reldir) THEN
      RAISE Quake.Error ("can only export relative to INSTALL_ROOT");
    END;
    WITH dir = Pathname.Join( t.install_root, reldir ) DO
      InstallFile (t, file, dir, ModeF, derived := TRUE);
    END;
  END DoRootdExport; 

(*------------------------------------------------------------- .M3EXPORTS --*)

PROCEDURE DoGenM3Exports (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);
  BEGIN
    EVAL PopID (t);   (* backward compatibility *)
    GenM3Exports (t, NIL);
  END DoGenM3Exports;

PROCEDURE GenM3Exports (t: T;  header: TEXT)
  RAISES {Quake.Error} =
  CONST HTag = ARRAY BOOLEAN OF TEXT { "", "hidden" };
  CONST KindTag = ARRAY UK OF TEXT {
     NIL,
     "_map_add_interface", NIL, NIL, NIL, NIL,
     "_map_add_module", NIL, NIL, NIL, NIL, 
     "_map_add_generic_interface", "_map_add_generic_module",
     "_map_add_c", "_map_add_h", NIL, "_map_add_s", NIL,
     "_import_m3lib", "_import_otherlib", NIL, NIL, NIL, "template" } ;

  VAR fail_msg: TEXT := NIL;

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    VAR nm: INTEGER;  ref: REFANY;
    BEGIN
      Out (wr, "% exports of ", M3ID.ToText (t.build_pkg), Wr.EOL);
      Out (wr, header);
  
      (* copy forward overrides *)
      VAR it := t.pkg_overrides.iterate (); BEGIN
        WHILE it.next (nm, ref) DO
          Out (wr, "override(\"", M3Path.Convert (M3ID.ToText (nm)), QCQ);
          Out (wr, M3Path.Convert (ref), QRPCR);
        END;
      END;
  
      (* copy forward package imports *)
      VAR x := t.imports.iterate (); BEGIN
        WHILE x.next (nm, ref) DO
          Out (wr, "import_version(\"", M3Path.Convert (M3ID.ToText (nm)), QCQ);
          Out (wr, M3Path.Convert (ref), QRPCR);
        END;
      END;
  
      (* output the units *)
      VAR u := t.units.head; BEGIN
        WHILE (u # NIL) DO
          IF (NOT u.imported) THEN
            CASE u.kind OF
            | UK.Unknown, UK.IB, UK.IC, UK.IS, UK.MB, UK.MC, UK.MS, UK.PGM, UK.LIBX,
              UK.PGMX, UK.IO, UK.MO, UK.O, UK.B =>
                <*ASSERT KindTag[u.kind] = NIL *>
            | UK.I3, UK.M3, UK.IG, UK.MG, UK.C, UK.H, UK.S =>
                <*ASSERT KindTag[u.kind] # NIL *>
                Out (wr, KindTag[u.kind], "(\"");
                Out (wr, M3Path.Convert (M3Unit.FileName (u)), QCQ);
                Out (wr, M3Path.Convert (M3ID.ToText (u.loc.pkg)), QCQ);
                Out (wr, M3Path.Convert (M3ID.ToText (u.loc.subdir)), QCQ);
                Out (wr, HTag [u.hidden], QRPCR);
            | UK.M3LIB =>
                Out (wr, "_import_m3lib(\"", M3ID.ToText (u.name), QCQ);
                Out (wr, M3Path.Convert (M3ID.ToText (u.loc.pkg)), QCQ);
                Out (wr, M3Path.Convert (M3ID.ToText (u.loc.subdir)), QRPCR);
            | UK.LIB =>
                Out (wr, "_import_otherlib(\"", M3ID.ToText (u.name), QCQ);
                Out (wr, M3Path.Convert (u.loc.path), "\", IMPORTED)", Wr.EOL);
            | UK.TMPL =>
                Out (wr, "_import_template(\"", M3Unit.FileName (u), QCQ);
                Out (wr, M3Path.Convert (M3ID.ToText (u.loc.pkg)), QCQ);
                Out (wr, M3Path.Convert (M3ID.ToText (u.loc.subdir)), QRPCR);
            END;
          END;
          u := u.next;
        END;
      END;
  
      (* dump the rest, including the explicit hide/export calls *)
      VAR x := t.tfile_args.head; BEGIN
        WHILE (x # NIL) DO
          Out (wr, x.head);
          x := x.tail;
        END;
      END;
  
      (* dump any 'custom' calls; these must come after any _import_template()
         calls, which may introduce the definitions of the custom calls  *)
      VAR b := t.lookup (M3ID.Add ("gen_map_hooks")); BEGIN
        IF (b # NIL) THEN
          TRY
            VAR sav := t.cur_wr (); BEGIN
              t.set_wr (wr);
              t.start_call (b.value);
              t.call_proc (0, FALSE);
              t.set_wr (sav);
            END;
          EXCEPT Quake.Error (msg) =>
            fail_msg := msg;
          END;
        END;
      END;
    END Emit;

  BEGIN
    Utils.WriteFile (M3Exports, Emit, append := FALSE);
    IF (fail_msg # NIL) THEN RAISE Quake.Error (fail_msg); END;
    IF (t.mode = MM.Build) THEN InstallDerived (t, M3Exports);  END;
    DeleteDeriveds (t, M3Exports, NoExtension);
  END GenM3Exports;

(*------------------------------------------------------------- "-T" files --*)
(* We don't need -T files any more.  This function is just here
   for backward compatibility with m3tk tools like stablegen... *)

PROCEDURE DoGenTFile (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {} =
  CONST UKImportableSet = SET OF UK { UK.I3, UK.IG, UK.MG };
  VAR
    t := Self (m);
    n_visible := 0;
    u : M3Unit.T;
    srcs: REF ARRAY OF M3Unit.T;
    map : REF ARRAY OF INTEGER;

  PROCEDURE CmpUnit (ax, bx: INTEGER): [-1 .. +1] =
    VAR a := srcs[ax];  b := srcs[bx];
    BEGIN
      IF a.loc.pkg < b.loc.pkg THEN RETURN -1; END;
      IF a.loc.pkg > b.loc.pkg THEN RETURN +1; END;
      IF a.loc.subdir < b.loc.subdir THEN RETURN -1; END;
      IF a.loc.subdir > b.loc.subdir THEN RETURN + 1; END;
      IF a.name < b.name THEN RETURN -1; END;
      IF a.name > b.name THEN RETURN +1; END;
      RETURN 0;
    END CmpUnit;

  PROCEDURE Emit (wr: Wr.T) RAISES {Wr.Failure, Thread.Alerted} =
    VAR uu: M3Unit.T;  last_loc : M3Loc.T := NIL;
    BEGIN
      FOR i := 0 TO LAST (map^) DO
        uu := srcs [map[i]];
        IF (uu.loc # last_loc) THEN
          Out (wr, "@", uu.loc.path, Wr.EOL);
          last_loc := uu.loc;
        END;
        Out (wr, M3Unit.FileName (uu), Wr.EOL);
      END;
    END Emit;

  BEGIN
    (* count the local sources *)
    u := t.units.head;
    WHILE (u # NIL) DO
      IF u.kind IN UKImportableSet AND NOT (u.hidden AND u.imported) THEN
        INC (n_visible);
      END;
      u := u.next;
    END;

    (* collect up the sources in an array *)
    srcs := NEW (REF ARRAY OF M3Unit.T, n_visible);
    map  := NEW (REF ARRAY OF INTEGER,  n_visible);
    n_visible := 0;
    u := t.units.head;
    WHILE (u # NIL) DO
      IF u.kind IN UKImportableSet AND NOT (u.hidden AND u.imported) THEN
        srcs [n_visible] := u;
        map  [n_visible] := n_visible;
        INC (n_visible);
      END;
      u := u.next;
    END;

    (* sort the local sources by directory *)
    IntArraySort.Sort (map^, CmpUnit);

    (* finally, dump the file *)
    Utils.WriteFile (M3TFile, Emit, append := FALSE);
    IF (t.mode = MM.Build) THEN InstallDerived (t, M3TFile); END;
    DeleteDeriveds (t, M3TFile, NoExtension);
  END DoGenTFile;

PROCEDURE DoDeleteFile (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  VAR t := Self (m);  file := PopText (t);
  BEGIN
    Utils.Remove (file);
  END DoDeleteFile;

PROCEDURE DoSymbolicOrHardLinkFile (link: PROCEDURE(from, to: TEXT); m: QMachine.T)
  RAISES {Quake.Error} =
  VAR t := Self (m);  src, dest, prefix: TEXT;
  BEGIN
    dest := PopText (t);
    src  := PopText (t);
    
    prefix := Env.Get("CM3_INSTALL_PREFIX");
    IF prefix # NIL AND Pathname.Absolute(src) THEN
      src := prefix & src;
      dest := prefix & dest;
    END;

    link (src, dest);
  END DoSymbolicOrHardLinkFile;

PROCEDURE DoSymbolicLinkFile (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  BEGIN
    DoSymbolicOrHardLinkFile(Utils.SymbolicLinkFile, m);
  END DoSymbolicLinkFile;

PROCEDURE DoHardLinkFile (m: QMachine.T;  <*UNUSED*> n_args: INTEGER)
  RAISES {Quake.Error} =
  BEGIN
    DoSymbolicOrHardLinkFile(Utils.HardLinkFile, m);
  END DoHardLinkFile;

(*----------------------------------------------------------- file writing --*)

PROCEDURE Out (wr: Wr.T;  a, b, c, d, e: TEXT := NIL)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (a # NIL) THEN Wr.PutText (wr, a); END;
    IF (b # NIL) THEN Wr.PutText (wr, b); END;
    IF (c # NIL) THEN Wr.PutText (wr, c); END;
    IF (d # NIL) THEN Wr.PutText (wr, d); END;
    IF (e # NIL) THEN Wr.PutText (wr, e); END;
  END Out;

PROCEDURE TouchFile (file: TEXT) =
  (* Create a new empty file *)
  BEGIN
    Utils.CloseWriter (Utils.OpenWriter (file, fatal := TRUE), file);
  END TouchFile;

(*------------------------------------------------------------------- misc --*)

PROCEDURE Self (m: QMachine.T): T =
  BEGIN
    RETURN NARROW (m, T);
  END Self;

PROCEDURE ConfigDefn (t: T;  sym: TEXT): QValue.Binding
  RAISES {Quake.Error} =
  VAR bind := t.lookup (M3ID.Add (sym));
  BEGIN    
    IF (bind = NIL) THEN ConfigErr (t, sym, "not defined"); END;
    RETURN bind;
  END ConfigDefn;

PROCEDURE GetConfigPath (t: T;  symbol: TEXT): TEXT
  RAISES {Quake.Error} =
  VAR bind := ConfigDefn (t, symbol);
      text: TEXT;
  BEGIN
    TRY
      (* M3Path.New is used to canonicalize the paths -- to remove dots.
       * Breaking through layers as we do here lets us alter "readonly" values.
       *)
      text := M3Path.New(QVal.ToText (t, bind.value));
      bind.value.int := M3ID.Add (text);
      RETURN text;
    EXCEPT Quake.Error (msg) =>
      ConfigErr (t, symbol, msg);
      RETURN "";
    END;
  END GetConfigPath;

PROCEDURE GetConfig (t: T;  symbol: TEXT): TEXT
  RAISES {Quake.Error} =
  VAR bind := ConfigDefn (t, symbol);
  BEGIN
    TRY
      RETURN QVal.ToText (t, bind.value);
    EXCEPT Quake.Error (msg) =>
      ConfigErr (t, symbol, msg);
      RETURN "";
    END;
  END GetConfig;

PROCEDURE GetConfigBool (t: T;  symbol: TEXT): BOOLEAN
  RAISES {Quake.Error} =
  VAR bind := t.lookup (M3ID.Add (symbol));
  BEGIN
    IF (bind = NIL) THEN RETURN FALSE; END;
    TRY
      RETURN QVal.ToBool (t, bind.value);
    EXCEPT Quake.Error (msg) =>
      ConfigErr (t, symbol, msg);
      RETURN FALSE;
    END;
  END GetConfigBool;

PROCEDURE ConfigErr (t: T;  symbol, msg: TEXT)
  RAISES {Quake.Error} =
  BEGIN
    t.error ("Unable to use definition of \"" & symbol
             & "\" from configuration file \"" & MxConfig.FindFile ()
             & "\": " & msg);
  END ConfigErr;

TYPE TextVector = REF ARRAY OF TEXT;

PROCEDURE SeqToTexts (t: T;  seq: QVSeq.T): TextVector
  RAISES {Quake.Error} =
  VAR
    n    := seq.size ();
    txts := NEW (TextVector, n);
  BEGIN
    FOR i := 0 TO n - 1 DO
      txts[i] := QVal.ToText (t, seq.get (i));
    END;
    RETURN txts;
  END SeqToTexts;

PROCEDURE PopArray (t: T): QVSeq.T
  RAISES {Quake.Error} =
  VAR v: QValue.T;
  BEGIN
    t.pop (v);
    RETURN QVal.ToArray (t, v);
  END PopArray;

PROCEDURE Str2ID (READONLY x: ARRAY OF CHAR): Quake.ID =
  BEGIN
    RETURN M3ID.FromStr (x);
  END Str2ID;

PROCEDURE Txt2ID (t: TEXT): Quake.ID =
  BEGIN
    RETURN M3ID.Add (t);
  END Txt2ID;

PROCEDURE ID2Txt (i: Quake.ID): TEXT =
  BEGIN
    RETURN M3ID.ToText (i);
  END ID2Txt;

BEGIN
END M3Build.

(********************************

%----------------------------------------------------------------------- M3 ---

readonly proc DO_M3(args) is
  before_do_m3_hooks()
  generate_tfile()

  if defined ("CAPTURE_M3")
    local all_args = arglist ("", [M3_CONFIG, "-make", M3OPTIONS, args,
                               M3SEARCH_TABLES, COMPILE_SOURCES,
                               IMPORT_LIBS, OTHER_LIBS_L])
    cp_if (all_args, ".M3ARGS")
  end

  local sources = arglist("-F", [M3SEARCH_TABLES, COMPILE_SOURCES,
                                 IMPORT_LIBS, OTHER_LIBS_L])
  
  if not defined ("_quiet")
    write(["m3", M3OPTIONS, args, sources, CR])
    % write([M3, M3_CONFIG, "-make", M3OPTIONS, args, sources, CR])
  end
  exec("@-" & M3, M3_CONFIG, "-make", M3OPTIONS, args, sources)
  install_derived (M3WEB)

  if not empty(PKG_OVERRIDES)
    % there were overrides => don't create any shipping information
    NoteOverrides()
  end
end

readonly proc NoteOverrides() is
  delete_file(M3SHIP_FILE)
  > M3OVERRIDES in  write (CR) end
end

%-------------------------------------------------------------------- noweb ---

readonly proc noweb(src_file,root,dest_file) is
  src_file = path_of(src_file & ".nw")
  local tmp_dest = "." & dest_file
  if defined("_all")
    if stale(tmp_dest, src_file)
      exec("notangle -L'<*LINE %L \"%F\" *>%N' -R'" & root & "'", src_file,
           ">", tmp_dest)
    end
    cp_if (tmp_dest, dest_file)
  end
  deriveds(tmp_dest, no_extension)
  deriveds(dest_file, no_extension)
end

readonly proc noweb_interface(f,r,d) is
  noweb(f,r,d & ".i3")
  derived_interface(d, HIDDEN)
end

readonly proc Noweb_interface(f,r,d) is
  noweb(f,r,d & ".i3")
  derived_interface(d, VISIBLE)
end

readonly proc noweb_implementation(f,r,d) is
  noweb(f,r,d & ".m3")
  derived_implementation(d)
end

%--------------------------------------------------------------------- Zume ---

proc zume(x) is
  write("zume is not implemented", CR)
end

********************************)
