(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE MxGen;

IMPORT Fmt, Wr, Thread, Stdio;
IMPORT Mx, MxRep, MxMap, M3ID, Target;
IMPORT M3CG, M3CG_Ops, TInt, TargetMap;

CONST (* name-mangling done by the compiler & back-end *)
  BinderSuffix = ARRAY BOOLEAN OF TEXT { "_M3", "_I3" };
  EOL = "\n";

TYPE
  State = RECORD
    base          : Mx.LinkSet   := NIL;
    errors        : Wr.T         := NIL;
    verbose       : BOOLEAN      := FALSE;
    gui           : BOOLEAN      := FALSE;
    genC          : BOOLEAN      := FALSE;
    lazyInit      : BOOLEAN      := TRUE;
    main_units    : UnitInfo     := NIL;
    all_units     : UnitInfo     := NIL;
    top_units     : UnitInfo     := NIL;
    used_units    : UnitInfo     := NIL;
    imported_units: UnitInfo     := NIL;

    (* C output information *)
    wr            : Wr.T         := NIL;
    eol           : TEXT         := NIL;

    (* M3CG output information *)
    cg            : M3CG.T       := NIL;
  END;

TYPE
  UnitInfo = REF RECORD
    next    : UnitInfo;
    unit    : Mx.Unit;
    cg_proc : M3CG.Proc := NIL;
    binder  : TEXT;
  END;

PROCEDURE ContainsUnit (ui: UnitInfo; u: Mx.Unit): BOOLEAN =
  BEGIN
    WHILE ui # NIL DO
      IF ui.unit = u THEN RETURN TRUE END;
      ui := ui.next;
    END;
    RETURN FALSE;
  END ContainsUnit;

(*------------------------------------------------------------------------*)

PROCEDURE GenerateMain (base: Mx.LinkSet;  c_output: Wr.T;  cg_output: M3CG.T;
                        verbose: BOOLEAN;  windowsGUI: BOOLEAN;
                        lazy := FALSE) =
(* TODO Around here is duplicated by m3core.h, M3C.m3 and MxGen.m3; at
 * least consolidate to two copies by moving to m3middle
 *)
      CONST Prefix = ARRAY OF TEXT {
"",
"// Put types inside extern C because some compilers (Sun) consider",
"// such linkage as part of the type and error upon conflicts,",
"// between m3core.h and _m3main.c when they are concatenated.",
"// At least for function pointer types.",
"",
"#ifdef __cplusplus",
"extern \"C\" {",
"#endif",
"",
"#ifndef M3_TARGET64", (* see M3_TARGET64 in Target and config *)
"#define M3_TARGET64 0",
"#endif",
"",
"#if defined(_MSC_VER) || defined(__DECC) || defined(__DECCXX) || defined(__int64)",
"typedef          __int64    INT64;",
"typedef unsigned __int64   UINT64;",
"#else",
"typedef          long long  INT64;",
"typedef unsigned long long UINT64;",
"#endif",
"",
"#if __INITIAL_POINTER_SIZE == 64 || M3_TARGET64",
"typedef INT64 INTEGER;",
"#else",
"typedef ptrdiff_t INTEGER;",
"#endif",
"",
"#if !defined(_MSC_VER) && !defined(__cdecl)",
"#define __cdecl /* nothing */",
"#endif",
"",
"#ifndef ADDRESS",
"#define ADDRESS ADDRESS",
"typedef char* ADDRESS;",
"#endif",
"",
"typedef void (__cdecl*M3PROC)(void);",
"",
"#ifndef RT0__ModulePtr",
"#define RT0__ModulePtr RT0__ModulePtr",
"typedef ADDRESS RT0__ModulePtr;",
"#endif",
"",
"//correct, but match preexisting m3core",
"//void __cdecl RTLinker__InitRuntime(INTEGER argc, char** argv, char** envp, void* hinstance);",
"void __cdecl RTLinker__InitRuntime(INTEGER argc, ADDRESS argv, ADDRESS envp, ADDRESS hinstance);",
"void __cdecl RTProcess__Exit(INTEGER);",
"//correct, but workaround hash collisions in ProcType",
"//void __cdecl RTLinker__AddUnitImports(void* (__cdecl*)(void));",
"void __cdecl RTLinker__AddUnitImports(M3PROC);",
"//correct, but void __cdecl RTLinker__AddUnit(void* (__cdecl*)(void));",
"void __cdecl RTLinker__AddUnit(M3PROC);",
""
};
  VAR s: State;
  BEGIN
    <*ASSERT  (c_output = NIL) # (cg_output = NIL) *>
    s.base      := base;
    s.wr        := c_output;
    s.cg        := cg_output;
    s.verbose   := verbose;
    s.errors    := Stdio.stdout;
    s.gui       := windowsGUI;
    s.genC      := (s.wr # NIL);
    s.lazyInit  := lazy;

    IF verbose THEN
      INC(debugLevel);
    END;

    IF s.genC THEN
      Out (s, "#include <stddef.h>", EOL);
      IF (s.gui) THEN
        Out (s, "#include <windows.h>", EOL);
      END;

      FOR i := FIRST(Prefix) TO LAST(Prefix) DO
        Out (s, Prefix[i] & "\n");
      END;

    ELSE
      GenCGTypeDecls (s);
    END;

    IF lazy THEN
      ImportMain (s);
    ELSE
      ImportTopUnits (s);
    END;

    IF s.genC THEN
      Out (s, EOL, "#ifdef __cplusplus", EOL);
      Out (s, "} /* extern \"C\" */", EOL);
      Out (s, "#endif", EOL, EOL);
      GenerateCEntry (s)
    ELSE
      GenerateCGEntry (s);
    END;

  END GenerateMain;

(*------------------------------------------------------------------------*)

PROCEDURE GenCGTypeDecls (VAR s: State) =
  VAR
    interface    : M3CG.Var;
    struct_align := MAX (Target.Structure_size_boundary, Target.Address.align)
                      DIV Target.Byte; (* == min structure alignment (bytes)*)
  BEGIN
    s.cg.begin_unit ();
    s.cg.set_source_file ("_m3main.mc");
    s.cg.set_source_line (1);

    (* we only need to declare a global segment so the gcc-based backend
       can pick up the unit name, "_m3main". But then, the native x86
       backend requires that it be explicitly initialized. *)
    interface := s.cg.declare_segment (M3ID.Add ("MM__m3main"), 0, FALSE);
    s.cg.bind_segment (interface, Target.Address.bytes, struct_align,
                       Target.CGType.Struct, FALSE, TRUE);
    s.cg.begin_init (interface);
    s.cg.end_init (interface);
  END GenCGTypeDecls;

(*------------------------------------------------------------------------*)

PROCEDURE ImportMain (VAR s: State) =
  VAR
    main  := M3ID.Add ("Main");
    units := MxMap.GetData (s.base.modules);
    u     : Mx.Unit;
  BEGIN
    s.main_units := NIL;

    (* find the modules exporting "Main" *)
    FOR i := 0 TO LAST (units^) DO
      u := units[i].value;
      IF (u # NIL) THEN
        FOR i := u.exported_units.start
              TO u.exported_units.start + u.exported_units.cnt - 1 DO
          IF (u.info[i] = main) THEN
            s.main_units := NEW (UnitInfo, unit := u, next := s.main_units);
            ImportUnit (s, s.main_units);
            EXIT;
          END;
        END;
      END;
    END;

    IF s.main_units = NIL THEN
      Err (s, "No module implements \"Main\".", EOL);
    END;
  END ImportMain;

PROCEDURE ImportTopUnits (VAR s: State) =
  VAR
    main  := M3ID.Add ("Main");
    mods  := s.base.modules;
    units := MxMap.GetData (mods);
    u, v  : Mx.Unit;
    ui    : UnitInfo;
    found : BOOLEAN;
  BEGIN
    s.main_units := NIL;

    FOR i := 0 TO LAST (units^) DO
      u := units[i].value;
      IF (u # NIL) THEN
        Debug (2, UnitName (u));
        FOR i := u.exported_units.start
              TO u.exported_units.start + u.exported_units.cnt - 1 DO
          IF (u.info[i] = main) THEN
            s.main_units := NEW (UnitInfo, unit := u, next := s.main_units);
            s.main_units.binder := UnitName (s.main_units.unit);
            EXIT;
          END;
        END;
        IF u.imported_units.cnt > 0 THEN
          found := FALSE;
          FOR i := u.imported_units.start
            TO u.imported_units.start + u.imported_units.cnt - 1 DO
            v := MxMap.Get (mods, u.info[i]);
            IF v # NIL AND v # u THEN
              Debug (2, "  imports ", UnitName (v));
              found := TRUE;
              IF NOT ContainsUnit (s.imported_units, v) THEN
                s.imported_units := 
                    NEW (UnitInfo, unit := v, next := s.imported_units);
              END;
            END;
          END;
        END;
        IF u.used_modules.cnt > 0 THEN
          found := FALSE;
          FOR i := u.used_modules.start
            TO u.used_modules.start + u.used_modules.cnt - 1 DO
            v := MxMap.Get (mods, u.info[i]);
            IF v # NIL AND v # u THEN
              Debug (2, "  uses ", UnitName (v));
              found := TRUE;
              IF NOT ContainsUnit (s.used_units, v) THEN
                s.used_units := 
                    NEW (UnitInfo, unit := v, next := s.used_units);
              END;
            END;
          END;
        END;
        s.all_units := NEW (UnitInfo, unit := u, next := s.all_units);
      END;
    END;

    FOR i := 0 TO LAST (units^) DO
      u := units[i].value;
      IF (u # NIL) THEN
        Debug (3, "checking unit ", UnitName (u));
        IF NOT ContainsUnit (s.imported_units, u) AND 
          NOT ContainsUnit (s.used_units, u) AND
          NOT ContainsUnit (s.main_units, u) THEN
          Debug (3, "  --> not used ==> top unit");
          s.top_units := NEW (UnitInfo, unit := u, next := s.top_units);
          s.top_units.binder := UnitName (s.top_units.unit);
        ELSE
          Debug (3, "  --> used");
        END;
      END;
    END;

    DumpUnits("Main Units:", s.main_units);
    DumpUnits("Imported Units:", s.imported_units);
    DumpUnits("Used Units:", s.used_units);
    DumpUnits("Top Units:", s.top_units);

    (* concatenate main and other top units *)
    (*
    ui := s.top_units;
    IF ui # NIL THEN
      WHILE ui.next # NIL DO
        ui := ui.next;
      END;
      ui.next := s.main_units;
      s.main_units := s.top_units;
    END;
    *)

    (* import all top units different from Main *)
    ui := s.top_units;
    WHILE ui # NIL DO
      ImportUnit (s, ui);
      ui := ui.next;
    END;

    (* import all main units *)
    ui := s.main_units;
    WHILE ui # NIL DO
      ImportUnit (s, ui);
      ui := ui.next;
    END;
    IF s.main_units = NIL THEN
      Err (s, "No module implements \"Main\".", EOL);
    END;
  END ImportTopUnits;

PROCEDURE ImportUnit (VAR s: State;  ui: UnitInfo) =
  VAR u := ui.unit;
  BEGIN
    ui.binder := UnitName (u);
    IF s.genC THEN
      (* This content should match m3c output so the files can be concatenated. *)
      Out (s, "RT0__ModulePtr __cdecl ", ui.binder, "(INTEGER mode);\n");
    ELSE
      ui.cg_proc := s.cg.import_procedure (M3ID.Add (ui.binder), 1,
                                          Target.CGType.Addr,
                                          Target.DefaultCall);
      EVAL DeclareParam (s, "mode",  Target.Integer.cg_type);
    END;
  END ImportUnit;

PROCEDURE UnitName (u: Mx.Unit): TEXT =
  BEGIN
    IF u = NIL THEN RETURN "NIL" END;
    RETURN M3ID.ToText (u.name) & BinderSuffix [u.interface];
  END UnitName;

PROCEDURE DumpUnits (h: TEXT; units: UnitInfo) =
  VAR n := 1; nstr: TEXT;
  BEGIN
    IF debugLevel < 1 THEN RETURN END;
    Debug (1, h);
    WHILE (units # NIL) DO
      IF units.binder = NIL THEN
        units.binder := UnitName (units.unit);
      END;
      nstr := Fmt.F("%4s ", Fmt.Int(n));
      INC(n);
      Debug (1, nstr, units.binder);
      units := units.next;
    END;
    Debug (1);
  END DumpUnits;

(*------------------------------------------------------------------------*)

PROCEDURE GenerateCEntry (VAR s: State) =

  PROCEDURE GenAddUnits(ui: UnitInfo) =
    BEGIN
      WHILE (ui # NIL) DO
        Out (s, "  RTLinker__AddUnit ((M3PROC)", ui.binder, ");", EOL);
        ui := ui.next;
      END;
    END GenAddUnits;

  PROCEDURE GenAddUnitImports(ui: UnitInfo) =
    BEGIN
      IF s.lazyInit THEN RETURN END;
      WHILE (ui # NIL) DO
        Out (s, "  RTLinker__AddUnitImports ((M3PROC)", ui.binder, ");\n");
        ui := ui.next;
      END;
    END GenAddUnitImports;

  BEGIN
    Out (s, EOL);
    IF (s.gui) THEN
      Out (s, "int WINAPI ");
      Out (s, "WinMain (HINSTANCE self, HINSTANCE prev, PSTR args, int mode)", EOL);
      Out (s, "{", EOL);
      Out (s, "  RTLinker__InitRuntime (-1/*argc*/, (ADDRESS)args, (ADDRESS)GetEnvironmentStringsA(), (ADDRESS)self);\n");
    ELSE
      Out (s, "int main (int argc, char** argv, char** envp)", EOL);
      Out (s, "{", EOL);
      Out (s, "  RTLinker__InitRuntime (argc, (ADDRESS)argv, (ADDRESS)envp, 0);\n");
    END;

    GenAddUnitImports(s.main_units);
    GenAddUnits(s.top_units);
    GenAddUnits(s.main_units);

    Out (s, "  RTProcess__Exit (0);", EOL);
    Out (s, "  return 0;", EOL);
    Out (s, "}", EOL, EOL);
  END GenerateCEntry;

(*------------------------------------------------------------------------*)

PROCEDURE GenerateCGEntry (VAR s: State) =
  VAR
    main      : M3CG.Proc;
    run_proc  : M3CG.Proc;
    link_proc : M3CG.Proc;
    link_proc2: M3CG.Proc;
    exit_proc : M3CG.Proc;
    getenv    : M3CG.Proc;
    winapi    : Target.CallingConvention;
    argv      : M3CG.Var;
    argc      : M3CG.Var;
    envp      : M3CG.Var;
    self      : M3CG.Var;
    prev      : M3CG.Var;
    mode      : M3CG.Var;
    src_line  : INTEGER := 2;
    int_t     := Target.Integer.cg_type;
    addr_t    := Target.CGType.Addr;

  PROCEDURE GenAddUnits(ui: UnitInfo) =
    BEGIN
      WHILE (ui # NIL) DO
        s.cg.set_source_line (src_line);  INC (src_line);
        s.cg.start_call_direct (link_proc, 0, Target.CGType.Void);
        s.cg.load_procedure (ui.cg_proc);
        s.cg.pop_param (addr_t);
        s.cg.call_direct (link_proc, Target.CGType.Void);
        ui := ui.next;
      END;
    END GenAddUnits;

  PROCEDURE GenAddUnitImports(ui: UnitInfo) =
    BEGIN
      IF s.lazyInit THEN RETURN END;
      WHILE (ui # NIL) DO
        s.cg.set_source_line (src_line);  INC (src_line);
        s.cg.start_call_direct (link_proc2, 0, Target.CGType.Void);
        s.cg.load_procedure (ui.cg_proc);
        s.cg.pop_param (addr_t);
        s.cg.call_direct (link_proc2, Target.CGType.Void);
        ui := ui.next;
      END;
    END GenAddUnitImports;

  BEGIN
    run_proc := s.cg.import_procedure (M3ID.Add ("RTLinker__InitRuntime"), 4,
                                       Target.CGType.Void, Target.DefaultCall);
    EVAL DeclareParam (s, "argc", int_t);
    EVAL DeclareParam (s, "argv", addr_t);
    EVAL DeclareParam (s, "envp", addr_t);
    EVAL DeclareParam (s, "instance", addr_t);

    link_proc := s.cg.import_procedure (M3ID.Add ("RTLinker__AddUnit"), 1,
                                       Target.CGType.Void, Target.DefaultCall);
    EVAL DeclareParam (s, "m", addr_t);

    IF NOT s.lazyInit THEN
      link_proc2 := s.cg.import_procedure (M3ID.Add ("RTLinker__AddUnitImports"),
                                           1, Target.CGType.Void,
                                           Target.DefaultCall);
      EVAL DeclareParam (s, "m", addr_t);
    END;

    exit_proc := s.cg.import_procedure (M3ID.Add ("RTProcess__Exit"), 1,
                                       Target.CGType.Void, Target.DefaultCall);
    EVAL DeclareParam (s, "n", int_t);

    IF (s.gui) THEN
      (*
         #include <windows.h>
         PSTR WINAPI GetEnvironmentStringsA(void);
      *)
      winapi := Target.FindConvention ("WINAPI");
      getenv := s.cg.import_procedure (M3ID.Add ("GetEnvironmentStringsA"), 0, 
                                       addr_t, winapi);

      (* int WINAPI WinMain(HINSTANCE self, HINSTANCE prev, PSTR args, int mode) *)
      main := s.cg.declare_procedure (M3ID.Add ("WinMain"), (*n_params*) 4,
                                 int_t, (*lev*) 0, winapi, TRUE, NIL);
      self := DeclareParam (s, "self", addr_t);
      prev := DeclareParam (s, "prev", addr_t);
      argv := DeclareParam (s, "args", addr_t);
      mode := DeclareParam (s, "mode", int_t);
      s.cg.begin_procedure(main);

      (* int argc;   void *envp;  *)
      argc := DeclareLocal (s, "argc", int_t);
      envp := DeclareLocal (s, "envp", addr_t);

      (* argc = -1; *)
      s.cg.set_source_line (src_line);  INC (src_line);
      s.cg.load_integer (int_t, TInt.MOne);
      s.cg.store (argc, 0, int_t, int_t);

      (* envp = (_ADDRESS)GetEnvironmentStringsA(); *)
      s.cg.set_source_line (src_line);  INC (src_line);
      s.cg.start_call_direct (getenv, 0, addr_t);
      s.cg.call_direct (getenv, addr_t);
      s.cg.store (envp, 0, addr_t, addr_t);

    ELSE (* not GUI *)
      (*
        int main (int argc, char **argv, char **envp)
      *)
      main := s.cg.declare_procedure (M3ID.Add("main"), (*n_params*) 3,
                                      int_t, (*lev*) 0,
                                      Target.DefaultCall, TRUE, NIL);
      argc := DeclareParam (s, "argc", int_t);
      argv := DeclareParam (s, "argv", addr_t);
      envp := DeclareParam (s, "envp", addr_t);
      s.cg.begin_procedure(main);

      (* void * self; *)
      self := DeclareLocal (s, "self", addr_t);
      
      (* self = 0; *)
      s.cg.set_source_line (src_line);  INC (src_line);
      s.cg.load_integer (int_t, TInt.Zero);
      s.cg.store (self, 0, int_t, int_t);
    END; (* if GUI *)

    (* RTLinker__InitRuntime (argc, argv, envp, self); *)
    s.cg.set_source_line (src_line);  INC (src_line);
    s.cg.start_call_direct (run_proc, 0, Target.CGType.Void);
    IF Target.DefaultCall.args_left_to_right THEN
      s.cg.load (argc, 0, int_t, int_t);        (* argc *)
      s.cg.pop_param (int_t);
      s.cg.load (argv, 0, addr_t, addr_t);      (* argv *)
      s.cg.pop_param (addr_t);
      s.cg.load (envp, 0, addr_t, addr_t);      (* envp *)
      s.cg.pop_param (addr_t);
      s.cg.load (self, 0, addr_t, addr_t);      (* self *)
      s.cg.pop_param (addr_t);
    ELSE
      s.cg.load (self, 0, addr_t, addr_t);      (* self *)
      s.cg.pop_param (addr_t);
      s.cg.load (envp, 0, addr_t, addr_t);      (* envp *)
      s.cg.pop_param (addr_t);
      s.cg.load (argv, 0, addr_t, addr_t);      (* argv *)
      s.cg.pop_param (addr_t);
      s.cg.load (argc, 0, int_t,  int_t);       (* argc *)
      s.cg.pop_param (int_t);
    END;
    s.cg.call_direct (run_proc, Target.CGType.Void);

    GenAddUnitImports(s.main_units);
    GenAddUnits(s.top_units);
    GenAddUnits(s.main_units);

    (*  RTProcess.Exit (0); *)
    s.cg.set_source_line (src_line);  INC (src_line);
    s.cg.start_call_direct (exit_proc, 0, Target.CGType.Void);
    s.cg.load_integer (int_t, TInt.Zero);
    s.cg.pop_param (int_t);
    s.cg.call_direct (exit_proc, Target.CGType.Void);

    (*  return 0; *)
    s.cg.set_source_line (src_line);  INC (src_line);
    s.cg.load_integer (int_t, TInt.Zero);
    s.cg.exit_proc (int_t);
    s.cg.end_procedure (main);
    s.cg.end_unit ();
  END GenerateCGEntry;

PROCEDURE DeclareParam (VAR s: State;  nm: TEXT;  tipe: Target.CGType): M3CG.Var =
  BEGIN
    RETURN s.cg.declare_param (M3ID.Add (nm), TargetMap.CG_Bytes[tipe],
                               TargetMap.CG_Align_bytes [tipe],
                               tipe, (*typeUID*) 0, (*in_memory*) FALSE,
                               (*up_level*) FALSE, (*frequency*) M3CG.Always);
  END DeclareParam;

PROCEDURE DeclareLocal (VAR s: State;  nm: TEXT;  tipe: Target.CGType): M3CG.Var =
  BEGIN
    RETURN s.cg.declare_local (M3ID.Add (nm), TargetMap.CG_Bytes[tipe],
                               TargetMap.CG_Align_bytes [tipe],
                               tipe, (*typeUID*) 0, (*in_memory*) FALSE,
                               (*up_level*) FALSE, (*frequency*) M3CG.Always);
  END DeclareLocal;

(*------------------------------------------------------------------------*)

PROCEDURE Err (VAR s: State;  a, b, c, d: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    IF (s.errors = NIL) THEN RETURN END;
    IF (a # NIL) THEN Wr.PutText (s.errors, a); END;
    IF (b # NIL) THEN Wr.PutText (s.errors, b); END;
    IF (c # NIL) THEN Wr.PutText (s.errors, c); END;
    IF (d # NIL) THEN Wr.PutText (s.errors, d); END;
  END Err;

PROCEDURE Out (VAR s: State;  a, b, c, d: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    IF (a # NIL) THEN Wr.PutText (s.wr, a) END;
    IF (b # NIL) THEN Wr.PutText (s.wr, b) END;
    IF (c # NIL) THEN Wr.PutText (s.wr, c) END;
    IF (d # NIL) THEN Wr.PutText (s.wr, d) END;
  END Out;

PROCEDURE Debug (level: INTEGER; a, b, c, d: TEXT := NIL) =
  BEGIN
    IF debugLevel >= level THEN
      Msg (a, b, c, d);
    END;
  END Debug;

PROCEDURE Msg (a, b, c, d: TEXT := NIL) =
  <*FATAL Wr.Failure, Thread.Alerted*>
  BEGIN
    WITH wr = Stdio.stdout DO
      IF (a # NIL) THEN Wr.PutText (wr, a) END;
      IF (b # NIL) THEN Wr.PutText (wr, b) END;
      IF (c # NIL) THEN Wr.PutText (wr, c) END;
      IF (d # NIL) THEN Wr.PutText (wr, d) END;
      Wr.PutText(wr, EOL);
    END;
  END Msg;

BEGIN
  debugLevel := 0;
END MxGen.

