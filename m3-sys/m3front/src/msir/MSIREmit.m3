MODULE MSIREmit;

IMPORT MSIR, MSIRPrinter, MSIRVerifier, MSIRToLLVM, MSIRToM3CG, MSIRBuilder, M3ID, RTParams, Target, Text, FileWr, Stdio, Wr, Thread, OSError;
IMPORT RunTyme, Error, Fmt, Host, MSIRChecks;

<*FATAL Thread.Alerted, Wr.Failure*>

VAR
  enabled:    BOOLEAN     := FALSE;
  enabledSet: BOOLEAN     := FALSE;
  curModule:  MSIR.Module := NIL;
  lastModule: MSIR.Module := NIL;  (* last unit that verified; see LastModule *)
  llOutPath:  TEXT        := NIL;

  (* Names of units emitted as a MODULE (non-interface) in this compilation.
     A MODULE and its same-named INTERFACE both default their .ll filename to
     <ModuleName>.ll; the module's emission (body + <Mod>_M3 binder + info) is
     authoritative.  When an EXPORTS clause forces the interface to be
     RE-compiled after the module (e.g. MODULE A EXPORTS Main with INTERFACE A),
     its later interface-only emission would clobber the module's .ll and drop
     <Mod>_M3 — an MSIR-FAIL link error.  Skip writing a redundant interface
     .ll once its module has been emitted. *)
  emittedModuleNames: REF ARRAY OF TEXT := NIL;
  nEmittedModules:    INTEGER := 0;

PROCEDURE SetLLOutPath(path: TEXT) =
  BEGIN
    llOutPath := path;
  END SetLLOutPath;

PROCEDURE IsEnabled(): BOOLEAN =
  BEGIN
    IF NOT enabledSet THEN
      enabled := RTParams.IsPresent("m3front-msir")
              OR (Target.BackendMode IN Target.BackendMSIRSet);
      enabledSet := TRUE;
    END;
    RETURN enabled;
  END IsEnabled;

PROCEDURE BeginUnit(name: M3ID.T;  isInterface: BOOLEAN := FALSE) =
  VAR txt, triple, datalayout: TEXT;
  BEGIN
    IF NOT IsEnabled() THEN RETURN END;
    txt := M3ID.ToText(name);
    IF txt = NIL THEN txt := "<anonymous>" END;
    curModule := MSIR.NewModule(txt);
    lastModule := NIL;
    MSIR.ModuleSetIsInterface(curModule, isInterface);
    (* RT0.ModuleInfo.gc_flags, as Module.m3 sets it for the C payload. *)
    MSIR.SetModuleGcFlags(curModule,
      ORD(Host.doGenGC) * 1 + ORD(Host.doIncGC) * 2);
    triple := NIL;  datalayout := NIL;
    IF Target.System_name # NIL THEN
      IF Text.Equal(Target.System_name, "ARM64_DARWIN") THEN
        triple     := "arm64-apple-macosx11.0";
        datalayout := "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32";
      ELSIF Text.Equal(Target.System_name, "AMD64_DARWIN") THEN
        triple     := "x86_64-apple-macosx10.15";
        datalayout := "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128";
      ELSIF Text.Equal(Target.System_name, "AMD64_LINUX") THEN
        triple     := "x86_64-unknown-linux-gnu";
        datalayout := "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128";
      END;
    END;
    IF triple # NIL THEN MSIR.SetModuleTarget(curModule, triple, datalayout) END;
    MSIRBuilder.BeginModule();
  END BeginUnit;

VAR driverLowers := FALSE;

PROCEDURE SetDriverLowers(on: BOOLEAN) =
  BEGIN driverLowers := on END SetDriverLowers;

PROCEDURE LastModule(): MSIR.Module =
  VAR m := lastModule;
  BEGIN
    lastModule := NIL;
    RETURN m;
  END LastModule;

PROCEDURE CurrentModule(): MSIR.Module =
  BEGIN RETURN curModule END CurrentModule;

PROCEDURE RegisterImport(binder: TEXT) =
  BEGIN
    IF curModule = NIL THEN RETURN END;
    (* Keep every import the front end records, RTHooks included, as the C
       backend does.  The RTHooks link is what makes the linker pull in the
       RTHooks interface object, whose typecells (REF INTEGER among them)
       other units' type links resolve to; skipping it left p165's
       TYPECODE(REF INTEGER) link unresolved ("A compile-time type is
       missing").  The skip dated from the stub-runtime link test. *)
    MSIR.ModuleAddImportBinder(curModule, binder);
  END RegisterImport;

PROCEDURE AddProc(p: MSIR.Proc) =
  BEGIN
    IF curModule = NIL THEN RETURN END;
    MSIR.ModuleAddProc(curModule, p);
  END AddProc;

PROCEDURE NoteSkipped(procName: TEXT;  reason: TEXT) =
  VAR wr: Wr.T;
  BEGIN
    TRY wr := FileWr.OpenAppend("/tmp/msir-debug.txt"); EXCEPT ELSE RETURN END;
    TRY
      Wr.PutText(wr, procName & ": " & reason & "\n");
      Wr.Close(wr);
    EXCEPT ELSE END;
  END NoteSkipped;

CONST RtHookOf = ARRAY MSIR.RtHook OF RunTyme.Hook {
  RunTyme.Hook.Abort,            (* ReportFault *)
  RunTyme.Hook.CheckIsType,
  RunTyme.Hook.RaiseEx,
  RunTyme.Hook.NewTracedRef,  RunTyme.Hook.NewUntracedRef,
  RunTyme.Hook.NewTracedObj,  RunTyme.Hook.NewUntracedObj,
  RunTyme.Hook.NewTracedArray, RunTyme.Hook.NewUntracedArray };

PROCEDURE UsedBarriers (m: MSIR.Module;
                        VAR loads, stores, typecases, loadBlocks: BOOLEAN) =
  (* Does m contain a traced load (read barrier: CheckLoadTracedRef), a
     barriered store or dirty (write barrier: CheckStoreTraced), a
     TYPECASE (ScanTypecase), or a block read barrier
     (CheckLoadTracedBlock)? *)
  VAR p: MSIR.Proc;  b: MSIR.Block;  i: MSIR.Insn;
  BEGIN
    FOR pi := 0 TO MSIR.ModuleProcCount(m) - 1 DO
      p := MSIR.ModuleProc(m, pi);
      FOR bi := 0 TO MSIR.ProcBlockCount(p) - 1 DO
        b := MSIR.ProcBlock(p, bi);
        FOR ii := 0 TO MSIR.BlockInsnCount(b) - 1 DO
          i := MSIR.BlockInsn(b, ii);
          CASE MSIR.InsnOp(i) OF
          | MSIR.Op.GcLoad   => loads := TRUE;
          | MSIR.Op.GcDirty  => stores := TRUE;
          | MSIR.Op.GcLoadBlock => loadBlocks := TRUE;
          | MSIR.Op.GcStore  =>
              IF MSIR.InsnOperandCount(i) = 3 THEN stores := TRUE END;
          | MSIR.Op.Typecase => typecases := TRUE;
          ELSE
          END;
        END;
      END;
    END;
  END UsedBarriers;

PROCEDURE EndUnit() =
  VAR wr: Wr.T;  path: TEXT;  errs: REF ARRAY OF TEXT;
  BEGIN
    IF curModule = NIL THEN RETURN END;
    (* Register hook procs now — RTHooks is available after full compilation
       of the unit (imports resolved).  BeginUnit is too early: the Assert
       in RunTyme.LookUpProc fires if hooks = NIL (RTHooks not yet seen).
       Only the hooks the unit's code calls: looking a hook up declares it,
       which records an import of it in the unit's link information, as the
       C walk does only where it emits a call. *)
    VAR loads, stores, typecases, loadBlocks := FALSE;
        gcLoad, gcStore, scanTC: MSIR.Proc := NIL;
    BEGIN
      UsedBarriers (curModule, loads, stores, typecases, loadBlocks);
      (* the runtime procedures MSIRChecks expands ops into (D32-D34) *)
      VAR need := MSIRChecks.Hooks(curModule);
      BEGIN
        FOR h := FIRST(MSIR.RtHook) TO LAST(MSIR.RtHook) DO
          IF h IN need THEN
            MSIR.SetModuleRtHook(curModule, h, MSIRBuilder.HookProc(RtHookOf[h]));
          END;
        END;
      END;
      IF loadBlocks THEN
        MSIR.SetModuleLoadBlockHook(curModule,
          MSIRBuilder.HookProc(RunTyme.Hook.CheckLoadTracedBlock));
      END;
      IF loads THEN
        gcLoad := MSIRBuilder.HookProc(RunTyme.Hook.CheckLoadTracedRef);
      END;
      IF stores THEN
        gcStore := MSIRBuilder.HookProc(RunTyme.Hook.CheckStoreTraced);
      END;
      IF typecases THEN
        scanTC := MSIRBuilder.HookProc(RunTyme.Hook.ScanTypecase);
      END;
      MSIR.SetModuleHooks(curModule, gcLoad, gcStore, scanTC);
    END;
    (* The EH lowerings (MSIREH, MSIRToM3CG) re-raise through
       RTHooks.ResumeRaiseEx only in a procedure that MSIR.ProcMayReraise:
       import it for those, as C does. *)
    VAR scoped, checked, reraises := FALSE;  p: MSIR.Proc;
    BEGIN
      FOR i := 0 TO MSIR.ModuleProcCount(curModule) - 1 DO
        p := MSIR.ModuleProc(curModule, i);
        IF MSIR.ProcScopeCount(p) > 1 THEN scoped := TRUE END;
        IF MSIR.ProcChecksRaises(p) THEN checked := TRUE END;
        IF MSIR.ProcMayReraise(p) THEN reraises := TRUE END;
      END;
      IF reraises THEN
        MSIR.SetModuleResumeRaise(curModule,
          MSIRBuilder.HookProc(RunTyme.Hook.ResumeRaiseEx));
      END;
      IF (scoped OR checked) AND NOT Target.Has_stack_walker THEN
        (* setjmp frames, and the frames that enforce RAISES clauses, for
           lowering to M3CG's ex_frame scheme *)
        MSIR.SetModuleFrameHooks(curModule,
          MSIRBuilder.HookProc(RunTyme.Hook.PushEFrame),
          MSIRBuilder.HookProc(RunTyme.Hook.PopEFrame));
      END;
    END;
    (* Register the five TextLiteral vtable method procs so MSIRToLLVM can
       use the correct RTHooks__TextLit* names derived from m3front's view
       of RTHooks rather than hardcoding them in the lowering pass. *)
    IF MSIR.ModuleTextLitCount(curModule) > 0 THEN
      VAR tlhooks: ARRAY [0..4] OF MSIR.Proc;
      BEGIN
        tlhooks[0] := MSIRBuilder.HookProc(RunTyme.Hook.TextLitInfo);
        tlhooks[1] := MSIRBuilder.HookProc(RunTyme.Hook.TextLitGetChar);
        tlhooks[2] := MSIRBuilder.HookProc(RunTyme.Hook.TextLitGetWideChar);
        tlhooks[3] := MSIRBuilder.HookProc(RunTyme.Hook.TextLitGetChars);
        tlhooks[4] := MSIRBuilder.HookProc(RunTyme.Hook.TextLitGetWideChars);
        MSIR.ModuleSetTextLitHooks(curModule, tlhooks);
      END;
    END;

    path := MSIR.ModuleName(curModule) & ".msir";
    TRY
      wr := FileWr.Open(path);
      MSIRPrinter.Module(wr, curModule);
      Wr.Close(wr);
    EXCEPT
      OSError.E => (* best-effort *)
    END;
    IF RTParams.IsPresent("msirrbaudit") THEN
      (* Incremental read-barrier completeness audit: report every traced-ref
         acquisition that bypasses gc.load.  Informational (stderr), never
         fails the build — run with @M3msirrbaudit and grep 'rb-audit:'. *)
      VAR aud := MSIRVerifier.AuditReadBarriers(curModule);
      BEGIN
        IF aud # NIL THEN
          FOR i := 0 TO LAST(aud^) DO
            Wr.PutText(Stdio.stderr,
              "rb-audit: " & MSIR.ModuleName(curModule) & ": " & aud[i] & "\n");
          END;
          TRY Wr.Flush(Stdio.stderr) EXCEPT ELSE END;
        END;
      END;
    END;
    IF RTParams.IsPresent("msircensus") THEN
      (* D25 spike: which procedures an MSIR -> M3CG lowering could take
         today.  Informational; run with @M3msircensus, grep 'msir-census:'. *)
      MSIRToM3CG.Census(Stdio.stderr, curModule);
      TRY Wr.Flush(Stdio.stderr) EXCEPT ELSE END;
    END;
    errs := MSIRVerifier.VerifyModule(curModule);
    IF errs # NIL THEN
      FOR i := 0 TO LAST(errs^) DO
        Wr.PutText(Stdio.stderr, "msir-verify: " & errs[i] & "\n");
      END;
      (* In MSIRObj/MSIRAsm mode the MSIR lowering IS the object code, so a
         verifier failure means the emitted .ll is malformed — fail the build
         rather than silently shipping broken modules.  In parallel emission
         (@M3m3front-msir, backend = C) the C output is authoritative, so verify
         errors stay informational. *)
      IF Target.BackendMode IN Target.BackendMSIRSet THEN
        Error.Msg ("MSIR verification failed for " & MSIR.ModuleName(curModule)
                   & " (" & Fmt.Int(NUMBER(errs^)) & " error(s); first: "
                   & errs[0] & ")");
      END;
    END;
    IF errs = NIL THEN lastModule := curModule ELSE lastModule := NIL END;
    IF driverLowers AND lastModule # NIL THEN
      IF llOutPath = NIL AND NOT MSIR.ModuleIsInterface(curModule) THEN
        RecordEmittedModule(MSIR.ModuleName(curModule));
      END;
      curModule := NIL;  llOutPath := NIL;
      RETURN;
    END;
    IF llOutPath # NIL THEN
      path := llOutPath;
    ELSE
      path := MSIR.ModuleName(curModule) & ".ll";
    END;
    (* Skip a redundant INTERFACE emission whose same-named MODULE was already
       emitted to the same default path: the module's .ll is authoritative
       (it has the body and <Mod>_M3 binder), and an interface recompile
       triggered by EXPORTS must not clobber it.  Only applies to the default
       <ModuleName>.ll path (llOutPath = NIL). *)
    IF llOutPath = NIL
       AND MSIR.ModuleIsInterface(curModule)
       AND ModuleAlreadyEmitted(MSIR.ModuleName(curModule)) THEN
      curModule := NIL;  llOutPath := NIL;  RETURN;
    END;
    TRY
      wr := FileWr.Open(path);
      (* forRuntime selects runtime-owned TypeLink resolution (no harness ctor:
         the linked CM3 runtime's RTLinker.ResolveTypeLinks walks the defn chain
         and registers TypeCells).  True for MSIRObj/MSIRAsm self-hosting.  In
         parallel @M3m3front-msir emission (backend = C) the default is FALSE
         (a runtime-less standalone harness, e.g. the LLVM smoke test, needs the
         MSIR_InitTypeLinks ctor to populate TypeCells).  '@M3m3front-msir-forruntime'
         forces it TRUE so a parallel-mode .ll can be linked against the REAL
         runtime (the conformance harness) without the harness ctor clobbering
         the defn next-pointers the runtime walks. *)
      MSIRToLLVM.Module(wr, curModule,
        forRuntime := (Target.BackendMode IN Target.BackendMSIRSet)
                   OR RTParams.IsPresent("m3front-msir-forruntime"));
      Wr.Close(wr);
      (* Record a successfully-written MODULE so a later same-named interface
         recompile (EXPORTS) won't clobber it. *)
      IF llOutPath = NIL AND NOT MSIR.ModuleIsInterface(curModule) THEN
        RecordEmittedModule(MSIR.ModuleName(curModule));
      END;
    EXCEPT
      OSError.E => (* best-effort *)
    ELSE
        (* MSIRToLLVM raised an unchecked exception (e.g. ASSERT failure).
           Log to stderr and continue; the .ll file may be incomplete. *)
        Wr.PutText(Stdio.stderr, "msir: MSIRToLLVM.Module crashed for "
          & MSIR.ModuleName(curModule) & "\n");
    END;
    curModule  := NIL;
    llOutPath  := NIL;
  END EndUnit;

PROCEDURE ModuleEmitted(name: TEXT): BOOLEAN =
  BEGIN RETURN ModuleAlreadyEmitted(name) END ModuleEmitted;

PROCEDURE ModuleAlreadyEmitted(name: TEXT): BOOLEAN =
  BEGIN
    FOR i := 0 TO nEmittedModules - 1 DO
      IF Text.Equal(emittedModuleNames[i], name) THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END ModuleAlreadyEmitted;

PROCEDURE RecordEmittedModule(name: TEXT) =
  BEGIN
    IF ModuleAlreadyEmitted(name) THEN RETURN END;
    IF emittedModuleNames = NIL THEN
      emittedModuleNames := NEW(REF ARRAY OF TEXT, 64);
    ELSIF nEmittedModules >= NUMBER(emittedModuleNames^) THEN
      VAR bigger := NEW(REF ARRAY OF TEXT, 2 * NUMBER(emittedModuleNames^));
      BEGIN
        FOR i := 0 TO nEmittedModules - 1 DO bigger[i] := emittedModuleNames[i] END;
        emittedModuleNames := bigger;
      END;
    END;
    emittedModuleNames[nEmittedModules] := name;
    INC(nEmittedModules);
  END RecordEmittedModule;

BEGIN
END MSIREmit.
