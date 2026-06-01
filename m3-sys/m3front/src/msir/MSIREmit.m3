MODULE MSIREmit;

IMPORT MSIR, MSIRPrinter, MSIRVerifier, MSIRToLLVM, MSIRBuilder, M3ID, RTParams, Target, Text, FileWr, Stdio, Wr, Thread, OSError, Word;
IMPORT TextExpr, M3String, M3WString;
IMPORT RunTyme, Error, Fmt;

<*FATAL Thread.Alerted, Wr.Failure*>

VAR
  enabled:    BOOLEAN     := FALSE;
  enabledSet: BOOLEAN     := FALSE;
  curModule:  MSIR.Module := NIL;
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
    MSIR.ModuleSetIsInterface(curModule, isInterface);
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

PROCEDURE CurrentModule(): MSIR.Module =
  BEGIN RETURN curModule END CurrentModule;

PROCEDURE RegisterImport(binder: TEXT) =
  BEGIN
    IF curModule = NIL THEN RETURN END;
    (* RTHooks is always pre-initialised by RTLinker__InitRuntime before any
       module body runs; it must not appear in the module's import chain or
       the linker pulls in RTHooks_m.o which conflicts with raise_stub.cpp. *)
    IF Text.Equal(binder, "RTHooks_I3") OR Text.Equal(binder, "RTHooks_M3") THEN
      RETURN;
    END;
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

PROCEDURE EndUnit() =
  VAR wr: Wr.T;  path: TEXT;  errs: REF ARRAY OF TEXT;
  BEGIN
    IF curModule = NIL THEN RETURN END;
    (* Register hook procs now — RTHooks is available after full compilation
       of the unit (imports resolved).  BeginUnit is too early: the Assert
       in RunTyme.LookUpProc fires if hooks = NIL (RTHooks not yet seen). *)
    MSIR.SetModuleHooks(curModule,
      MSIRBuilder.HookProc(RunTyme.Hook.CheckLoadTracedRef),
      MSIRBuilder.HookProc(RunTyme.Hook.CheckStoreTraced),
      MSIRBuilder.HookProc(RunTyme.Hook.ScanTypecase));
    (* Populate text literals from TextExpr's per-module registry.
       TextExpr.SetUID tracks literals during compilation; we transfer
       them here so MSIRToLLVM (in the msir package) can emit the LLVM globals
       without importing TextExpr (which would create a circular dependency).
       Split8/Split32 on the stored expression give chars and cnt. *)
    FOR uid := 0 TO TextExpr.LiteralCount() - 1 DO
      VAR e := TextExpr.LiteralExpr(uid);
          s8: M3String.T;  s32: M3WString.T;
          chars: TEXT;  cnt: INTEGER;
      BEGIN
        IF TextExpr.Split8(e, s8) THEN
          chars := M3String.ToText(s8);
          cnt   := M3String.Length(s8);
        ELSE
          (* Wide-char literal: encode each WIDECHAR as little-endian bytes.
             MSIRToLLVM detects cnt<0 and emits the appropriately-sized struct. *)
          EVAL TextExpr.Split32(e, s32);
          VAR wlen      := M3WString.Length(s32);
              wcharBytes := Target.WideCharSize() DIV Target.Char.size;
              byteArr    := NEW(REF ARRAY OF CHAR, wlen * wcharBytes);
          BEGIN
            FOR k := 0 TO wlen - 1 DO
              VAR cp := M3WString.GetChar(s32, k);
              BEGIN
                FOR b := 0 TO wcharBytes - 1 DO
                  byteArr[k * wcharBytes + b] :=
                    VAL(Word.And(Word.RightShift(cp, b * 8), 16_FF), CHAR);
                END;
              END;
            END;
            chars := Text.FromChars(byteArr^);
            cnt   := - wlen;
          END;
        END;
        EVAL MSIR.ModuleAddTextLit(curModule, chars, cnt);
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
