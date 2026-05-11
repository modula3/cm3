MODULE MSIREmit;

IMPORT MSIR, MSIRPrinter, MSIRVerifier, MSIRToLLVM, MSIRBuilder, M3ID, RTParams, Target, Text, FileWr, Stdio, Wr, Thread, OSError;
IMPORT TextExpr, M3String, M3WString;
IMPORT RunTyme;

<*FATAL Thread.Alerted, Wr.Failure*>

VAR
  enabled:    BOOLEAN     := FALSE;
  enabledSet: BOOLEAN     := FALSE;
  curModule:  MSIR.Module := NIL;

PROCEDURE IsEnabled(): BOOLEAN =
  BEGIN
    IF NOT enabledSet THEN
      enabled := RTParams.IsPresent("m3front-msir");
      enabledSet := TRUE;
    END;
    RETURN enabled;
  END IsEnabled;

PROCEDURE BeginUnit(name: M3ID.T) =
  VAR txt, triple, datalayout: TEXT;
  BEGIN
    IF NOT IsEnabled() THEN RETURN END;
    txt := M3ID.ToText(name);
    IF txt = NIL THEN txt := "<anonymous>" END;
    curModule := MSIR.NewModule(txt);
    triple := NIL;  datalayout := NIL;
    IF Target.System_name # NIL THEN
      IF Text.Equal(Target.System_name, "ARM64_DARWIN") THEN
        triple     := "arm64-apple-macosx";
        datalayout := "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32";
      ELSIF Text.Equal(Target.System_name, "AMD64_DARWIN") THEN
        triple     := "x86_64-apple-macosx";
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
          EVAL TextExpr.Split32(e, s32);
          chars := M3WString.ToLiteral(s32);
          cnt   := - M3WString.Length(s32);
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

    errs := MSIRVerifier.VerifyModule(curModule);
    IF errs # NIL THEN
      FOR i := 0 TO LAST(errs^) DO
        Wr.PutText(Stdio.stderr, "msir-verify: " & errs[i] & "\n");
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
    path := MSIR.ModuleName(curModule) & ".ll";
    TRY
      wr := FileWr.Open(path);
      MSIRToLLVM.Module(wr, curModule);
      Wr.Close(wr);
    EXCEPT
      OSError.E => (* best-effort *)
    END;
    curModule := NIL;
  END EndUnit;

BEGIN
END MSIREmit.
