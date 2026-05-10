MODULE MSIREmit;

IMPORT MSIR, MSIRPrinter, MSIRVerifier, MSIRToLLVM, MSIRBuilder, M3ID, RTParams, Target, Text, FileWr, Stdio, Wr, Thread, OSError;
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
