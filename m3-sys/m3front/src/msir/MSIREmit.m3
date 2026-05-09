MODULE MSIREmit;

IMPORT MSIR, MSIRPrinter, M3ID, RTParams, FileWr, Wr, Thread, OSError;

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
  VAR txt: TEXT;
  BEGIN
    IF NOT IsEnabled() THEN RETURN END;
    txt := M3ID.ToText(name);
    IF txt = NIL THEN txt := "<anonymous>" END;
    curModule := MSIR.NewModule(txt);
  END BeginUnit;

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
  VAR wr: Wr.T;  path: TEXT;
  BEGIN
    IF curModule = NIL THEN RETURN END;
    path := MSIR.ModuleName(curModule) & ".msir";
    TRY
      wr := FileWr.Open(path);
      MSIRPrinter.Module(wr, curModule);
      Wr.Close(wr);
    EXCEPT
      OSError.E => (* best-effort: skip if we can't write *)
    END;
    curModule := NIL;
  END EndUnit;

BEGIN
END MSIREmit.
