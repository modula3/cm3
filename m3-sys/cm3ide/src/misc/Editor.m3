MODULE Editor;

IMPORT Thread, LineWr, Wr, Quake, QMachine, QValue;
IMPORT ConfigItem, Default, ErrLog;

TYPE
  EditClosure = Thread.Closure OBJECT
    file  : TEXT;
    line  : TEXT;
    mach  : Quake.Machine;
    proc  : QValue.T;
  OVERRIDES
    apply := DoEdit;
  END;

PROCEDURE Run (file, line: TEXT) =
  BEGIN
    EVAL Thread.Fork (NEW (EditClosure, file := file, line := line));
  END Run;

PROCEDURE DoEdit (cl: EditClosure): REFANY =
  VAR
    sav_echo : BOOLEAN;
    sav_wr   : Wr.T;
    wr       := LineWr.New (DumpLine, NIL);
  BEGIN
    TRY
      Default.GetConfigProc (ConfigItem.T.Edit_file, cl.mach, cl.proc);
      IF (cl.mach = NIL) THEN RETURN NIL; END;
      sav_echo := cl.mach.exec_echo (ConfigItem.X[ConfigItem.T.Verbose_log].bool);
      sav_wr := cl.mach.cur_wr ();
      cl.mach.set_wr (wr);
      cl.mach.start_call (cl.proc);
      QMachine.PushText (cl.mach, cl.file);
      QMachine.PushText (cl.mach, cl.line);
      cl.mach.call_proc (n_args := 2, isFunc := FALSE);
      cl.mach.set_wr (sav_wr);
      EVAL cl.mach.exec_echo (sav_echo);
    EXCEPT
    | Thread.Alerted =>
        LineWr.Clear (wr);
        ErrLog.Msg ("** interrupted while editing ", cl.file);
    | Quake.Error (msg) =>
        LineWr.Clear (wr);
        ErrLog.Msg ("** error while editing ", cl.file, ": ", msg);
    END;

    (* dump the output to the error log *)
    LineWr.Clear (wr);

    RETURN NIL;
  END DoEdit;

PROCEDURE DumpLine (<*UNUSED*> ref: REFANY;  line: TEXT) =
  BEGIN
    ErrLog.Msg (line);
  END DumpLine;

BEGIN
END Editor.


