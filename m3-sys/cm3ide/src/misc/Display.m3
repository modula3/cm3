MODULE Display;

IMPORT (*** Pathname, ***) Process, (*** Text, ***) Thread, Wr;
IMPORT Quake, QMachine, QValue, QVal;
IMPORT ConfigItem, Default, ErrLog, LineWr;
IMPORT Fmt;

CONST
  Browser = ConfigItem.Desc [ConfigItem.T.Start_browser].name;

TYPE
  BrowseClosure = Thread.Closure OBJECT
    url   : TEXT;
    mach  : Quake.Machine;
    proc  : QValue.T;
  OVERRIDES
    apply := DoBrowse;
  END;

PROCEDURE Start () =
  VAR
    start     := Default.server_href;
(***
    user_home := ConfigItem.X [ConfigItem.T.Homepage].text;
***)
  BEGIN
(***
    IF (user_home # NIL) AND Text.Length (user_home) > 0 THEN
      start := start & "user/" & Pathname.Last (user_home);
    END;
***)
    EVAL Thread.Fork (NEW (BrowseClosure, url := start));
  END Start;

PROCEDURE DoBrowse (cl: BrowseClosure): REFANY =
  VAR
    sav_echo : BOOLEAN;
    sav_wr   : Wr.T;
    wr       := LineWr.New (DumpLine, NIL);
    v        : QValue.T;
    shutdown : BOOLEAN;
  BEGIN
    ErrLog.Msg ("calling ", Browser, "(", cl.url & ")");
    TRY
      Default.GetConfigProc (ConfigItem.T.Start_browser, cl.mach, cl.proc);
      IF (cl.mach = NIL) THEN RETURN NIL; END;
      sav_echo := cl.mach.exec_echo (ConfigItem.X [ConfigItem.T.Verbose_log].bool);
      sav_wr := cl.mach.cur_wr ();
      cl.mach.set_wr (wr);
      cl.mach.start_call (cl.proc);
      QMachine.PushText (cl.mach, cl.url);
      cl.mach.call_proc (n_args := 1, isFunc := TRUE);
      cl.mach.pop (v);
      shutdown := QVal.ToBool (cl.mach, v);
      IF ConfigItem.X [ConfigItem.T.Verbose_log].bool THEN
        ErrLog.Msg (Browser, "() returned ", Fmt.Bool (shutdown));
      END; 
      IF shutdown THEN 
        ErrLog.Msg 
          ("CM3-IDE is shutting down because ", Browser, "() returned TRUE.");
      END; 
      cl.mach.set_wr (sav_wr);
      EVAL cl.mach.exec_echo (sav_echo);
    EXCEPT
    | Thread.Alerted =>
        LineWr.Clear (wr);
        ErrLog.Msg ("** interrupted while running ", Browser, "()");
        shutdown := FALSE;
    | Quake.Error (msg) =>
        LineWr.Clear (wr);
        ErrLog.Msg ("** error while running ", Browser, "() : ", msg);
        shutdown := FALSE;
    END;

    (* dump the output to the error log *)
    LineWr.Clear (wr);

    IF (shutdown) THEN
      Process.Exit(0);
    END;

    RETURN NIL;
  END DoBrowse;

PROCEDURE DumpLine (<*UNUSED*> ref: REFANY;  line: TEXT) =
  BEGIN
    ErrLog.Msg (line);
  END DumpLine;

BEGIN
END Display.
