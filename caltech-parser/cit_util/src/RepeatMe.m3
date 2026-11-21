MODULE RepeatMe;
IMPORT Process;
IMPORT Params;
IMPORT File;
IMPORT Thread;
IMPORT Time;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT OSError;
IMPORT AL;
IMPORT Wx;
IMPORT Watchdog;
IMPORT Usignal;

TYPE
  WDCallback = Watchdog.Callback OBJECT
    pid : Process.ID;
  OVERRIDES
    do := WDCDo;
  END;

PROCEDURE WDCDo(wdc : WDCallback) =

  PROCEDURE Kill(sig : CARDINAL) =
    BEGIN
      Debug.Out(F("RepeatMe.WDCDo : watchdog expired : killing %s -- signal %s",
                  Int(wdc.pid), Int(sig)));
      EVAL Usignal.kill(wdc.pid, sig);
    END Kill;
    
  BEGIN
    Kill(1);
    Thread.Pause(1.0d0);
    Kill(15);
    Thread.Pause(1.0d0);
    Kill(9);
  END WDCDo;
  
PROCEDURE Do(execFlag               : TEXT;
             immediateQuit          : Process.ExitCode;
             READONLY addArgs       : ARRAY OF TEXT;
             maxTime                : LONGREAL) : BOOLEAN =
  VAR
    stdin, stdout, stderr : File.T;
    params := NEW(REF ARRAY OF TEXT, Params.Count - 1 + NUMBER(addArgs) + 1);
    cmd    := Params.Get(0);
    watchdog : Watchdog.T := NIL;
  BEGIN
    (* copy the parameters over, but make params[0] the execFlag *)
    params[0] := execFlag;
    FOR i := FIRST(addArgs) TO LAST(addArgs) DO
      params[i + 1] := addArgs[i]
    END;
    FOR i := 1 TO Params.Count - 1 DO
      params[i + NUMBER(addArgs)] := Params.Get(i)
    END;
    
    Process.GetStandardFileHandles(stdin, stdout, stderr);

    TRY
      WITH proc     = Process.Create(cmd,
                                     params^,
                                     stdin := stdin,
                                     stdout := stdout,
                                     stderr := stderr) DO
        IF maxTime > 0.0d0 THEN
          watchdog := NEW(Watchdog.T).init(maxTime,
                                           callback :=
                                               NEW(WDCallback,
                                                   pid := Process.GetID(proc)));
        END;


        (* 
           if we really can't make this work any other way, we could
           add a secondary thread to do the Process.Wait and set a watchdog
           to wake up from waiting on that thread OR on a timer.  Then if
           the timer wakes us up, we simply ignore the activity in the 
           waiter thread and try to re-execute regardless of whether 
           the first attempt exits or not! (Of course, we need to TRY 
           to kill the first attempt so it at least doesn't interfere
           with subsequent attempts...)  UGH.
        *)
        
        WITH exitCode = Process.Wait(proc) DO

          IF watchdog # NIL THEN
            watchdog.kill()
          END;
        
        IF exitCode # 0 THEN
          VAR
            wx := Wx.New();
          BEGIN
            Wx.PutText(wx, cmd);
            FOR i := FIRST(params^) TO LAST(params^) DO
              Wx.PutChar(wx, ' ');
              Wx.PutText(wx, params[i])
            END;
            Debug.Warning(F("RepeatMe.Do : subprocess exit code %s, failed:\n %s",
                            Int(exitCode), Wx.ToText(wx)))
          END
        END;
        
        IF exitCode = immediateQuit AND exitCode # 0 THEN
          Debug.Error("Aborting with exitCode " & Int(exitCode), exitCode := exitCode)
        END;
        
        RETURN exitCode = 0
      END
      END
    EXCEPT
      OSError.E(x) => Debug.Warning("Process.Create failed : OSError.E : " & AL.Format(x));
      RETURN FALSE
    END
  END Do;
  
PROCEDURE Repeat(execFlag            : TEXT;
                 maxTimes            : CARDINAL;
                 delay               : Time.T;
                 immediateQuit       : Process.ExitCode;
                 READONLY addArgs    : ARRAY OF TEXT;
                 maxTime             : LONGREAL) =
  BEGIN
    FOR i := 1 TO maxTimes DO
      IF Do(execFlag, immediateQuit, addArgs, maxTime) THEN
        Process.Exit(0)
      ELSE
        Debug.Warning("Process failed.  Re-executing!");
        Thread.Pause(delay)
      END
    END;
    Debug.Error(F("Process failed maxTimes(%s), quitting with error", Int(maxTimes)))
  END Repeat;

BEGIN END RepeatMe.
