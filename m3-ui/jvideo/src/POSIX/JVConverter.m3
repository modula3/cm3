(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Sun Feb 19 18:47:02 PST 1995 by msm      *)
(*      modified on Tue Jan 31 10:59:54 PST 1995 by kalsow   *)
(*      modified on Thu Sep  2 16:08:52 PDT 1993 by sfreeman *)

MODULE JVConverter;

IMPORT IO, JVBuffer, JVConverterF, Process, Stdio, Thread;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init  := Init;
        start := StartCrash;
        stop  := StopCrash;

        join       := Join;
        leave      := Leave;
        setPaused  := SetPaused;
        getOutput  := GetOutput;
        close      := CloseCrash;
        startStats := StartStatsCrash;
        stopStats  := StopStats;
        getStats   := GetStats;
      END;

PROCEDURE Init (t: T): T =
  BEGIN
    t.pauseEvent := NEW(Thread.Condition);
    RETURN t;
  END Init;

PROCEDURE StartCrash (<*UNUSED*> t: T) =
  BEGIN
    Process.Crash("JVConverter.Start not defined");
  END StartCrash;

PROCEDURE StopCrash (<*UNUSED*> t: T) =
  BEGIN
    Process.Crash("JVConverter.Stop not defined");
  END StopCrash;

PROCEDURE Join (t: T) RAISES {Error, Thread.Alerted} =
  BEGIN
    LOCK t DO
      INC(t.clients);
      Thread.Broadcast(t.pauseEvent);
      IF t.clients = 1 THEN t.start(); END;
    END;
  END Join;

PROCEDURE Leave (t: T) RAISES {Error, Thread.Alerted} =
  BEGIN
    LOCK t DO
      <* ASSERT t.clients > 0 *>
      DEC(t.clients);
      Thread.Broadcast(t.pauseEvent);
      IF t.clients = 0 THEN t.stop(); END;
    END;
  END Leave;

PROCEDURE SetPaused (t: T; paused := FALSE) =
  BEGIN
    LOCK t DO
      IF paused THEN
        <* ASSERT t.paused < t.clients *>
        INC(t.paused);
      ELSE
        <* ASSERT t.paused > 0 *>
        DEC(t.paused);
        Thread.Broadcast(t.pauseEvent);
      END;
    END;
  END SetPaused;

PROCEDURE GetOutput (t: T): JVBuffer.Pool =
  BEGIN
    RETURN t.output;
  END GetOutput;

PROCEDURE CloseCrash (<*UNUSED*> t: T) =
  BEGIN
    Process.Crash("JVConverter.Close not defined");
  END CloseCrash;

PROCEDURE StartStatsCrash (<*UNUSED*> t: T) =
  BEGIN
    Process.Crash("JVConverter.StartStats not defined");
  END StartStatsCrash;

PROCEDURE StopStats (t: T) =
  BEGIN
    LOCK t DO t.statistics := NIL; END;
  END StopStats;

PROCEDURE GetStats (t: T): Statistics =
  BEGIN
    RETURN t.statistics;
  END GetStats;

(* -- Error reporting -- *)
PROCEDURE StderrReport(<* UNUSED*> e: ErrorReporter; msg: TEXT) =
  BEGIN
    IO.Put(msg & "\n", Stdio.stderr);
  END StderrReport;

VAR
  mutex:= NEW(MUTEX);
  reporter: ErrorReporter := NIL;

PROCEDURE RegisterErrorReporter (er: ErrorReporter): ErrorReporter =
  VAR oldRep: ErrorReporter;
  BEGIN
    LOCK mutex DO oldRep := reporter; reporter := er; END;
    RETURN oldRep;
  END RegisterErrorReporter;

PROCEDURE ReportError (msg: TEXT) =
  BEGIN
    LOCK mutex DO
      IF msg = NIL THEN Process.Crash("no message\n"); END;
      IF reporter = NIL THEN Process.Crash(msg & "\n"); END;
      reporter.report(msg);
    END;
  END ReportError;

BEGIN
  toStderr := NEW(ErrorReporter, report := StderrReport);
END JVConverter.
