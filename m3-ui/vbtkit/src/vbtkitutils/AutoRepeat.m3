(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jul  9 21:33:39 PDT 1993 by mhb                      *)
(*      modified on Fri Jun 11 23:01:24 PDT 1993 by meehan                   *)
(*      modified on Tue Jun 16 12:55:10 PDT 1992 by muller                   *)
(*      modified on Fri Feb 28 00:14:11 1992 by steveg                       *)
(*      modified on Wed Mar  6 11:16:56 PST 1991 by brooks                   *)
(*      modified on Tue Feb 12 13:44:22 PST 1991 by chan                     *)
(*      modified on Mon Sep 24 12:08:26 PDT 1990 by mcjones                  *)
<* PRAGMA LL                                                                 *>

MODULE AutoRepeat;

IMPORT Time, Thread;

(* This code needs an overhaul.  It could be simplified to take
   advantage of two features of the current interface:

   1) The repeat method can be called by the timer thread rather
   than from a thread forked by the timer thread. 

   2) The Start and Continue are no-ops if ar is already running.
   (The code was written when Start and Continue tried to change
   the timing of the existing timer thread.)

   *)

REVEAL
  T = Public BRANDED OBJECT
        timerMutex    : MUTEX;
        timerCondition: Thread.Condition;
        repeater      : RepeatClosure      := NIL;
        <* LL = timerMutex *>
        timerWait, firstWait, period: Time.T;
        (* current wait, initial wait, next wait *)
        timer: Thread.T := NIL;  (* auto-repeat timer thread *)
        timerSet, doRepeat, repeating := FALSE;
        (* running?, time to repeat?, repeater forked? *)
      OVERRIDES
        init      := Init;
        canRepeat := CanRepeat;
        apply     := Timer;
        repeat    := Repeat
      END;

REVEAL Private = Thread.Closure BRANDED OBJECT END;

TYPE
  RepeatClosure =
    Thread.Closure OBJECT cl: T OVERRIDES apply := Repeater END;

PROCEDURE Init (cl       : T;
                firstWait: Milliseconds := DefaultFirstWait;
                period   : Milliseconds := DefaultPeriod     ): T =
  BEGIN
    cl.timerMutex := NEW (MUTEX);
    cl.timerCondition := NEW (Thread.Condition);
    cl.repeater := NEW (RepeatClosure, cl := cl);
    LOCK cl.timerMutex DO
       (* milliseconds -> seconds *)
      cl.firstWait := FLOAT (firstWait, Time.T) / 1.0D3;
      cl.period := FLOAT (period, Time.T) / 1.0D3;
    END;
    RETURN cl
  END Init;
     
PROCEDURE CanRepeat(<* UNUSED *> cl: T): BOOLEAN =
  BEGIN RETURN TRUE END CanRepeat;

PROCEDURE Start (cl: T) =
  BEGIN
    LOCK cl.timerMutex DO
      IF cl.timerSet THEN RETURN END;
      cl.timerWait := cl.firstWait;
      ContinueWithTimerLocked(cl)
    END
  END Start;

PROCEDURE Stop (cl: T) =
  BEGIN
    LOCK cl.timerMutex DO cl.timerSet := FALSE END
  END Stop;

PROCEDURE Continue (cl: T) =
  BEGIN
    LOCK cl.timerMutex DO
      IF cl.timerSet THEN RETURN END;
      cl.timerWait := 0.0D0;
      ContinueWithTimerLocked(cl);
    END
  END Continue;

PROCEDURE ContinueWithTimerLocked (cl: T) =
  BEGIN
    cl.timerSet := TRUE;
    IF cl.timer = NIL THEN
      cl.timer := Thread.Fork(cl)
    ELSE
      Thread.Alert(cl.timer)
    END
  END ContinueWithTimerLocked;

PROCEDURE Timer (cl: T): REFANY =
  VAR
    wait: Time.T;
    set : BOOLEAN;
  BEGIN
    LOOP
      TRY
        LOCK cl.timerMutex DO
          wait := cl.timerWait;
          set := cl.timerSet;
          cl.timerWait := cl.period
        END;
        IF set THEN
          IF wait # 0.0D0 THEN Thread.AlertPause (wait) END
        ELSE
          Thread.AlertPause (10.0D0);
          (* kill thread if unused for 10 sec. *)
          LOCK cl.timerMutex DO
            IF NOT cl.timerSet THEN
              cl.timer := NIL;
              Thread.Signal (cl.timerCondition);
              RETURN NIL
            END
          END
        END;
        IF cl.canRepeat () THEN
          LOCK cl.timerMutex DO
            IF cl.timerSet THEN
              WHILE cl.doRepeat DO
                Thread.AlertWait (cl.timerMutex, cl.timerCondition)
              END;
              IF NOT cl.repeating THEN
                EVAL Thread.Fork (cl.repeater);
                cl.repeating := TRUE
              END;
              cl.doRepeat := TRUE;
              Thread.Signal (cl.timerCondition)
            END
          END
        END
      EXCEPT
      | Thread.Alerted =>        (* good, the wait was broken *)
      END
    END
  END Timer;

PROCEDURE Repeater (rcl: RepeatClosure): REFANY =
  <* LL = {} *>
  VAR cl: T := rcl.cl;
  BEGIN
    LOOP
      LOCK cl.timerMutex DO
        WHILE cl.timer # NIL AND NOT cl.doRepeat DO
          Thread.Wait (cl.timerMutex, cl.timerCondition)
        END;
        IF cl.timer = NIL THEN
          cl.doRepeat := FALSE;
          cl.repeating := FALSE;
          RETURN NIL
        END;
        cl.doRepeat := FALSE
      END;
      Thread.Signal (cl.timerCondition);
      cl.repeat ()
    END;
  END Repeater;

PROCEDURE Repeat (<* UNUSED *> cl: T) =
  BEGIN
  END Repeat;

BEGIN
END AutoRepeat.
