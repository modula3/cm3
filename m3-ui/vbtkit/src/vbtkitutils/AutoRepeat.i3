(* Copyright (C) 1992, Digital Equipment Corporation                        *)
(* All rights reserved.                                                     *)
(* See the file COPYRIGHT for a full description.                           *)
(*                                                                          *)
(* Last modified on Fri Jun 11 23:01:25 PDT 1993 by meehan                  *)
(*      modified on Mon Feb 15 10:21:36 PST 1993 by mhb                     *)
(*      modified on Tue Jun 16 12:54:53 PDT 1992 by muller                  *)
(*      modified on Tue Feb 12 13:44:21 PST 1991 by chan                    *)
(*      modified on Wed Dec 26 11:03:59 PST 1990 by brooks                  *)

(* The "AutoRepeat" interface provides support for calling a procedure
   repetitively.  Auto-repeating typically takes place while a key or
   mouse button is held down, although there is no direct relation
   between "AutoRepeat" and "VBT"s.

   When an auto-repeat object "ar" is activated, it forks a {\it timer
   thread\/} that calls "ar.repeat()" after "firstWait" milliseconds,
   and every "period" milliseconds thereafter.  However, there is a
   flow-control mechanism: if the call to "ar.repeat()" has not
   returned by the time the next repetition is scheduled to take
   place, the timer thread will wait.  That is, repetitions cannot
   queue up more than one deep.

   An auto-repeat object "ar" is activated by a call to "Start(ar)",
   terminated by a call to "Stop(ar)", and resumed by a call to
   "Continue(ar)".

   All locking is handled within "AutoRepeat"; calls to "Start(ar)",
   "Stop(ar)", and "Continue(ar)" are serialized on a per-"ar" basis.
   These procedures may be called by a "repeat" method.  Clients must
   not call the "repeat" method directly; it is called by the timer
   thread subject to client-calls to "Start", "Stop", and "Continue".
   The "AutoRepeat" interface will never call a "repeat" method
   re-entrantly. *)

INTERFACE AutoRepeat;

TYPE 
  Milliseconds = CARDINAL;

CONST
  DefaultFirstWait: Milliseconds = 500;
  DefaultPeriod   : Milliseconds = 66;

TYPE
  T <: Public;
  Public =
    Private OBJECT
    METHODS
      init (firstWait: Milliseconds := DefaultFirstWait;
            period   : Milliseconds := DefaultPeriod): T;
      repeat ();
      canRepeat(): BOOLEAN;
    END;
  Private <: ROOT;

(* The call "ar.init(firstWait, period)" initializes "ar" as an
   "AutoRepeat.T", and it returns "ar".  The "firstWait" and
   "period" parameters are stored internally for use by the
   "Start" and "Continue" procedures.

   The call "ar.canRepeat" should return "FALSE" whenever there's
   reason to suspect that a client might want to call "Stop" in
   the near future.  The next call to "ar.repeat" will be
   suspended for "period" milliseconds.  The default for
   this method always returns "TRUE".

   The "canRepeat" method is intended for situations when a
   "repeat" method takes more time than "period"
   milliseconds to complete.  The problem with slow "repeat"
   methods is that the scheduler might decide to always run the
   timer thread (since it will want to call the "repeat" method
   as soon as the slow "repeat" method completes), thereby
   blocking another thread from being able to call "Stop".

   The default "repeat" method is a no-op. *)

PROCEDURE Start (ar: T);
(* Initiate auto-repeating for "ar". *)

(* "Start(ar)" forks a timer thread that will wait "ar.firstWait"
   milliseconds before calling "ar.repeat()" the first time, then
   "ar.period" milliseconds between subsequent calls to "ar.repeat()".
   This procedure is a no-op if "ar" is already running. *)

PROCEDURE Stop (ar: T);
(* Stop auto-repeating as soon as possible. *)

(* After calling "Stop(ar)", the implementation will not call
   "ar.repeat()" again until a call to "Start(ar)" or "Continue(ar)"
   restarts auto-repeating.  This procedure is a no-op if "ar" is not
   already running.

   It is possible (but unlikely) that "ar.repeat()" is called one more
   time after a call to "Stop(ar)" returns.  This can happen because
   calls to "ar.repeat" are not serialized with respect to the call to
   "Stop(ar)".  They are not serialized in order to allow a "repeat"
   method to call "Stop". *)

PROCEDURE Continue (ar: T);
(* Resume auto-repeating immediately. *)

(* "Continue(ar)" is like "Start(ar)", except rather than waiting
   "ar.firstWait" milliseconds as in the call to "Start(ar)", the
   timer thread calls "ar.repeat" without waiting at all.  Subsequent
   calls to "ar.repeat()" happen every "period" milliseconds, as
   usual.  This procedure is a no-op if "ar" is already running. *)

END AutoRepeat.

