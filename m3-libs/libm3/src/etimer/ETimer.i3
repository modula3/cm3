(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ETimer.i3                                             *)
(* Last modified on Thu Dec  1 10:13:38 PST 1994 by kalsow     *)

(* This interface provides a set of non-overlapping timers.
   The timers record elapsed time, not CPU time.

   A background timer and a garbage collection timer are
   automatically provided.

   The stack of suspended timers can contain at most 100 timers.
*)

INTERFACE ETimer;

IMPORT Wr;

TYPE T <: REFANY;

PROCEDURE New (lab: TEXT): T;
(* Create and return a new timer with label "lab". *)

PROCEDURE Relabel (t: T;  lab: TEXT);
(* Assign "lab" to "t"'s label. *)

PROCEDURE Push (t: T);
(* Suspend the currently running timer, start "t". *)

PROCEDURE Pop ();
(* Suspend the currently running timer, resume the previous one. *)

PROCEDURE Dump (wr: Wr.T);
(* Dump the non-zero timers to "wr".  Note, the timers will be dumped
   in the reverse order of how they were created.  *)

PROCEDURE Elapsed (t: T): LONGREAL;
(* Returns the seconds accumulated by "t". *)

PROCEDURE TotalElapsed (): LONGREAL;
(* Returns the total elpased seconds *)

PROCEDURE Next (prev: T): T;
(* Returns the timer following "prev" in the global list of all timers.
   If "prev" is "NIL", the first timer is returned.  If "prev" is the
   last known timer, "NIL" is returned. *)

PROCEDURE Enable ();
(* Start the timers.  Note that "Enable" must be called at
   least once before any timers will begin accumulating time.  *)

PROCEDURE Reset (t: T);
(* Reset "t"'s accumulator to zero. *)

PROCEDURE ResetAll ();
(* Reset all timers to zero. *)

END ETimer.
