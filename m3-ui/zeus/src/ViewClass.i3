(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Thu Feb 11 17:52:23 PST 1993 by johnh  *)
(*      modified on Tue May 12 08:37:40 1992 by mhb        *)

INTERFACE ViewClass;

IMPORT Thread, View;

TYPE
  TT = View.Public BRANDED OBJECT
        name      : TEXT               := NIL;
        alertable : BOOLEAN;
        evtHandler: Thread.T;
        evtCond   : Thread.Condition;
        evtArg    : REFANY;
        evtHandled: BOOLEAN;
        evtHQuit  : BOOLEAN := FALSE;
      END;

(* The "name" field is set by Zeus immediately after a "View.T" has been
   instantiated.  Thereafter, the field is read-only. *)
(* The "alertable" field is set by Zeus immediately after a "View.T" has
   been instantiated.  Thereafter, the field is read-only.  This field
   indicates whether the view is willing to receive alerts to tell it to
   stop processing the current event. *)
(* The "evtHandler" field is a worker thread tickled by Zeus to process
   each output event when it occurs.  evtCond and evtArg are used in the
   coroutines for output event procesing.*)
(* The "evtHandled" field is set to FALSE by the default output event
   methods.  User-provided event handlers will not see this field, and
   hence will not set it.  The event dispatcher uses this field to decide
   whether an event was handled by any view. *)
(* The "evtHQuit" field is used by Zeus to tell the evtHandler thread to
   terminate. *)

REVEAL View.T <: TT;

<*PRAGMA LL*>

PROCEDURE Activate (v: View.T; on: BOOLEAN);
  <* LL = VBT.mu *>
  (* Activate the view if on is TRUE; o/w set it to a gray texture. *)

END ViewClass.

