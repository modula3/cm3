(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Tue Aug 25 14:56:57 PDT 1992 by johnh  *)
(*      modified on Tue May 12 08:35:48 1992 by mhb        *)

INTERFACE AlgorithmClass;

IMPORT Algorithm, FormsVBT;

<*PRAGMA LL*>

TYPE
  T = Algorithm.Public BRANDED OBJECT
        name            : TEXT         := "";
        stopAtEvent     : BOOLEAN      := FALSE;
        waitAtEvent     : CARDINAL     := 1;
        stopatCodeEvents: BOOLEAN      := TRUE;
        waitatCodeEvents: CARDINAL     := 1;
        eventData       : FormsVBT.T;
        evtHandled      : BOOLEAN;
        evtMu           : MUTEX;
      METHODS
        <* LL = VBT.mu *>
        updateEventCounts (reset: BOOLEAN);
      END;

(* The "name" field is set by Zeus immediately after an "Algorithm.T" has
   been instantiated.  Thereafter, the field is read-only. *)
(* The "eventData" field is a FormsVBT.T that ZeusPanel inserts into the
   Session control panel, just like the public "data" field.  The
   "eventData" form is used to set the stop/wait information about
   individual events; the automatically-generated <Foo>AlgClass module
   creates the form in its init() method and maintains the event data. *)
(* The stopAtEvent and waitAtEvent fields control whether to stop after the
   current output event, and how long to wait if you do.  These variables
   are set by #(_ALGNAME_)IE and read by ZeusPanel.  (Could be returned by
   procedure calls, but aren't (for efficiency?).) *)
(* The "evtHandled" field is set to FALSE by the default feedback event
   methods.  User-provided event handlers will not see this field, and
   hence will not set it.  The event dispatcher uses this field to decide
   whether an event was handled by the algorithm. *)
(* The "evtMu" MUTEX is locked by the IE output event procs.  It prevents
   output events from occurring simultaneously, and hence protects the view
   event-handling threads. *)
(* The "updateEventCounts" method is called to refresh the displayed counts
   of output events.  If reset = TRUE, all the counts are reset to 0.
   Whether or not the counts are reset, they are written into the EventData
   form. *)

REVEAL Algorithm.T <: T;
END AlgorithmClass.

