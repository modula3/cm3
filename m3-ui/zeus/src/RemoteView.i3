(* Copyright 1994 Digital Equipment Corporation.                             *)
(* Distributed only by permission.                                           *)
(* Last modified on Fri Jun  3 14:14:07 PDT 1994 by mhb                      *)
(*      modified on Fri Jun  3 11:00:36 PDT 1994 by heydon                   *)

INTERFACE RemoteView;

IMPORT NetObj, Thread;

(* A "RemoteView.T" is a network-object version of a "View.T".  This
   interface is under active development for Juno views, so it is
   subject to change. *)

EXCEPTION Error(TEXT);

TYPE
  T = NetObj.T BRANDED "RemoteView" OBJECT
      METHODS
        startrun () RAISES {NetObj.Error, Thread.Alerted};
        endrun   () RAISES {NetObj.Error, Thread.Alerted};
        event (tfactor: REAL; nm, args: TEXT)
               RAISES {Error, NetObj.Error, Thread.Alerted}
      END;

(* The "startrun" and "endrun" methods must be called at the
   start and end of each run of the animation.

   The "event" method invokes the output-event named "nm" with
   arguments "args".  The "args" string should be a symbolic
   expression such that "Sx.Read(TextRd.New(args))" will not
   fail.  The "tfactor" is the amount of time that a one-second
   animation within an event should actually take in terms of
   real seconds (on the wall clock).  The "Error" exception is
   raised with a descriptive error message if there was an error
   parsing the arguments or invoking the named event on those
   arguments. *)

END RemoteView.
