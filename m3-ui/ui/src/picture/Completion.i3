(* Copyright (C) 1992, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Fri Oct 28 17:20:28 PDT 1994 by kalsow   *)
(*      modified on Wed Oct  6 09:17:22 PDT 1993 by sfreeman *)

INTERFACE Completion;

IMPORT Picture, Thread;

(* a Completion is a mechanism for notifying the caller when an Image has
   been processed.  Each user of the image should increment the Completion
   when it starts using the image and decrement when it has finished.  When
   the count returns to 0, if /freeProc/ # NIL then the Completion.T will
   be put on a queue for /freeProc/ to be called and those threads blocked
   in /waitUntilFree/ to be signalled to continue.  /Dispose/ will be
   called on the T after /freeProc/ has returned.  If /freeProc/ = NIL,
   then the threads blocked in /waitForFree/ will be signalled
   immediately *)

CONST
  Brand = "Completion";

TYPE
  T <: Public;
  Public = MUTEX OBJECT
           METHODS
             init (initialCount                   := 1;
                   freeProc    : Picture.FreeProc := NIL;
                   freeParam   : REFANY           := NIL  ): T;
             id  (): CARDINAL;
             inc ();
             dec ();

             isFree (): BOOLEAN;
             (* non-blocking check to see if Completion is free *)
             waitUntilFree () RAISES {Thread.Alerted};
             (* block thread until Completion is freed *)
           END;

(* get T from free list.  do not forget to call t.init() and Dispose(t) on
   Ts got from this procedure *)
PROCEDURE New (): T;
PROCEDURE Dispose (t: T);        (* return to the free list *)

END Completion.
