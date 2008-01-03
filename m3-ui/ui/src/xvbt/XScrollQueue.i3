(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
(* modified on Wed Sep 11 15:53:09 PDT 1991 by msm *)
<*PRAGMA LL*>

INTERFACE XScrollQueue;         (* = RingBuffer(XScrollQElt) *)
IMPORT PaintPrivate;

TYPE
  T = RECORD
        lo, hi: CARDINAL                            := 0;
        buff  : REF ARRAY OF PaintPrivate.ScrollRec := NIL
      END;
(* buff[lo..hi-1] circularly are the active entries; lo = hi => the queue
   is empty; lo # hi => buff # NIL. *)

CONST Empty = T{0, 0, NIL};

<*INLINE*> PROCEDURE IsEmpty (READONLY rb: T): BOOLEAN;
(* Return whether rb is empty. *)

PROCEDURE Insert (VAR rb: T; READONLY e: PaintPrivate.ScrollRec);
(* Insert e into rb, extending rb if necessary. *)

EXCEPTION Exhausted;

PROCEDURE Remove (VAR rb: T): PaintPrivate.ScrollRec RAISES {Exhausted};
(* Raise the exception if br is empty, else remove the oldest element of rb
   and return it. *)

END XScrollQueue.

