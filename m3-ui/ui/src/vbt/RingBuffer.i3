(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:57:51 PST 1992 by muller   *)
(*      modified on Fri Sep 13  5:05:03 PDT 1991 by msm      *)
<*PRAGMA LL*>

GENERIC INTERFACE RingBuffer(Elem);
(* Elem.T must not be an open array type;
   Elem.NormalSize must be a constant positive integer. *)

(* A RingBuffer.T represents an unmonitored indefinitely expandible
   FIFO queue. *)

TYPE T = 
  RECORD 
    lo, hi: CARDINAL := 0; 
    buff: REF ARRAY OF Elem.T := NIL
  END;
  (* buff[lo..hi-1] circularly are the active entries;
     lo = hi => the queue is empty; lo # hi => buff # NIL. *)

CONST Empty = T{0, 0, NIL};

<*INLINE*> PROCEDURE IsEmpty(READONLY rb: T): BOOLEAN;
(* Return whether rb is empty. *)

PROCEDURE Insert(VAR rb: T; READONLY e: Elem.T);
(* Insert e into rb, extending rb if necessary. *)

EXCEPTION Exhausted;

PROCEDURE Remove(VAR rb: T): Elem.T RAISES {Exhausted};
(* Raise the exception if rb is empty, else remove the
   oldest element of rb and return it. *)

PROCEDURE Peek(VAR rb: T): Elem.T RAISES {Exhausted};
(* Like Remove, but leave the returned element at the head of the queue. *)

END RingBuffer. 

