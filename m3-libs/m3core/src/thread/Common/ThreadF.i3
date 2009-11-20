(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri Apr  7 09:11:10 PDT 1995 by kalsow     *)
(*      modified on Fri May 14 16:18:01 PDT 1993 by mjordan    *)
(*      modified on Mon Apr  5 14:50:26 PDT 1993 by muller     *)
(*      modified on Mon Jul  6 16:43:19 PDT 1992 by muller     *)

INTERFACE ThreadF;

(*--------------------------------------------------- showthreads support ---*)

TYPE
  State = {
        alive    (* can run *),
        waiting  (* waiting for a condition via Wait *),
        locking  (* waiting for a mutex to be unlocked *),
        pausing  (* waiting until some time is arrived *),
        blocking (* waiting for some IO *),
        dying    (* done, but not yet joined *),
        dead     (* done and joined *)
	};

(*-------------------------------------------------------------- identity ---*)

TYPE
  Id = INTEGER;

PROCEDURE MyId(): Id RAISES {};
(* return Id of caller *)

(*------------------------------------------------------------ preemption ---*)

PROCEDURE SetSwitchingInterval (usec: CARDINAL);
(* Sets the time between thread preemptions to 'usec' microseconds.
   This procedure is a no-op with kernel threads. *)

END ThreadF.
