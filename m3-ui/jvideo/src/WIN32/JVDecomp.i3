(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Thu Sep  7 08:56:22 PDT 1995 by najork                   *)
(*       Created on Wed Sep  6 16:19:37 PDT 1995 by najork                   *)


INTERFACE JVDecomp;

IMPORT Tick;

TYPE 
  T = MUTEX OBJECT END;                               (* used in VideoVBT.i3 *)

TYPE
  Statistics = OBJECT                                 (* used in VideoVBT.i3 *)
    framesStarted  : CARDINAL; 
    framesProcessed: CARDINAL; 
    timesBlocked   : CARDINAL; 
    cumLatency     : Tick.T;
  END;

END JVDecomp.
