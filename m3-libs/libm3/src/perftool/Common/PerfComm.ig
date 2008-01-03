(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Aug 26 13:03:01 PDT 1993 by kalsow                   *)
(*      modified on Tue Feb 11 16:43:36 PST 1992 by muller                   *)

GENERIC INTERFACE PerfComm (Event);

IMPORT Rd, Wr;

PROCEDURE Send (wr: Wr.T;  READONLY e: Event.T);
PROCEDURE Receive (rd: Rd.T): Event.T;

END PerfComm.
