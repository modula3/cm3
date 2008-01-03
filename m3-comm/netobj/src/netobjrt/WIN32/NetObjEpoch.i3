(* Copyright (C) 1995, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Apr 13 13:46:27 PDT 1995 by kalsow  *)

INTERFACE NetObjEpoch;

CONST T = 11644473600.0d0;
(* "X - T" gives the network object time that corresponds to a
   "Time.T" of "X".  The base of network object time is Jan 1, 1970.
   The Win32 epoch is Jan 1, 1600. *)

END NetObjEpoch.
