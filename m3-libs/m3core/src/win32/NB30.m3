(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Tue Nov  8 10:46:01 PST 1994 by kalsow   *)
(*      modified on Wed Feb 10 21:31:07 PST 1993 by harrison *)

UNSAFE MODULE NB30;

IMPORT M3toC;

BEGIN
  ALL_TRANSPORTS := M3toC.TtoS("M\000\000\000");
  MS_NBF := M3toC.TtoS("MNBF");
END NB30.
