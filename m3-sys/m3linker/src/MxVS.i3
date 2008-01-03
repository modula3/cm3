(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxVS.i3                                               *)
(* Last Modified On Mon Aug  1 11:36:08 PDT 1994 By kalsow     *)

INTERFACE MxVS;

IMPORT M3ID, M3FP;

TYPE
  T = INTEGER;
  (* a version stamp handle *)

TYPE
  Info = RECORD
    source : M3ID.T; (* exporting interface's name *)
    symbol : M3ID.T; (* symbol's name *)
    stamp  : M3FP.T; (* symbol's fingerprint *)
  END;

CONST
  NoVS = 0; (* a value never returned by "Put" *)

PROCEDURE Get (t: T;  VAR(*OUT*) i: Info);

PROCEDURE Put (READONLY i: Info): T;

END MxVS.
