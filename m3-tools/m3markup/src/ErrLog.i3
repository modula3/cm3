(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu May  6 10:37:59 PDT 1993 by kalsow                   *)

INTERFACE ErrLog;

PROCEDURE Note (msg: TEXT);

PROCEDURE Msg (a, b, c, d: TEXT := NIL);

END ErrLog.
