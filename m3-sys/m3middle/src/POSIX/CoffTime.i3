(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Wed Nov 30 14:31:06 PST 1994 by kalsow   *)

(* An OS-independent way to get COFF timestamps *)

INTERFACE CoffTime;

CONST EpochAdjust = 0.0d0; (* to 1/1/1970 *)

PROCEDURE Now (): INTEGER;

PROCEDURE OfFile (file: TEXT): INTEGER;

END CoffTime.
