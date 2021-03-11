(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Wed Nov 30 14:30:23 PST 1994 by kalsow   *)

(* An OS-independent way to get COFF timestamps *)

INTERFACE CoffTime;

CONST EpochAdjust = 11644473600.0d0; (* seconds from 1/1/1600 -> 1/1/1970 *)

(* TODO Change INTEGER to LONGINT *)
PROCEDURE Now (): INTEGER;

(* TODO Change INTEGER to LONGINT
 * This is not used.
PROCEDURE OfFile (file: TEXT): INTEGER;
 *)

END CoffTime.
