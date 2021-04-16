(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Wed Nov 30 14:31:06 PST 1994 by kalsow   *)

(* An OS-independent way to get COFF timestamps *)

INTERFACE CoffTime;

(* CONST *) <*EXTERNAL CoffTime__EpochAdjust*> VAR EpochAdjust: LONGREAL;

<*EXTERNAL CoffTime__Now*>PROCEDURE Now (): INTEGER;

(* This code is not used.
<*EXTERNAL*> CoffTime__OfFileC*>PROCEDURE OfFileC (file: Ctypes.const_char_star): LONGINT;
PROCEDURE OfFile (file: TEXT): LONGINT;
 *)

END CoffTime.
