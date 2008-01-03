(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "Log" interface manages the log of updates made to the board. *)


INTERFACE Log;

IMPORT SmallDB, OSError, 
       NotifyRec, BoardX;

PROCEDURE Update (stable: SmallDB.T; bd: BoardX.T; nr: NotifyRec.T) 
    RAISES {OSError.E};
 
(* Records the update indicated by "nr" in the log associated with "stable".
   If the log grows too big, then takes a snapshot by calling 
   "BoardX.Save (bd)".
*)

TYPE UpdateClosure <: SmallDB.Closure;

END Log.
