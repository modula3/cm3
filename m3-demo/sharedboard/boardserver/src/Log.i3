(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "Log" interface manages the log of updates made to the board. *)


INTERFACE Log;

IMPORT SmallDB, OSError, Pickle,
       AtomicItemTbl, NotifyRec, BoardX;

PROCEDURE Update (stable: SmallDB.T; bd: BoardX.T; nr: NotifyRec.T) 
    RAISES {OSError.E};
 
(* Records the update indicated by "nr" in the log associated with "stable".
   If the log grows too big, then takes a snapshot by calling 
   "BoardX.Save (bd)".
*)

PROCEDURE Recover (stable: SmallDB.T; state: AtomicItemTbl.State)
    RAISES {OSError.E, SmallDB.CorruptedDB, Pickle.Error};
(* Updates "state" by recovering updates logged in "stable". *)

END Log.
