(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The "BoardX" interface allows the server to create a
   useful subtype of "Board.T".
*)

INTERFACE BoardX;

IMPORT Board, SmallDB, OSError, Pickle;

TYPE T <: Public;
     Public = Board.T OBJECT
     METHODS
       init (stable: SmallDB.T; recover: BOOLEAN): T 
           RAISES {OSError.E, SmallDB.CorruptedDB, Pickle.Error};
     END;

(* The "stable" argument to the "init" method is where the board must
   log updates or save a snapshot. 
   If "recover" is true then the board must recover its
   set of items from "stable", otherwise it must initialize the
   "stable" to be an empty set of items.
*)

PROCEDURE Busy (board: T): BOOLEAN;
(* Returns false iff the board has no clients (more specifically, if the board
   knows of no live clients).
*)

PROCEDURE Save (board: T) RAISES {OSError.E};
(* Forces a snapshot, thus truncating the log. *)

PROCEDURE Quit (board: T);
(* Terminate all threads, and ready for GC.
   WARNING: not implemented. *)

END BoardX. 
