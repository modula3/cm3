(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jun 12 21:56:23 PDT 1996 by detlefs                  *)
(*      modified on Thu Mar 21 17:57:53 1996 by gnelson                      *)

(* Manages lists of clauses that can be added to or deleted from
   undoably. *)

INTERFACE ClauseList;

IMPORT Clause;
IMPORT Wr;

PROCEDURE Add(sentinel, c: Clause.T; writeUndo := TRUE);
(* Add "c" to the head of clause list whose sentinel is "sentinel".
   This addition is undoable if "writeUndo" is "TRUE". *)

PROCEDURE Delete(sentinel, c: Clause.T; writeUndo := TRUE);
(* Delete "c" from whatever sentineled list it is in, undoably if
   "writeUndo" is "TRUE". *)

PROCEDURE Init();
PROCEDURE Push();
PROCEDURE Pop();

PROCEDURE DBGPrint(wr: Wr.T; sentinel: Clause.T);

END ClauseList.
