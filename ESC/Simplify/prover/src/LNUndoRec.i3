(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Sep  9 00:34:55 PDT 1999 by saxe                     *)
(*      modified on Thu Sep 14 22:41:39 PDT 1995 by detlefs                  *)

(* This interface defines the types used to represent label names (see msg
   on src.sparta, detlefs 9/14/95), and the global set of asserted
   label names.  In addition, it represents an undoable global set "S" of
   asserted label names.
*)

INTERFACE LNUndoRec;

IMPORT LabelName;

CONST Brand = "LNUndoRec";

TYPE
  UndoType = { MkAtom, Insert, Dec, Mark };
  T = RECORD
        type: UndoType;
        ln: LabelName.T;
        anyAtSignLabels: BOOLEAN END (* RECORD *);

END LNUndoRec.
