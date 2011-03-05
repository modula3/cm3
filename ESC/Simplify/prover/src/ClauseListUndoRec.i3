(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Mar 22 13:48:38 PST 1996 by detlefs                  *)

INTERFACE ClauseListUndoRec;

IMPORT Clause;

CONST Brand = "ClauseListUndoRec";

TYPE 
  Tag = { Mark, Add, Delete };

  T = RECORD tag: Tag; sentinel, c: Clause.T END (* RECORD *);   

END ClauseListUndoRec.
