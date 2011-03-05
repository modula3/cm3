(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Mar 21 17:47:28 1996 by gnelson                      *)
(*      modified on Tue Sep  5 15:29:33 PDT 1995 by detlefs                  *)

INTERFACE AFTabUndoRec;

IMPORT AF;

CONST Brand = "AFTabUndoRec";

TYPE
  T = RECORD af: AF.T; tv: AF.TruthVal END (* RECORD *);

END AFTabUndoRec.
