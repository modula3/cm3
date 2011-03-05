(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 10 21:15:41 PDT 1999 by saxe                     *)
(*      modified on Wed Sep 20 17:05:17 PDT 1995 by detlefs                  *)

INTERFACE RefListMisc;

(* Generally useful procedures on "RefList.T"'s. *)

IMPORT RefList;

TYPE
  EquivProc = PROCEDURE(READONLY e1, e2: REFANY): BOOLEAN;

PROCEDURE SetEquiv(l1, l2: RefList.T; equiv: EquivProc): BOOLEAN;

PROCEDURE SetSubset(l1, l2: RefList.T; equiv: EquivProc): BOOLEAN;

PROCEDURE ListEquiv(l1, l2: RefList.T; equiv: EquivProc): BOOLEAN;

END RefListMisc.
