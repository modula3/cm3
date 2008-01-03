(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Apr  7 18:15:30 1992 by steveg  *)
<*PRAGMA LL*>

(* A "ETAgent1.T" is a filter which implements the redirection of "read",
   "write", and "key" methods.  *)

INTERFACE ETAgent1;

IMPORT ETAgent;

PROCEDURE ReleaseSelections(v: ETAgent.T); <* LL = VBT.mu *>
(* Release any selections held by v. *)

END ETAgent1.
