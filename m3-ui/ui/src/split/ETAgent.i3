(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* File: ETAgent.i3, by cgn, Tue Apr 21 22:00:25 1987 *)
(* Last modified on Mon Sep 28 22:55:35 PDT 1992 by msm     *)
(*      modified on Mon Feb 24 13:53:03 PST 1992 by muller  *)
(*      modified on Sun Nov 10 17:34:18 PST 1991 by gnelson *)
<*PRAGMA LL*>

(* A "ETAgent.T" is a filter which implements the redirection of "read",
   "write", and "key" methods.  *)

INTERFACE ETAgent;

IMPORT VBT, Filter;

TYPE
  T <: Filter.T;

(* The call "v.init(ch) initializes "v" as a "ETAgent" with child "ch",
   and marks "v" for redisplay.  *)

PROCEDURE New(ch: VBT.T): T; <* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

PROCEDURE ReleaseSelections(v: T); <* LL.sup = VBT.mu *>
(* Notify v's children that all selections have been lost. *)

END ETAgent.
