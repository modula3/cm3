(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Mar  1 15:23:51 PST 1993 by meehan *)
(*      modified on Mon Feb  1 00:21:37 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:07:56 PDT 1992 by muller *)

(* The "ZSplitUtils" interface contains utility procedures for working
   with "ZSplit"s.  The "ZChildVBT" interface contains some additional
   utility procedures that are oriented for children of "ZSplit"s that
   are used as subwindows. *)

INTERFACE ZSplitUtils;

IMPORT VBT;

PROCEDURE FindZChild (v: VBT.T): VBT.T;

(* Return the lowest (possibly improper) ancestor of "v" whose parent
   is a "ZSplit.T" and which is not the "ZSplit.T"'s background child.
   If no such "VBT" is found, return "NIL".  There's a good chance
   that the "VBT" returned is a "ZChildVBT.T", but this is not
   required. *)

END ZSplitUtils.

