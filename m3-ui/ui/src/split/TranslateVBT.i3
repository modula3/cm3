(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 13:55:12 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* A "TranslateVBT.T" is a filter that maintains a translation between 
   the coordinate systems of the child and parent such that the child's 
   coordinate system has its origin at the northwest corner of the 
   child domain. The child can be "NIL", in which case the "TranslateVBT" 
   ignores all events. *)

INTERFACE TranslateVBT;

IMPORT VBT, Filter;

TYPE T <: Filter.T;

(* The call "v.init(ch)" initializes "v" as a "TranslateVBT" with 
   child "ch". *)

PROCEDURE New(ch: VBT.T): T; <* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(T).init(...)". *)

END TranslateVBT.

