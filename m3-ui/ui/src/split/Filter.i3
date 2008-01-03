(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 13:53:11 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* A "Filter.T" is a "Split.T" with at most one child. *)

INTERFACE Filter;

IMPORT Split, VBT;

TYPE 
  T <: Public; 
  Public = Split.T OBJECT METHODS
     <* LL.sup <= VBT.mu *> 
     init(ch: VBT.T): T 
   END;

(* The call "v.init(ch)" initializes "v" as a filter with 
   child "ch" and returns "v". *)

(* "Split.Move" on a filter is a noop.  "Split.Insert" replaces the child, 
   if any, and detaches it. *)

PROCEDURE Child(v: T): VBT.T;
<* LL.sup = VBT.mu *>
(* Return the child of "v", or "NIL" if there is no child. *)

(* "Filter.Child(v)" is equivalent to "Split.Succ(v, NIL)". *)

PROCEDURE Replace(v: T; ch: VBT.T): VBT.T;
<* LL.sup = VBT.mu *>
(* Replace "v"'s child by "ch", detach and return "v"'s old child, and
   mark "v" for redisplay.  *)
   
(* "Filter.Replace" is similar to "Split.Replace", except that it
   returns the old child instead of taking the old child as an argument,
   and if "ch" is "NIL" it is similar to "Split.Delete".  *)

END Filter.
