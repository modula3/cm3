(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 10 13:12:40 PDT 1993 by meehan *)
(*      modified on Tue Feb  2 00:13:22 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:15 PDT 1992 by muller *)
<* PRAGMA LL *>

(* A "ShadowedVBT.T" is a filter whose parent's screen consists of the
   child's screen surrounded by a 3-D border.  The style, size, and
   colors of the shadow can be set dynamically.  The parent's shape is
   determined from the child's shape by adding the size of the shadow.
   *)

INTERFACE ShadowedVBT;

IMPORT Filter, Shadow, VBT;

TYPE
  T <: Public;
  Private <: Filter.T;
  Public = Private OBJECT
    METHODS
      <* LL.sup <= VBT.mu *>
      init (ch: VBT.T;
            shadow: Shadow.T := NIL;
            style: Shadow.Style := Shadow.Style.Flat): T;
    END;

(* The call "v.init(...)" initializes "v" as a "ShadowedVBT" with
   child "ch" and the given "shadow" and "style".  When
   "Shadow.Support(shadow, v)" is "TRUE", each dimension of "v"
   exceeds the corresponding dimension of "ch" by "2 *
   ABS(shadow.size)"; otherwise, each dimension of "v" exceeds
   the corresponding dimension of "ch" by "2 *
   ABS(shadow.size/2)".  If "shadow=NIL", it defaults to
   "Shadow.None". *)

PROCEDURE Set (v: T; shadow: Shadow.T);
<* LL.sup = VBT.mu *>
(* Change the size and colors of "v"'s shadow and mark "v" for
   redisplay. *)

PROCEDURE SetStyle (v: T; style: Shadow.Style);
<* LL.sup = VBT.mu *>
(* Change the style of "v"'s shadow, and mark "v" for redisplay. *)

PROCEDURE Get (v: T): Shadow.T;
<* LL.sup = VBT.mu *>
(* Return "v"'s shadow. *)

PROCEDURE GetStyle (v: T): Shadow.Style;
<* LL.sup = VBT.mu *>
(* Return "v"'s shadow style.  *)

END ShadowedVBT.









