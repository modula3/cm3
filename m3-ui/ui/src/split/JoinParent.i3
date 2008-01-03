(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Dec 14 03:14:17 PST 1992 by msm    *)
(*      modified on Wed Aug 12 23:29:10 PDT 1992 by meehan *)
(*      modified on Wed Aug 12 17:12:45 PDT 1992 by guarino *)
(*      modified on Tue Jun 16 13:08:47 PDT 1992 by muller *)
(*      modified on Mon Jun 15 21:49:39 1992 by mhb    *)
(*      modified on Fri Mar 27 01:58:11 1992 by steveg *)
(* modified on Fri Feb 2 14:03:04 PST 1990 by glassman *)
(* modified on Sun Jun 5 23:37:29 PDT 1988 by gnelson *)
<*PRAGMA LL*>

INTERFACE JoinParent;

IMPORT ETAgent, JoinedVBT, VBT, Rect, ScrnCursor;

TYPE Join = JoinedVBT.Public OBJECT parents: T := NIL END;

TYPE
  T <: Public;
  Public =
    ETAgent.T OBJECT             <* LL >= {JoinedVBT.T child of T, VBT.mu} *>
      link: T
    METHODS
      <* LL.sup <= VBT.mu *>
      init (join: JoinedVBT.T): T;
    END;

(* The call "v.init(join, north, west)" initializes "v" as a
   "JoinParent.T" with child "join", and returns "v".

   The northwest corner of the parent's domain is positioned at
   point "(north, west)" in "Joined"'s domain; this will be
   maintained as the parent is reshaped or rescreened. 
   
*)

PROCEDURE New (v: JoinedVBT.T): T;
<* LL.sup <= VBT.mu *>
(* "New(...)" is equivalent to "NEW(ParentT).init(...)". *)

PROCEDURE Rem (prnt: T);
<* LL = VBT.mu *>
(* Cause the child of prnt to lose parent "prnt", and mark it for
   redisplay. *)

PROCEDURE Child (prnt: T): JoinedVBT.T;
<* LL = VBT.mu *>
(* Return the "JoinedVBT" for which "prnt" is a parent. *)

PROCEDURE Succ (v: JoinedVBT.T; prnt: T): T;
<* LL.sup = VBT.mu OR LL.sup = v *>
(* Return the successor to parent "prnt" of "v".  It is a checked runtime
   error if "prnt" is not a parent of "v".  As with "Split.Succ", the
   successor of "NIL" is the first parent; the successor the last parent is
   "NIL"; and the successor of "NIL" is "NIL" if "v" has no parents. *)

PROCEDURE Current(v: JoinedVBT.T): T;
<* LL.sup = v OR LL.sup = VBT.mu *>

PROCEDURE SetInput(v: JoinedVBT.T; prnt: T);
<* LL.sup = VBT.mu *>

PROCEDURE NeedsRescreen(v: JoinedVBT.T): BOOLEAN;
<* LL.sup = VBT.mu *>

PROCEDURE ST(v: JoinedVBT.T): VBT.ScreenType;
<* LL.sup = VBT.mu *>

PROCEDURE Domain(v: JoinedVBT.T): Rect.T;
<* LL.sup = VBT.mu *>

PROCEDURE SetCursor(v: T; cs: ScrnCursor.T);
<* LL.sup = JoinedVBT child of v *>

END JoinParent.

