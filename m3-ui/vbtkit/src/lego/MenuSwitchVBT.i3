(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 29 15:21:43 PST 1993 by mhb        *)
(*      modified on Sat Aug  1  0:23:29 PDT 1992 by meehan     *)
(*      modified on Tue Jun 16 13:08:38 PDT 1992 by muller     *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "MenuSwitchVBT" is a switch version of Trestle's "MenuBtnVBT".  *)

INTERFACE MenuSwitchVBT;

IMPORT FeedbackVBT, MenuBtnVBT, VBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public = MenuBtnVBT.T OBJECT
           METHODS
             <* LL.sup <= VBT.mu *>
             init (f: FeedbackVBT.T): T;
             <* LL.sup = VBT.mu *>
             callback (READONLY cd: VBT.MouseRec);
           END;

END MenuSwitchVBT.




