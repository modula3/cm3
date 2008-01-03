(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 29 15:21:43 PST 1993 by mhb    *)
(*      modified on Sat Aug  1  0:26:57 PDT 1992 by meehan *)
(*      modified on Tue Jun 16 13:08:29 PDT 1992 by muller *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "QuickSwitchVBT" is a switch version of Trestle's
   "QuickBtnVBT". *)

INTERFACE QuickSwitchVBT;

IMPORT FeedbackVBT, QuickBtnVBT, VBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public = QuickBtnVBT.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (f: FeedbackVBT.T): T;
             <* LL = VBT.mu *>
             callback (READONLY cd: VBT.MouseRec);
           END;

END QuickSwitchVBT.




