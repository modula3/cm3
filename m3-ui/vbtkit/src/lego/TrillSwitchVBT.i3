(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 29 15:21:41 PST 1993 by mhb    *)
(*      modified on Sat Aug  1  0:29:25 PDT 1992 by meehan *)
(*      modified on Tue Jun 16 13:08:09 PDT 1992 by muller *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "TrillSwitchVBT.T" is a switch version of Trestle's
   "TrillBtnVBT".

   Actually, a "TrillBtnVBT" does not exist.  If it existed, it
   would be a button that generates events repeatedly while the
   mouse is down and in its domain.  When the mouse leaves the
   domain, events generation is suspended until the mouse
   returns.

   The implementation uses the "AutoRepeat" interface for
   repeatedly generating events.  That interface defines the
   parameters that control how frequently events are generated,
   and how long to wait before starting to auto-repeat. *)

INTERFACE TrillSwitchVBT;

IMPORT ButtonVBT, FeedbackVBT, VBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public = ButtonVBT.T OBJECT
           METHODS
             <* LL.sup <= VBT.mu *>
             init (f: FeedbackVBT.T): T;
             <* LL.sup = VBT.mu *>
             callback (READONLY cd: VBT.MouseRec);
           END;

END TrillSwitchVBT.




