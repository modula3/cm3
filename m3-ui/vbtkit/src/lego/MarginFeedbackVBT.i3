(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Wed Jun 16 10:18:47 PDT 1993 by meehan *)
(*      modified on Tue Feb  2 00:19:36 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:41 PDT 1992 by muller *)
(* modified on Fri Mar 27 01:58:46 1992 by steveg *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "MarginFeedbackVBT" is a multi-filter feedback that provides
   visual feedback to the left of another VBT.  This interface
   defines a handful of useful ``left-hand sides.'' *)

INTERFACE MarginFeedbackVBT;

IMPORT FeedbackVBT, Shadow, VBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public = FeedbackVBT.T OBJECT
           METHODS
             <* LL.sup <= VBT.mu *>
             init (ch, marginVBT: VBT.T): T
           END;

(* The following procedures create some popular types of
   "MarginFeedbackVBT"s. See Figure~\ref{fig:marginfeedbacks}. *)

PROCEDURE NewCheck  (ch: VBT.T; shadow: Shadow.T := NIL): T;
<* LL.sup <= VBT.mu *>

PROCEDURE NewBox (ch: VBT.T; shadow: Shadow.T := NIL): T;
<* LL.sup <= VBT.mu *>

PROCEDURE NewBullet (ch: VBT.T; shadow: Shadow.T := NIL): T;
<* LL.sup <= VBT.mu *>

END MarginFeedbackVBT.

