(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Fri Jun 11 09:21:13 PDT 1993 by meehan *)
(*      modified on Mon Feb  1 14:17:32 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:09:04 PDT 1992 by muller *)
(*      modified on Fri Mar 27 01:48:06 1992 by steveg *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "BiFeedbackVBT" is a multi-filter feedback that is used for
   composing two arbitrary feedbacks.  The default "normal" and
   "excited" methods of a "BiFeedbackVBT" invoke the corresponding
   methods on the two feedbacks.  The "BiFeedbackVBT" itself doesn't
   have any visual appearance. *)

INTERFACE BiFeedbackVBT;

IMPORT FeedbackVBT, VBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public = FeedbackVBT.T OBJECT
           METHODS
             <* LL <= VBT.mu *>
             init (f1, f2: FeedbackVBT.T; ch: VBT.T): T;
           END;

(* The call "v.init(f1, f2, ch)" initializes "v" as a "BiFeedbackVBT".
   The multi-child of "v" is "ch".  The internal structure of "v" is
   as follows: The VBT-child of "v" is "f1", the multi-child of "f1"
   is "f2", and the multi-child of "f2" is "ch".  (Recall that it is
   legal and meaningful for a VBT to have multiple multi-parents, as
   "ch" will have.)  When the "init" method is called, both "f1" and
   "f2" must be childless. *)

END BiFeedbackVBT.

