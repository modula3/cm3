(* Copyright (C) 1992, Digital Equipment Corporation                 *)
(* All rights reserved.                                              *)
(* See the file COPYRIGHT for a full description.                    *)
(*                                                                   *)
(* Last modified on Mon Feb  1 22:39:23 PST 1993 by mhb              *)
(*      modified on Mon Aug 10  0:21:24 PDT 1992 by meehan           *)
(*      modified on Tue Jun 16 13:09:02 PDT 1992 by muller           *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "BorderedFeedbackVBT" is a multi-filter feedback that
   displays a border as visual feedback to another "VBT". *)

INTERFACE BorderedFeedbackVBT;

IMPORT FeedbackVBT, PaintOp, VBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public =
    FeedbackVBT.T OBJECT
    METHODS
      <* LL <= VBT.mu *>
      init (
        ch: VBT.T; 
        size: REAL := 0.5; 
        op: PaintOp.T := PaintOp.BgFg): T
    END;

(* The call "v.init(ch, size, op)" initializes "v" as a
   "BorderedFeedbackVBT".  The size of the border is "size"
   millimeters.  In the ``on'' state, the default "normal" method
   draws the border with paint op "op" using texture
   "Pixmap.Solid".  In the ``off'' state, the default "normal"
   method uses "Pixmap.Empty" instead.  The default "excited"
   method draws the border with text "Pixmap.Gray". *)

END BorderedFeedbackVBT.







