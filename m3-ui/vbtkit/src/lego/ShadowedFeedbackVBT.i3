(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Jun 10 14:03:49 PDT 1993 by meehan *)
(*      modified on Thu Feb  4 15:02:29 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:16 PDT 1992 by muller *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "ShadowedFeedbackVBT" is a multi-filter feedback that displays a
   3-D border as visual feedback to another VBT. *)

INTERFACE ShadowedFeedbackVBT;

IMPORT FeedbackVBT, Shadow, VBT;

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public =
    FeedbackVBT.T OBJECT
    METHODS
      <* LL <= VBT.mu *>
      init (ch            : VBT.T;
            shadow        : Shadow.T := NIL;
            onStyle                  := Shadow.Style.Lowered;
            onExcitedStyle           := Shadow.Style.Raised;
            offStyle                 := Shadow.Style.Raised;
            offExcitedStyle          := Shadow.Style.Lowered): T
    END;

(* The call "v.init(ch, shadow, ...)" initializes "v" as a
   "ShadowedFeedbackVBT".  The internal structure of "v" includes a
   "ShadowedVBT" for displaying the shadow "shadow" around "ch".  The
   default "normal" and "excited" methods change the style of the
   shadow, taking into account the state of "v".  For example, when
   "FeedbackVBT.GetState(v)" returns "FALSE", the "excited" method
   uses the value of "offExcitedStyle".

   On a monochrome screen (whenever "Shadow.IsSupport(v, shadow)" is
   false), "ch" is inverted by the default "normal" method when the
   state is ``on'' and by the "excited" method when the state is
   ``off.''

   The default parameters to the "init" method generate a feedback
   that is appropriate for buttons.  The following procedure generates
   a feedback that is appropriate for use by menu buttons: *)

PROCEDURE NewMenu (ch: VBT.T; shadow: Shadow.T := NIL): T;
<* LL <= VBT.mu *>
(* Return a "ShadowedFeedbackVBT" appropriate for menu buttons.  The
   "normal" method always uses "Shadow.Style.Flat"; the "excited"
   method always uses "Shadow.Style.Lowered". *)

END ShadowedFeedbackVBT.

