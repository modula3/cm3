(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Aug 24 17:21:04 PDT 1994 by mhb    *)
(*      modified on Fri Jun 11 11:08:34 PDT 1993 by meehan *)
(*      modified on Tue Jun 16 21:18:24 PDT 1992 by muller *)
<* PRAGMA LL *>
<* PRAGMA SUBTYPE *>

(* A "SourceVBT" is used to implement a ``drag-and-drop'' paradigm.
   The object being dragged is the {\em source} and an object into
   which the source may be dropped is the {\em target}.

   As a subclass of "ButtonVBT", a "SourceVBT" has "pre", "post", and
   "cancel" methods.  In addition, it has "during", "callback", and
   "hit" methods.  The methods are called as follows: The "pre" method
   is invoked on the first click in the VBT; the "post" method is
   called on an uncanceled upclick; the "cancel" method is called
   whenever the mouse is chord-canceled; the "during" method is called
   whenever the mouse has moved (and remained on the same screen)
   since the last call to "during" or "pre".  A new VBT cage
   containing the current cursor position will be set before calls to
   "pre" and "during". The "callback" method is called after
   the "post" method, as long as the mouse was over an ``acceptable
   target'' when the upcplick happened.

   The heart of drag-and-drop is implemented by the default "during"
   method: Recall that the "during" method is invoked each time the
   mouse moves while the button is down and not chord-cancelled.  The
   default "during" method looks to see if the mouse is over a VBT
   marked as a {\em target}.  If so, then the "SourceVBT"'s "hit"
   method is invoked to see if the target is acceptable for the
   source.  If so, an "excited" method on the target is invoked to
   give feedback, and eventually, a target's "normal" method is
   called to remove the feedback.  If the target is not acceptable,
   nothing happens.  *)


INTERFACE SourceVBT;

IMPORT ButtonVBT, FeedbackVBT, HighlightVBT, PaintOp, VBT;

(* \subsubsection{Sources} *)

TYPE
  <* SUBTYPE T <: MultiFilter.T *>
  T <: Public;
  Public =
    ButtonVBT.T OBJECT
    METHODS
      <* LL <= VBT.mu *>
      init (f: FeedbackVBT.T): T;
      <* LL = VBT.mu *>
      during   (READONLY cd: VBT.PositionRec);
      callback (READONLY cd: VBT.MouseRec);
      hit (target: VBT.T; READONLY cd: VBT.PositionRec):
           BOOLEAN;
    END;

(* The call "v.init(...)" initializes "v" as a "SourceVBT".  The
   default "pre" method changes the cursor to a starburst and calls
   "SwitchVBT.Pre".  The default "during" method calls the "hit"
   method whenever it is on a location controlled by a VBT that is a
   target.  If the "hit" method returns "TRUE", the target's "excited"
   method is called.  As the mouse moves from target to target, the
   previous trarget's "normal" method is called before another
   target's "excited" method is invoked.  The "post" and "cancel"
   methods invoke the current target's "normal" method, restore the
   original cursor, and call "SwitchVBT.Post" and "SwitchVBT.Cancel"
   respectively.  It's guaranteed that a target's "excited" and
   "normal" methods are called in non-nested pairs.

   The default "hit" method always returns "TRUE".  The default
   "during" and "callback" methods are no-ops. *)

PROCEDURE GetTarget (v: T): Target;
<* LL.sup = VBT.mu *>
(* If the mouse is not over a valid target, or if the most recent call
   to "v.hit(target, cd)" returned "FALSE", then return "NIL";
   otherwise return "target". This procedure is intended to be called
   by a "callback" method to find out the current target. *)

(* \subsubsection{Targets} *)

TYPE Target = VBT.T;
(* A target is a VBT on which "BeTarget" has been invoked. *)

TYPE
  TargetClass <: TargetClassPublic;
  TargetClassPublic =
    ROOT OBJECT
      vbt: VBT.T;  (* READONLY; set by BeTarget *)
      source: T;   (* READONLY; for use by normal/excited *)
    METHODS
      <* LL = VBT.mu *>
      normal  ();
      excited ();
    END;

(* A "TargetClass" determines the feedback when a target's "excited"
   method is called. The "source" field can be read by the "normal"
   and "excited" methods, but clients may find "GetSource" more
   convenient to use.

   The default "normal" and "excited" methods are no-ops. *)
   
PROCEDURE BeTarget (w: VBT.T; class: TargetClass);
<* LL.sup < w *>
(* Make "w" into a target for a "SourceVBT".  As a target, "w" may
   be passed to some "SourceVBT"'s "hit" method. *)

PROCEDURE TargetClassOf (w: Target): TargetClass;
<* LL.sup < w *>
(* Return the "class" argument for which there was a previous call
   to "BeTarget(w, class)", or "NIL" if there was no such call. *)
   
PROCEDURE GetSource (w: Target): T;
<* LL.sup = VBT.mu *>
(* Called by a target's "normal" or "excited" methods to find out
   the "SourceVBT" causing the method to be invoked. *)

PROCEDURE GetHighlighter (v: T): HighlightVBT.T;
<* LL.sup = VBT.mu *>
(* Returns the "HighlightVBT" above the nearest Trestle-installed
   ancestor of "v". This is typically called by a "normal" or
   "excited" method. *)


(* Here are three "TargetClass" objects that may be useful. Each of
these use the "op" parameter for painting in the "HighlighVBT". *)

PROCEDURE NewInserterTarget (op := PaintOp.TransparentSwap): TargetClass;
<* LL = arbitrary *>
(* Displays a grid over itself when excited.  Appropriate for an
   adjusting bar in a tiling window manager.  The parent of the target
   must be an "HVSplit", and grid has a minimum size in the
   "HVSplit"'s axis. *)

PROCEDURE NewSwapTarget (op := PaintOp.TransparentSwap): TargetClass;
<* LL = arbitrary *>
(* Displays a grid over itself when excited.  This target is
   appropriate for a non-adjusting bar in a tiling window manager. *)

PROCEDURE NewTarget (op := PaintOp.TransparentSwap): TargetClass;
<* LL = arbitrary *>
(* Inverts itself when excited.  This target class is a
   general-purpose target. *)

END SourceVBT.








