(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Mar 27 19:31:39 PST 1996 by heydon                   *)

INTERFACE PSImpl;

(* Defines a type representing a Juno PostScript state, and implementations
   for the built-in PostScript procedures. *)

IMPORT Drawing, PSFont, View, JunoPt, JunoRect;
IMPORT JunoScope, JunoValue;
IMPORT VBT, PaintOp, Path, Font;
IMPORT Wr;

TYPE
  T = View.PSImpl;

REVEAL
  View.PSImpl <: Public;

TYPE
  Public = View.T OBJECT
    (* PS state *)
    ps: State;
    sp := 0;
    psStack: REF ARRAY OF State
  METHODS
    init(ch: Drawing.ChildPublic; root: View.Root): T
  END;

(* The "ps" field is the current PostScript state. PostScript states are
   pushed and popped from the stack "(sp, psStack)" on calls to the Juno
   procedures "PS.Save" and "PS.Restore". *)

(* "NEW(T).init(ch, r)" creates a new "T" on the root "r" with
   child "ch". *)

TYPE
  Color = RECORD
    r, g, b: JunoValue.Real
  END;
  State = RECORD
    color: Color;
    width: JunoValue.Real;
    end: VBT.EndStyle;
    join: VBT.JoinStyle;
    wind: VBT.WindingCondition;
    path: Path.T;
    moveto: BOOLEAN;
    movetoPt: JunoPt.T;
    currPt, subpathStartPt: JunoPt.T;
    face: TEXT;
    size: CARDINAL;
    ptSize: JunoValue.Real;
    bbox: JunoRect.T;
    (* Trestle painting only *)
    colorOp, textColorOp: PaintOp.T;
    xFont: Font.T;
    (* PS implementation only *)
    psMetric: PSFont.Metric;
  END;

(* A "State" record embodies a complete Juno PostScript state. The state is
   used when both painting to Trestle VBT's and when writing PostScript
   output. Except as noted, all pieces of the state must be maintained in both
   cases (the Trestle state must be maintained even when writing PostScript
   due to the procedures in the PS module for reading parts of the
   state). Also, except for the "path" field, all coordinates in a "State"
   record are in Juno coordinates.

   "color" contains the red, green, and blue coordinates of the current color,
   "width" is the pen width for strokes, "end" is the end style for strokes,
   "join" is the join style for strokes, "wind" is the winding condition for
   fills, and "path" is the current path.

   When painting to the drawing view, the current path is determined by
   "path", "moveto", and "movetoPt". If "moveto" is "FALSE", then the current
   path is simply "path". If "moveto" is "TRUE", then the current path is
   actually "path" concatenated with "PS.MoveTo(movetoPt)". The extra state
   allows us to mimic the PostScript semantics: if a sequence of consecutive
   MoveTo's are performed, only the last one takes effect. The "movetoPt" is
   incorporated into the path when we know it cannot be followed by another
   call to "PS.MoveTo", namely, when processing a call to "PS.LineTo",
   "PS.CurveTo", or "PS.Close".

   The fields "currPt" and "subpathStartPt" are defined iff the current
   (logical) path is non-empty. The field "currPt" stores the current
   point. It is the (last) argument to the most recent call to "PS.MoveTo",
   "PS.LineTo", or "PS.CurveTo". We store this value instead of reading it
   from the current path because points on the path are integer valued, so
   this will guarantee for all points "a" that the code:

|    PS.MoveTo(a);
|    b := PS.CurrentPoint()

   has the effect of setting "b" to "a". The field "subpathStartPt" stores the
   location of the most recent "MoveTo" on the curernt path; its value is
   valid iff the current path is non-empty and the current sub-path is
   open. This point is saved to implement the "PS.Close" operation.

   "face" and "size" are the current font face and font size. "ptSize" is the
   size of the current font in points. "bbox" is the current bounding box.

   When painting in the drawing view, "colorOp" is the current color for
   strokes and fills, "textColorOp" is the PaintOp pair that paints the
   foreground in "colorOp" and the background transparent, and "xFont" is the
   current X font.

   When painting to a PostScript file, "psMetric" is the
   metric of the current PostScript font. *)

TYPE
  Impl <: ImplPublic;
  ImplPublic = JunoScope.Mod BRANDED "PSImpl.ImplPublic" OBJECT METHODS
    startToFile(wr: Wr.T);
    prologue() RAISES {Wr.Failure};
    epilogue(showPage := FALSE) RAISES {Wr.Failure};
    endToFile()
  END;

(* A "PS.Impl" is a "JunoScope.Mod" that implements the "PS" module. By
   default, its procedures direct their output to a particular drawing view.
   Clients can bracket the execution of a particular bytestream by calls to
   the "startToFile"/"prologue" and "epilogue"/"endToFile" methods to instead
   cause PostScript code to be written to a specified writer.

   The call "impl.startToFile(wr)" arranges that all the external procedures
   installed on behalf of "impl" send PostScript output to "wr" instead of
   updating the drawing view. Clients must call the "endToFile" method to
   restore the original external procedures. It is a checked run-time error
   to call any of the following three methods without an initial call to
   "startToFile".

   The call "impl.prologue()" writes the PostScript prologue to the writer
   associated with the most recent call to "startToFile". The call
   "impl.endToFile(showPage)"  writes the PostScript epilogue (such as the
   bounding box and number of pages) to the writer associated with the most
   recent call to "startToFile". If "showPage" is true, a PostScript
   "showpage" command is written to the writer before the epilogue.

   The call "impl.endToFile()" re-installs the original external
   procedures to direct drawing to the drawing view associated with "impl".
   This method does *not* close the underlying
   writer. *)

PROCEDURE Reset(d: T; inExec := TRUE);
(* Reset the PostScript state associated with the drawing "d". The "inExec"
   argument is used for logging purposes only, to distinguish those calls to
   "Reset" within the scope of a "JunoRT.Exec" execution from those that are
   not. *)

PROCEDURE New(rt: View.Root): Impl;
(* Return an implementation of the PS interface, whose procedures operate on
   the drawing view "rt.currView". *)

END PSImpl.
