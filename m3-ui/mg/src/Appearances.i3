(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Fri Jul 17 19:16:11 PDT 1992 by harrison *)
(*      modified on Tue May  5 18:49:04 1992 by steveg   *)

INTERFACE Appearances;

<* PRAGMA LL *>

IMPORT MG, PaintOp, R2;

TYPE Appearance = MG.Appearance;

TYPE
  DropShadow <: DSPublic;
  DSPublic = Appearance OBJECT
               color: PaintOp.ColorScheme;
               delta: R2.T;
             END;
(* A DropShadow appearance paints an MG.T twice.  First it paints the
   object offsetting the object's position by "delta" and its color to
   "color".  Then it paints the object with its original position and
   color. *)

TYPE
  DropShadowHighlight <: DSHPublic;
  DSHPublic = DropShadow;
(* A drop shadow appearance where the drop shadow "delta" is scaled by the
   object's highlight value.  In an animation that changes the highlight
   value, the effect is to increase the shadow's width and highlight
   increases. *)

TYPE
  WeightUnderlay <: WUPublic;
  WUPublic = Appearance OBJECT
               weight: REAL;
               color : PaintOp.ColorScheme;
             END;
(* A WeightUnderlay appearance paints an MG.T twice.  First it paints the
   object using weight "weight" and color "color".  Then it paints the
   object with its original weight and color. *)

TYPE
  WeightUnderlayHighlight <: WUHPublic;
  WUHPublic = WeightUnderlay;
(* A WeightUnderlay appearance where the weight underlay "weight" is scaled
   by the object's highlight value.  In an animation that changes the
   highlight value, the effect is to widen the underlay as highlight
   increases. *)

TYPE
  Side = {Left, Right, Top, Bottom};
  Sides = SET OF Side;

  Clipped <: ClippedPublic;
  ClippedPublic = MG.AppearanceDefault OBJECT
                    sides: Sides := Sides{Side.Left.. Side.Bottom};
                  END;
(* A Clipped appearance clips an MG.T proportionally based on the object's
   highlight value.  When highlight = 0, the object is unclipped.  When
   highlight = 1, the object is completely clipped.

   The clipping rectangle shrinks in from the sides in "self.sides".  If
   two opposing sides are given, the clipping rectangle shrinks to meet in
   the middle.  Otherwise, the clipping rectangle shrinks toward the
   opposite side.

   In an animation that changes the highlight value, the effect is for the
   shrink as highlight increases. *)


TYPE
  ColorLineHighlight <: CLHPublic;
  CLHPublic = Appearance OBJECT
                highlightColor: PaintOp.ColorScheme;
                fromFrom                              := TRUE;
              END;
(* A ColorLineHighlight appearance paints an MG.Line in two colors based on
   the line's highlight value.  In an animation that changes the highlight
   value, the effect is to change the color of the line starting at one
   endpoint and moving to the other.  If fromFrom is TRUE, then the color
   change starts at the "from" endpoint of the line, otherwise it starts at
   the "to" endpoint.

   It is an error to apply a ColorLineHighlight to an object that isn't a
   subtype of a MG.Line *)

END Appearances.

