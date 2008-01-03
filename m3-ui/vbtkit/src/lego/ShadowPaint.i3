(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 16 13:08:19 PDT 1992 by muller     *)
(*      modified on Tue Jul 30 12:50:46 PDT 1991 by mhb        *)

INTERFACE ShadowPaint;

(* Utilities for painting shadows. For all procedures, LL=VBT.mu; they are
   typically called from within a VBT's repaint method. *)

IMPORT Axis, PaintOp, Pixmap, Rect, Region, Shadow, VBT;

PROCEDURE Bar (         v     : VBT.T;
               READONLY clip  : Region.T;
                        shadow: Shadow.T;
                        style : Shadow.Style;
                        axis  : Axis.T;
               READONLY target: Rect.T        );
(* Paint a "bar" shadow in VBT v in the specified style, clipped against
   the specified region. The shadow fills target rectangle in the
   horizontal or vertical orientation. Styles Chiseled and Lowered are
   synonomous, as are Ridged and Raised. *)

PROCEDURE Border (         v      : VBT.T;
                  READONLY clip   : Region.T;
                           shadow : Shadow.T;
                           style  : Shadow.Style;
                  READONLY in, out: Rect.T        );
(* Paint a "border" shadow in VBT v in the specified style, clipped against
   the specified region. The shadow is a rectangular tube, with the
   specified inner and outer borders. *)

PROCEDURE Diamond (         v           : VBT.T;
                   READONLY clip        : Region.T;
                            shadow      : Shadow.T;
                            style       : Shadow.Style;
                   READONLY in, out     : Rect.T;
                            insideOp    : PaintOp.T;
                            insidePixmap: Pixmap.T      );
(* Paint a "diamond" shadow in VBT v in the specified style, clipped
   against the specified region. The shadow is a diamondular tube, whose
   inner and outer vertices are the midpoints of in and out rectangles.
   Styles Chiseled and Ridged are (incorrectly) implemented as Lowered and
   Raised respectively. The inside of the diamond is filled with the given
   pixmap in the given colors. *)

END ShadowPaint.


