(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Stephen Harrison                                    *)
(* Last modified on Fri Aug 19 16:24:45 PDT 1994 by steveg   *)
(*      modified on Tue Jul 21 20:26:45 PDT 1992 by harrison *)


INTERFACE MGPublic;

<* PRAGMA LL *>

(* This interface provides the convenience routines for manipulating MG
   elements *)
   
IMPORT Color, Font, Matrix2D, MG, PaintOp, R2, R2Box, VBT, R2Path;

<* LL < v.mu for all procedures *>
<* INLINE *> PROCEDURE Pos(t: MG.T; v: MG.V): R2.T;
(* Return the position of t.  Exact interpretation depends on the 
   subtype.  Logically it returns t.m * R2.Origin *)

<* INLINE *> PROCEDURE LineFrom(line: MG.Line; v: MG.V): R2.T;
<* INLINE *> PROCEDURE LineTo(line: MG.Line; v: MG.V): R2.T;

<* INLINE *> PROCEDURE RectangleSW(rectangle: MG.Rectangle; v: MG.V): R2.T;
<* INLINE *> PROCEDURE RectangleNE(rectangle: MG.Rectangle; v: MG.V): R2.T;

<* INLINE *> PROCEDURE EllipseSW(ellipse: MG.Ellipse; v: MG.V): R2.T;
<* INLINE *> PROCEDURE EllipseNE(ellipse: MG.Ellipse; v: MG.V): R2.T;

<* INLINE *> PROCEDURE ShapeOrigin(shape: MG.Shape; v: MG.V): R2.T;
<* INLINE *> PROCEDURE ShapePath(shape: MG.Shape; v: MG.V := NIL): R2Path.T;

PROCEDURE IterateGroup (group      : MG.Group;
                        iter       : MG.GroupIterator;
                        recursively                     := FALSE;
                        fromBack                        := TRUE   ):
  BOOLEAN;
(* If "recursively" then groups within "group" are also iterated so that
   all leaf nodes are depth first.  Group nodes are visited after all of
   their descendants.

   IF "fromBack" then the nodes are visited in visual back to front order
   else from front to back.

   Result is false if any proc returned FALSE else TRUE *)

PROCEDURE AddToGroup (group: MG.Group; v: MG.V; t: MG.T; atTop := TRUE);
PROCEDURE AddToGroupAfter (group: MG.Group;
                           v    : MG.V;
                           t    : MG.T;
                           prev : MG.T       := NIL);
PROCEDURE AddToGroupBefore (group: MG.Group;
                            v    : MG.V;
                            t    : MG.T;
                            next : MG.T       := NIL);
PROCEDURE RemoveFromGroup (group: MG.Group; v: MG.V; t: MG.T);
PROCEDURE TopInGroup (group: MG.Group; v: MG.V; t: MG.T);
PROCEDURE BottomInGroup (group: MG.Group; v: MG.V; t: MG.T);

PROCEDURE Register (v: MG.V; id: MG.ID; t: MG.T);
(* Register "t" in "v"'s with "id" *)

PROCEDURE Lookup (v: MG.V; id: MG.ID): MG.T;
(* Lookup "id" in "v"'s lookup table and return the corresponding element.
   Returns NIL if id = NoID or id is not registered with v. *)

PROCEDURE Clear(v: MG.V);
(* Clear "v" of any visual state -
   Reset the lookup table, clear the display list, delete any animations *)

PROCEDURE ResetLookups(v: MG.V);
(* Clear the lookup table for "v". *)

PROCEDURE BoundingBox (t: MG.T; v: MG.V): R2Box.T;
PROCEDURE SetColor (t: MG.T; v: MG.V; color: PaintOp.ColorScheme);
PROCEDURE SetFont (t: MG.T; v: MG.V; font: Font.T);
PROCEDURE SetHighlight (t: MG.T; v: MG.V; highlight := 1.0);
PROCEDURE SetVisible (t: MG.T; v: MG.V; visible := 1.0);
PROCEDURE SetWeight (t: MG.T; v: MG.V; weight := 1.0);
PROCEDURE SetAlignment (t: MG.T; v: MG.V; alignment := MG.Alignment.Center);
PROCEDURE SetLabel (t: MG.T; v: MG.V; label := "");
PROCEDURE SetAppearance (t: MG.T; v: MG.V; appearance: MG.Appearance := NIL);

PROCEDURE SetLineStyle (line: MG.Line; v: MG.V; style := VBT.EndStyle.Round);
PROCEDURE SetLineReshape (line: MG.Line; v: MG.V; READONLY from, to: R2.T);
PROCEDURE SetRectangleReshape (         rect            : MG.Rectangle;
                                        v               : MG.V;
                               READONLY corner1, corner2: R2.T          );

PROCEDURE RTranslate (t: MG.T; v: MG.V; READONLY by: R2.T);
(* translate t.pos by "by" *)

PROCEDURE TranslateTo (         t     : MG.T;
                                v     : MG.V;
                       READONLY dest  : R2.T;
                                center         := FALSE);
(* Convenience procedures to move t.pos to "dest" (or to center t around
   "dest" if "center") *)

PROCEDURE Scale (         t     : MG.T;
                          v     : MG.V;
                 READONLY factor         := R2.Ones;
                 READONLY wrt            := R2.Origin);
(* scale t by "factor[0]" horizontally and "factor[1]" vertically, with
   respect to the point "wrt" *)

PROCEDURE Rotate (         t     : MG.T;
                           v     : MG.V;
                           angle : REAL;
                  READONLY origin         := R2.Origin);
(* Rotate t around "origin" by "angle" degrees *)

PROCEDURE Transform (t: MG.T; v: MG.V; READONLY m: Matrix2D.T);
(* Transform "t" by "m" *)

<* LL arbitrary for following procedures *>
VAR defaultColor: PaintOp.ColorScheme;

PROCEDURE OpFromName (name: TEXT; mode := PaintOp.Mode.Accurate):
  PaintOp.T;

PROCEDURE ColorFromText (bg, fg: TEXT := NIL; mode := PaintOp.Mode.Accurate):
  PaintOp.ColorScheme;
PROCEDURE ColorFromRGB (bg, fg: Color.T; mode := PaintOp.Mode.Accurate):
  PaintOp.ColorScheme;
(* Returns the color scheme for the colors specifed by "bg" and "fg" (for
   "color" the texts are interpreted by ColorName.ToRGB) *)

END MGPublic.
