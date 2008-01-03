(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Sat Jun 10 18:00:39 PDT 1995 by najork                   *)
(*       Created on Sat Jun 10 17:42:32 PDT 1995 by najork                   *)


MODULE Main;

IMPORT Axis, PaintOp, Point, Rect, Region, Trestle, TrestleComm, VBT;

<* FATAL TrestleComm.Failure *>

PROCEDURE Shape (<*UNUSED*> v : VBT.T;
                            ax: Axis.T;
                 <*UNUSED*> n : CARDINAL): VBT.SizeRange =
  BEGIN
    IF ax = Axis.T.Hor THEN
      RETURN VBT.SizeRange{lo := 440, pref := 440, hi := 441};
    ELSE
      RETURN VBT.SizeRange{lo := 300, pref := 300, hi := 301};
    END;
  END Shape;


PROCEDURE Repaint (v: VBT.T; <*UNUSED*> READONLY bad: Region.T) =

  PROCEDURE PaintSlice (r: INTEGER; nw: Point.T) =
    BEGIN
      FOR g := 0 TO 5 DO
        FOR b := 0 TO 5 DO
          WITH red   = FLOAT (r) / 5.0,
               green = FLOAT (g) / 5.0, 
               blue  = FLOAT (b) / 5.0,
               op    = PaintOp.FromRGB (red, green, blue),
               off   = Point.T {g * 20, b * 20},
               rect  = Rect.FromCorner (Point.Add (nw, off), 20, 20) DO
            VBT.PaintTint (v, rect, op);
          END;
        END;
      END;
    END PaintSlice;

  BEGIN
    VBT.PaintTint (v, Rect.Full, PaintOp.Bg);
    PaintSlice (0, Point.T { 20, 20});
    PaintSlice (1, Point.T {160, 20});
    PaintSlice (2, Point.T {300, 20});
    PaintSlice (3, Point.T { 20,160});
    PaintSlice (4, Point.T {160,160});
    PaintSlice (5, Point.T {300,160});
  END Repaint;


BEGIN
  WITH v = NEW (VBT.Leaf, shape := Shape, repaint := Repaint) DO
    Trestle.Install (v);
    Trestle.AwaitDelete (v);
  END;
END Main.
