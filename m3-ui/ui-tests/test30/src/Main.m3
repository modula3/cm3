(* Copyright (C) 1991, Digital Equipment Corporation              *)
(* Copyright 1990 David Lemke and Network Computing Devices       *)
(* Copyright (c) 1989, Donald R. Woods and Sun Microsystems, Inc. *)
(* All rights reserved.                                           *)
(* See the file COPYRIGHT for a full description.                 *)

(* Last modified on Fri Jun  9 14:04:05 PDT 1995 by najork        *)
(*      modified on Tue Jan 31 10:27:31 PST 1995 by kalsow        *)
(*      modified on Wed Jul 14 16:41:03 PDT 1993 by msm           *)


MODULE Main;

IMPORT Point, VBT, Rect, Axis, PaintOp, Region, Trestle, TrestleComm,
       Pixmap, ScrnPixmap;


<* FATAL TrestleComm.Failure  *>


PROCEDURE P(): Pixmap.T =
  VAR 
    r := ScrnPixmap.NewRaw (1, Rect.FromSize(20, 20));
  BEGIN
    FOR v := 0 TO 19 DO
      FOR h := 0 TO 19 DO
        r.set (Point.T {h, v}, 1);
      END
    END;
    RETURN Pixmap.FromBitmap (r);
  END P;


PROCEDURE Shape (<*UNUSED*> ch: VBT.T;
                 <*UNUSED*> ax: Axis.T;
                 <*UNUSED*> n : CARDINAL): VBT.SizeRange =
  BEGIN
    RETURN VBT.SizeRange{lo := 100, pref := 100, hi := 101}
  END Shape;


PROCEDURE Repaint (v: VBT.T; READONLY bad: Region.T) =
  VAR
    realRed   := PaintOp.FromRGB (0.75, 0.0, 0.0, bw := PaintOp.BW.UseFg);
    red       := PaintOp.Pair(PaintOp.Bg, realRed);
    transpRed := PaintOp.Pair(PaintOp.Transparent, realRed);
    pm        := P ();
  BEGIN
    VBT.PaintTint (v, Rect.Full, PaintOp.Bg);

    VBT.PaintPixmap(v, bad.r, transpRed, pm, Point.T {20, 20});
    VBT.PaintPixmap(v, bad.r, red, pm, Point.T {60, 20});

    VBT.PaintTint (v, Rect.T{20,40,60,80}, red);
    VBT.PaintTint (v, Rect.T{60,80,60,80}, transpRed);
  END Repaint;


BEGIN
  WITH v = NEW (VBT.Leaf, repaint := Repaint, shape := Shape) DO
    Trestle.Install(v);
    Trestle.AwaitDelete(v);
  END;
END Main.
