(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Jun  5 14:22:27 PDT 1995 by najork                   *)
(*       Created on Mon Jun  5 09:41:01 PDT 1995 by najork                   *)


MODULE Test29 EXPORTS Main;

IMPORT Axis, Point, Rect, Region, Trestle, TrestleComm, VBT;

<* FATAL TrestleComm.Failure *>


VAR 
  r := Rect.T {0, 200, 0, 200};


PROCEDURE Shape (<*UNUSED*> v : VBT.T;
                 <*UNUSED*> ax: Axis.T;
                 <*UNUSED*> n : CARDINAL): VBT.SizeRange =
  BEGIN
    RETURN VBT.SizeRange{lo := 200, pref := 200, hi := 201}
  END Shape;


PROCEDURE Position (v: VBT.Leaf; READONLY cd: VBT.PositionRec) =
  VAR
    srec := Trestle.ScreenOf (v, cd.cp.pt);
    sdom := srec.dom;
    p    := srec.q;
  BEGIN
    p.h := MAX (MIN (p.h, sdom.east  - 100), sdom.west  + 100);
    p.v := MAX (MIN (p.v, sdom.south - 100), sdom.north + 100);
    r := Rect.T {p.h - 100, p.h + 100, p.v - 100, p.v + 100};
    VBT.SetCage (v, VBT.CageFromPosition (cd.cp, trackOutside := TRUE));
    VBT.Mark (v);
  END Position;


PROCEDURE Repaint (self: VBT.Leaf; <* UNUSED *> READONLY rgn: Region.T) =
  VAR
    br : Region.T;
  BEGIN
    WITH id = Trestle.ScreenOf (self, Point.Origin).id,
         pm = Trestle.Capture (id, r, br) DO
      VBT.PaintScrnPixmap (self, src := pm, delta := Point.Origin);
      VBT.Sync (self);
      pm.free();
    END;
  END Repaint;


BEGIN
  WITH v = NEW (VBT.Leaf, repaint  := Repaint, 
                          shape    := Shape, 
                          position := Position) DO
    Trestle.Install (v);
    Trestle.AwaitDelete (v);
  END;
END Test29.
