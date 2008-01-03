(* Copyright (C) 1993, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Apr  4 11:36:46 PDT 1995 by msm                      *)
<* PRAGMA LL *>

MODULE OverlayVBT;

IMPORT VBTClass, Region, VBT, Rect, ScrnPixmap, Batch, BatchUtil, Filter, 
       BatchRep;

REVEAL
  T = Public BRANDED OBJECT
        active := FALSE;
        rgn    := Region.Empty;
      OVERRIDES
        init       := Init;
        set        := Set;
        paint      := Paint;
        repaint    := Repaint;
        reshape    := Reshape;
        capture    := Capture;
        paintbatch := PaintBatch;
      END;

PROCEDURE Init (v: T; ch: VBT.T): T =
  BEGIN
    EVAL Filter.T.init(v, ch);
    RETURN v;
  END Init;

PROCEDURE Set (v: T; READONLY rgn: Region.T) =
  VAR diff := Region.SymmetricDifference(rgn, v.rgn);
  BEGIN
    LOCK v DO v.active := NOT Region.IsEmpty(rgn); v.rgn := rgn END;
    Repaint(v, diff);
  END Set;

PROCEDURE Paint(<* UNUSED *> v: T; <* UNUSED *> READONLY rgn: Region.T) =
  BEGIN
    (* The default paint procedure is a no-op *)
  END Paint;

PROCEDURE Repaint (v: T; READONLY rgn: Region.T) =
  BEGIN
    Filter.T.repaint(v, rgn);
    IF v.active THEN v.paint(Region.Meet(rgn, v.rgn)) END;
  END Repaint;

PROCEDURE Reshape (v: T; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    Filter.T.reshape(v, cd);
    IF v.active THEN v.paint(Region.MeetRect(cd.new, v.rgn)) END;
  END Reshape;

PROCEDURE Capture (               v : T;
                   <* UNUSED *> ch  : VBT.T;
                       READONLY rect: Rect.T;
                       VAR      br  : Region.T): ScrnPixmap.T =
  VAR res := VBT.Capture(v, rect, br);
  BEGIN
    IF v.active THEN br := Region.Join(br, v.rgn) END;
    RETURN res
  END Capture;

PROCEDURE PaintBatch (v: T; ch: VBT.T; ba: Batch.T) =
  VAR rect: Rect.T;
  BEGIN
    IF NOT v.active THEN
      VBTClass.PaintBatch(v, ba)
    ELSE
      BatchUtil.Tighten(ba);
      IF Rect.IsEmpty(ba.scrollSource) THEN
        rect := ba.clip;
	VBT.BeginGroup(v);
        VBTClass.PaintBatch(v, ba);
        v.paint(Region.FromRect(rect));
	VBT.EndGroup(v);
      ELSE
        VBTClass.ForceRepaint(ch, Region.FromRect(ba.clip));
        Batch.Free(ba);
      END
    END
  END PaintBatch;

BEGIN
END OverlayVBT.
