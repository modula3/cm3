(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Feb  6 18:05:20 PST 1993 by meehan     *)
(*      modified on Sat Jan 30 00:01:31 PST 1993 by mhb        *)
(*      modified on Tue Jun 16 13:08:17 PDT 1992 by muller     *)


MODULE ShadowedBarVBT;

IMPORT Axis, Region, Shadow, ShadowPaint, VBT;

REVEAL
  T = Public BRANDED OBJECT
        axis  : Axis.T;
        shadow: Shadow.T;
        style : Shadow.Style;
      OVERRIDES
        init    := Init;
        shape   := Shape;
        repaint := Repaint;
      END;

PROCEDURE Init (v     : T;
                axis  : Axis.T;
                shadow: Shadow.T     := NIL;
                style : Shadow.Style := Shadow.Style.Flat): T =
  BEGIN
    IF shadow = NIL THEN shadow := Shadow.None; END;
    v.axis := axis;
    v.shadow := shadow;
    v.style := style;
    RETURN v
  END Init;

PROCEDURE Set (v: T; shadow: Shadow.T) =
  BEGIN
    IF v.shadow.size # shadow.size THEN VBT.NewShape (v);  END;
    v.shadow := shadow;
    VBT.Mark (v);
  END Set;

PROCEDURE SetStyle (v: T; style: Shadow.Style) =
  BEGIN
    v.style := style;
    VBT.Mark(v);
  END SetStyle;

PROCEDURE Shape (v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  VAR sr: VBT.SizeRange;
  BEGIN
    IF v.axis = ax THEN
      sr.lo := ROUND(VBT.MMToPixels(v, ABS(v.shadow.size), ax));
      sr.pref := sr.lo;
      sr.hi := sr.lo + 1;
      RETURN sr;
    ELSE
      RETURN VBT.Leaf.shape(v, ax, n)
    END;
  END Shape;

PROCEDURE Repaint (v: T; READONLY rgn: Region.T) =
  BEGIN
    ShadowPaint.Bar(v, rgn, v.shadow, v.style, v.axis, VBT.Domain (v));
  END Repaint;

BEGIN
END ShadowedBarVBT.
