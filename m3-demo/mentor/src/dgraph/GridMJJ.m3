(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman and Stephen Harrison *)
(* Last modified on Thu Aug  6 17:10:58 PDT 1992 by mjordan *)
(*      modified on Wed Jul 22 01:41:52 1992 by steveg *)

<*PRAGMA LL*>

MODULE GridMJJ;

IMPORT Axis, MG, MGV, 
       Pts, R2, VBT;

REVEAL
  V = PublicV BRANDED OBJECT
      OVERRIDES
        init    := Init;
        reshape := MGV.ReshapeLeaveOrigin;
        shape   := Shape;
      END;

PROCEDURE Shape (v: V; ax: Axis.T; <* UNUSED *> n: CARDINAL):
  VBT.SizeRange =
  BEGIN
    WITH pref = Pts.ToScreenPixels(v, v.size[ax] + 2.0 * v.border[ax], ax) DO
      RETURN VBT.SizeRange{0, pref, MAX(pref + 1, VBT.DefaultShape.hi)}
    END;
  END Shape;

PROCEDURE Init (v               : V;
                rows, cols      : CARDINAL;
                width, height   : REAL;
                borderW, borderH             := 0.0): V =
  VAR
    outerW := width + 2.0 * borderW;
    outerH := height + 2.0 * borderH;
  BEGIN
    EVAL MGV.V.init(v);
    v.nonOverlappingElements := TRUE; v.doubleBuffer := FALSE;
    v.a := NEW(Array, rows, cols);
    v.group := NEW(MG.Group).init(v := v);
    v.size[Axis.T.Hor] := FLOAT(cols) * outerW;
    v.size[Axis.T.Ver] := FLOAT(rows) * outerH;
    VAR hor := 0.0;
        ver := -v.border[Axis.T.Ver];
    BEGIN
      FOR i := 0 TO rows - 1 DO
        hor := v.border[Axis.T.Hor];
        FOR j := 0 TO cols - 1 DO
          v.a[i, j] :=
            NEW(MG.Rectangle, weight := 2.0).init(
              R2.T{hor + borderW, ver - borderH},
              R2.T{hor + borderW + width, ver - borderH - height}, v, v.group);
          hor := hor + outerW;
        END;
        ver := ver - outerH;
      END;
    END;
    RETURN v;
  END Init;

BEGIN
END GridMJJ.
