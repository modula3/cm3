(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman and Stephen Harrison *)
(* Last modified on Wed Jul 22 01:41:52 1992 by steveg *)

<*PRAGMA LL*>

MODULE Grid;

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

PROCEDURE Init (v: V; rows, cols: CARDINAL; width, height: REAL): V =
  BEGIN
    EVAL MGV.V.init(v);
    v.a := NEW(Array, rows, cols);
    v.group := NEW(MG.Group).init(v := v);
    v.size[Axis.T.Hor] := FLOAT(cols) * width;
    v.size[Axis.T.Ver] := FLOAT(rows) * height;
    WITH hor = v.border[Axis.T.Hor],
         ver = v.border[Axis.T.Ver]  DO
      FOR i := 0 TO rows - 1 DO
        FOR j := 0 TO cols - 1 DO
          v.a[i, j] := NEW(MG.Rectangle).init(
                         R2.T{hor + FLOAT(j) * width,
                              -ver - FLOAT(i) * height},
                         R2.T{hor + FLOAT(j + 1) * width,
                              -ver - FLOAT(i + 1) * height}, v, v.group);
        END;
      END;
    END;
    RETURN v;
  END Init;

BEGIN
END Grid.
