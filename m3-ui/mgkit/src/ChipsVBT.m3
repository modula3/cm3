(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Tue Jan  5 15:54:22 PST 1993 by steveg *)
(*      modified on Sun Jul 19 19:27:17 1992 by mhb *)

MODULE ChipsVBT;

IMPORT Color, PaintOp, RectsVBT, VBTClass;

TYPE RefOps = REF ARRAY OF PaintOp.T;

REVEAL
  T = Public BRANDED OBJECT
        (* fields are protected by v: *)
        C, R, K: INTEGER;
        ops    : RefOps;
      OVERRIDES
        init := Init;
      END;

PROCEDURE Init (v: T): T =
  BEGIN
    RETURN RectsVBT.T.init(v)
  END Init;

PROCEDURE Reset (v: T; C, R, K: INTEGER) =
  BEGIN
    LOCK v DO
      v.C := MAX(C, 1);
      v.R := MAX(R, 1);
      v.K := MAX(K, 1);
      v.ops := NEW(RefOps, K);
      FOR i := 0 TO K - 1 DO
        WITH rgb = Color.FromHSV(
                     Color.HSV{FLOAT(i) * 1.0 / FLOAT(K), 1.0, 1.0}) DO
          v.ops[i] :=
            PaintOp.FromRGB(rgb.r, rgb.g, rgb.b, PaintOp.Mode.Accurate)
        END
      END
    END;
    RectsVBT.SetN(v, C * R);
    RectsVBT.SetWC(v, 0.0, 0.0, FLOAT(C), FLOAT(R))
  END Reset;

PROCEDURE Set (v: T; c, r, k: INTEGER) =
  VAR
    ix: INTEGER;
    op: PaintOp.T;
  BEGIN
    LOCK v DO
      IF r < 1 OR c < 1 OR k < 1 OR r > v.R OR c > v.C OR k > v.K THEN
        RETURN
      END;
      ix := (r - 1) * v.C + c;
      op := v.ops[k - 1];
    END;
    RectsVBT.Position(
      v, ix, FLOAT(c - 1), FLOAT(r - 1), FLOAT(c), FLOAT(r));
    RectsVBT.Color(v, ix, op);
    RectsVBT.Draw(v, ix)
  END Set;

BEGIN
END ChipsVBT.


