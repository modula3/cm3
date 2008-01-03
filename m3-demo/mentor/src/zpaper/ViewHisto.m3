(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Fri Jan  6 00:42:23 PST 1995 by najork     *)
(*      modified on Wed Jan  6 16:45:45 PST 1993 by steveg     *)
(*      modified on Mon Sep 14 22:17:10 PDT 1992 by mhb        *)

MODULE ViewHisto;

IMPORT ZPaperViewClass, Color, PaintOp, RectsVBT, View,
       ZeusPanel;

TYPE
  RefOps = REF ARRAY OF PaintOp.T;
  RefCts = REF ARRAY OF INTEGER;
  T = ZPaperViewClass.T BRANDED OBJECT
        k    : INTEGER;
        ops  : RefOps;
        cts  : RefCts;
        rects: RectsVBT.T;
      OVERRIDES
        oeInit := Init;
        oeSet  := Set;
      END;

PROCEDURE Init (view: T; N, k: INTEGER) =
  BEGIN
    view.k := k;
    RectsVBT.SetMargin(view.rects, 2.0, 2.0, 2.0, 2.0);
    RectsVBT.SetWC(
      view.rects, 0.0, -2.0, FLOAT(k), FLOAT(10 + N * N DIV (2 * k)));
    RectsVBT.SetN(view.rects, k);
    view.ops := NEW(RefOps, k);
    view.cts := NEW(RefCts, k);
    FOR i := 0 TO k - 1 DO
      view.cts[i] := 1;
      WITH rgb = Color.FromHSV(
                   Color.HSV{FLOAT(i) * 1.0 / FLOAT(k), 1.0, 1.0}) DO
        view.ops[i] :=
          PaintOp.FromRGB(rgb.r, rgb.g, rgb.b, PaintOp.Mode.Accurate)
      END
    END
  END Init;

PROCEDURE Set (view: T; <* UNUSED *> x, y: INTEGER; val:  INTEGER) =
  VAR
    k  := view.k;
    ix := val MOD k;
  BEGIN
    INC(view.cts[ix]);
    RectsVBT.Position(view.rects, ix, FLOAT(ix), 0.0,
                      FLOAT(ix + 1), FLOAT(view.cts[ix]));
    RectsVBT.Color(view.rects, ix, view.ops[ix]);
    RectsVBT.Draw(view.rects, ix);
    RectsVBT.Erase(view.rects, k);
    RectsVBT.Position(
      view.rects, k, FLOAT(ix), -2.0, FLOAT(ix + 1), -1.0);
  END Set;

PROCEDURE New (): View.T =
  VAR rects := NEW(RectsVBT.T).init();
  BEGIN
    RETURN NEW(T, rects := rects).init(rects)
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Histogram", "ZPaper");
END ViewHisto.
