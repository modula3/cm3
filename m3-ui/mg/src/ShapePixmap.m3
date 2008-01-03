(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Stephen Harrison and Steve Glassman *)
(* *)
(* Last modified on Tue Jan 31 11:53:02 PST 1995 by kalsow   *)
(*      modified on Fri Aug 19 16:39:04 PDT 1994 by steveg   *)
(*      modified on Tue Jul 21 20:28:25 PDT 1992 by harrison *)

UNSAFE MODULE ShapePixmap;

IMPORT MGV, Pixmap, ScrnPixmap, Palette, ScreenType, Rect, Path,
       R2PathExtra, VBT, Region, PaintOp;

TYPE
  Closure = Palette.PixmapClosure OBJECT
              pixmap: Pixmap.T;
              path: Path.T;
              border: CARDINAL;
              fill: BOOLEAN;
              v: MGV.V;
            OVERRIDES
              apply := Apply;
            END;

PROCEDURE Apply (self: Closure; st: ScreenType.T): ScrnPixmap.T =
  VAR bounds := R2PathExtra.PathBounds(self.path);
  BEGIN
    (* We're only interested in 1-bit deep pixmaps *)
    IF st # st.bits THEN
      RETURN Palette.ResolvePixmap(st.bits, self.pixmap);
    END;

    IF NOT self.fill THEN bounds := Rect.Inset(bounds, -self.border); END;

    VAR shapeVBT := self.v.shapeVBT;
    BEGIN
      IF shapeVBT = NIL THEN
        RETURN Palette.ResolvePixmap(st, Pixmap.Solid);
      END;

      VBT.PaintTint(shapeVBT, bounds, PaintOp.Bg);
      IF self.fill THEN
        VBT.Fill(shapeVBT, bounds, self.path);
      ELSE
        VBT.Stroke(shapeVBT, bounds, self.path, self.border);
      END;

      VAR
        dummy_region: Region.T;
        res := VBT.Capture(shapeVBT, bounds, dummy_region);
      BEGIN
        RETURN res;
      END;
    END;
  END Apply;

PROCEDURE New (READONLY path  : Path.T;
               READONLY border: CARDINAL := 0;
               READONLY fill             := TRUE;
               READONLY v     : MGV.V             ): Pixmap.T =
  VAR
    cl  := NEW(Closure, path := path, border := border, fill := fill, v := v);
    res := Palette.FromPixmapClosure(cl);
  BEGIN
    cl.pixmap := res;
    RETURN res;
  END New;

BEGIN
END ShapePixmap.

