(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Mon Apr 19 03:41:31 PDT 1993 by msm    *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE MODULE XScrnPntOp;

IMPORT PaintOp, ScreenType, ScrnPaintOp, TrestleClass, VBTClass, Word, X,
         XScreenType, XScrnTpRep;

REVEAL
  T = T_Pub BRANDED OBJECT
        opcount: CARDINAL := 0;
        (* numbers of entries in optable. *)
      END;

TYPE
  XPaintOp = ScrnPaintOp.T;
  OpOracle = ScrnPaintOp.Oracle OBJECT
               st: XScreenType.T
             OVERRIDES
               opaque      := Opaque;
               bgfg        := Bgfg;
               swap        := Swap;
               transparent := Transparent;
               copy        := Copy;
               builtIn     := OpBuiltIn;
             END;

PROCEDURE NewOracle (st: XScreenType.T): ScrnPaintOp.Oracle =
  BEGIN
    RETURN NEW(OpOracle, st := st)
  END NewOracle;

PROCEDURE Opaque (orc: OpOracle; pix: ScrnPaintOp.Pixel): ScrnPaintOp.T
  RAISES {} =
  VAR rec: XScrnTpRep.OpRecord;
  BEGIN
    rec.function := X.GXcopy;
    rec.fill_style := X.FillSolid;
    rec.plane_mask := -1;
    rec.foreground := pix;
    rec.background := pix;
    RETURN NewPaintOp(orc.st, rec, pix)
  END Opaque;

PROCEDURE Swap (orc: OpOracle; p, q: ScrnPaintOp.Pixel): ScrnPaintOp.T
  RAISES {} =
  VAR rec: XScrnTpRep.OpRecord;
  BEGIN
    IF p = q THEN RETURN Transparent(orc) END;
    rec.function := X.GXxor;
    rec.fill_style := X.FillSolid;
    rec.plane_mask := -1;
    rec.foreground := Word.Xor(p, q);
    rec.background := Word.Xor(p, q);
    RETURN NewPaintOp(orc.st, rec)
  END Swap;

PROCEDURE Transparent (orc: OpOracle): ScrnPaintOp.T RAISES {} =
  VAR rec: XScrnTpRep.OpRecord;
  BEGIN
    rec.function := X.GXnoop;
    rec.fill_style := X.FillSolid;
    rec.plane_mask := -1;
    rec.foreground := 0;
    rec.background := 0;
    RETURN NewPaintOp(orc.st, rec)
  END Transparent;

PROCEDURE Copy (orc: OpOracle): ScrnPaintOp.T RAISES {} =
  VAR rec: XScrnTpRep.OpRecord;
  BEGIN
    rec.function := X.GXcopy;
    rec.fill_style := X.FillTiled;
    rec.plane_mask := -1;
    rec.foreground := 0;
    rec.background := 0;
    RETURN NewPaintOp(orc.st, rec)
  END Copy;

PROCEDURE Bgfg (orc: OpOracle; bg, fg: ScrnPaintOp.T): ScrnPaintOp.T
  RAISES {ScrnPaintOp.Failure} =
  VAR rec: XScrnTpRep.OpRecord;
  BEGIN
    LOCK orc.st.trsl DO
      IF (bg.id < 0) OR (bg.id >= orc.st.opcount) OR (fg.id < 0)
           OR (fg.id >= orc.st.opcount) THEN
        RAISE ScrnPaintOp.Failure
      END;
      WITH bgrec = orc.st.optable[bg.id],
           fgrec = orc.st.optable[fg.id]  DO
        IF (bgrec.function = X.GXnoop)
             OR (bgrec.fill_style = X.FillStippled) THEN
          rec := fgrec;
          rec.fill_style := X.FillStippled
        ELSIF (bgrec.function = fgrec.function)
                AND (bgrec.plane_mask = fgrec.plane_mask) THEN
          rec := fgrec;
          rec.background := bgrec.background;
          IF rec.background = rec.foreground THEN
            rec.fill_style := X.FillSolid
          ELSE
            rec.fill_style := X.FillOpaqueStippled
          END
        ELSE
          RAISE ScrnPaintOp.Failure
        END
      END
    END;
    RETURN NewPaintOp(orc.st, rec)
  END Bgfg;

PROCEDURE OpBuiltIn (orc: OpOracle; op: PaintOp.Predefined):
  ScrnPaintOp.T =
  VAR rec: XScrnTpRep.OpRecord;
  BEGIN
    rec.plane_mask := -1;
    rec.fill_style := X.FillOpaqueStippled;
    CASE op OF
      PaintOp.Bg.op => RETURN Opaque(orc, orc.st.bg)
    | PaintOp.Fg.op => RETURN Opaque(orc, orc.st.fg)
    | PaintOp.Transparent.op => RETURN Transparent(orc)
    | PaintOp.Swap.op => RETURN Swap(orc, orc.st.bg, orc.st.fg)
    | PaintOp.Copy.op => RETURN Copy(orc)
    | PaintOp.BgFg.op =>
        rec.function := X.GXcopy;
        rec.foreground := orc.st.fg;
        rec.background := orc.st.bg
    | PaintOp.FgBg.op =>
        rec.function := X.GXcopy;
        rec.foreground := orc.st.bg;
        rec.background := orc.st.fg
    | PaintOp.TransparentBg.op =>
        rec.function := X.GXcopy;
        rec.fill_style := X.FillStippled;
        rec.foreground := orc.st.bg;
        rec.background := 0
    | PaintOp.TransparentFg.op =>
        rec.function := X.GXcopy;
        rec.fill_style := X.FillStippled;
        rec.foreground := orc.st.fg;
        rec.background := 0
    | PaintOp.TransparentSwap.op =>
        rec.function := X.GXxor;
        rec.foreground := Word.Xor(orc.st.bg, orc.st.fg);
        rec.background := 0
    | PaintOp.SwapTransparent.op =>
        rec.function := X.GXxor;
        rec.foreground := 0;
        rec.background := Word.Xor(orc.st.bg, orc.st.fg);
    ELSE
      WITH ones = Word.Shift(1, orc.st.depth) - 1,
           bg   = orc.st.bg,
           fg   = orc.st.fg                        DO
        IF ((fg # 0) AND (fg # ones)) OR ((bg # 0) AND (bg # ones))
             OR (bg = fg) THEN
          RETURN Transparent(orc)
        ELSE
          CASE op OF
          | PaintOp.BgTransparent.op =>
              IF bg = 0 THEN
                rec.function := X.GXand;
                rec.foreground := ones;
                rec.background := 0
              ELSE              (* bg = ones *)
                rec.function := X.GXor;
                rec.foreground := 0;
                rec.background := ones
              END;
          | PaintOp.FgTransparent.op =>
              IF fg = 0 THEN
                rec.function := X.GXand;
                rec.foreground := ones;
                rec.background := 0
              ELSE              (* fg = ones *)
                rec.function := X.GXor;
                rec.foreground := 0;
                rec.background := ones
              END;
          | PaintOp.BgSwap.op =>
              IF bg = 0 THEN
                rec.function := X.GXnor;
                rec.foreground := 0;
                rec.background := ones
              ELSE
                rec.function := X.GXnand;
                rec.foreground := ones;
                rec.background := 0
              END
          | PaintOp.FgSwap.op =>
              IF fg = 0 THEN
                rec.function := X.GXnor;
                rec.foreground := 0;
                rec.background := ones
              ELSE
                rec.function := X.GXnand;
                rec.foreground := ones;
                rec.background := 0
              END
          | PaintOp.SwapBg.op =>
              IF bg = 0 THEN
                rec.function := X.GXnor;
                rec.foreground := ones;
                rec.background := 0
              ELSE
                rec.function := X.GXnand;
                rec.foreground := 0;
                rec.background := ones
              END
          | PaintOp.SwapFg.op =>
              IF fg = 0 THEN
                rec.function := X.GXnor;
                rec.foreground := ones;
                rec.background := 0
              ELSE
                rec.function := X.GXnand;
                rec.foreground := 0;
                rec.background := ones
              END
          ELSE
            RETURN Transparent(orc)
          END
        END
      END
    END;
    RETURN NewPaintOp(orc.st, rec)
  END OpBuiltIn;

PROCEDURE NewPaintOp (VAR      st : XScreenType.T;
                      READONLY rec: XScrnTpRep.OpRecord;
                               pix                        := -1):
  XPaintOp =
  VAR res := NEW(XPaintOp, pix := pix);
  BEGIN
    LOCK st.trsl DO
      WITH n = NUMBER(st.optable^) DO
        IF n = st.opcount THEN
          WITH new = NEW(REF ARRAY OF XScrnTpRep.OpRecord, 2 * n) DO
            FOR i := 0 TO n - 1 DO new[i] := st.optable[i] END;
            st.optable := new
          END
        END
      END;
      res.id := st.opcount;
      st.optable[res.id] := rec;
      INC(st.opcount)
    END;
    RETURN res
  END NewPaintOp;

BEGIN
END XScrnPntOp.
