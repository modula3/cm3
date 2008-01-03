(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Mon Apr  3 15:19:02 PDT 1995 by msm     *)
(*      modified on Fri Mar 11 16:59:42 PST 1994 by gnelson *)
(*      modified on Mon Nov 22 13:43:22 PST 1993 by steveg  *)
(*      modified on Fri May  7 17:10:42 PDT 1993 by mjordan *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE MODULE XGC;

IMPORT X, XScreenType, PaintPrivate, Point, TrestleComm, VBT,
       XScrnTpRep, Ctypes, Rect, ScreenType, XScrnPxmp, XPaint;

REVEAL
  T = T_Pub BRANDED OBJECT
        textureGC := ARRAY [0 .. 15] OF TextureGCRec{TextureGCRec{}, ..};
        tintGC    := ARRAY [0 .. 7] OF TintGCRec{TintGCRec{}, ..};
        pixmapGC  := ARRAY [0 .. 3] OF PixmapGCRec{PixmapGCRec{}, ..};
        scrollGC  := ARRAY [0 .. 1] OF ScrollGCRec{ScrollGCRec{}, ..};
        textGC    := ARRAY [0 .. 7] OF TextGCRec{TextGCRec{}, ..};
        fillGC    := ARRAY [0 .. 1] OF FillGCRec{FillGCRec{}, ..};
        strokeGC  := ARRAY [0 .. 1] OF StrokeGCRec{StrokeGCRec{}, ..};
      END;

CONST NullPaintOp: PaintPrivate.PaintOp = -1;

TYPE
  TextureGCRec = RECORD
                   gc   : X.GC                := NIL;
                   op                         := NullPaintOp;
                   pm   : PaintPrivate.Pixmap := 0;
                   delta                      := Point.Origin;
                 END;

  TintGCRec = RECORD
                gc: X.GC := NIL;
                op       := NullPaintOp;
              END;

  PixmapGCRec = RECORD
                  gc   : X.GC                := NIL;
                  op                         := NullPaintOp;
                  pm   : PaintPrivate.Pixmap := 0;
                  delta                      := Point.Origin;
                  mode                       := FIRST(XMode);
                  src  : X.Pixmap            := X.None;
                END;

  ScrollGCRec = RECORD
                  gc: X.GC := NIL;
                  op       := NullPaintOp;
                END;

  TextGCRec = RECORD
                gc     : X.GC              := NIL;
                op                         := NullPaintOp;
                clipped: BOOLEAN           := FALSE;
                fnt    : PaintPrivate.Font := 0;
                mode                       := FIRST(XMode);
              END;

  FillGCRec = RECORD
                gc   : X.GC                := NIL;
                op                         := NullPaintOp;
                pm   : PaintPrivate.Pixmap := 0;
                delta                      := Point.Origin;
                wind                       := FIRST(VBT.WindingCondition);
              END;

  StrokeGCRec = RECORD
                  gc   : X.GC                := NIL;
                  op                         := NullPaintOp;
                  pm   : PaintPrivate.Pixmap := 0;
                  delta                      := Point.Origin;
                  end                        := FIRST(VBT.EndStyle);
                  join                       := FIRST(VBT.JoinStyle);
                  width: CARDINAL            := 0;
                END;

PROCEDURE ResolveTintGC (dpy: X.DisplayStar;
                         w  : X.Window;
                         st : XScreenType.T;
                         op : PaintPrivate.PaintOp): X.GC
  RAISES {TrestleComm.Failure} =
  VAR
    temp: TintGCRec;
    gcv : X.XGCValues;
    res : X.GC;
    mask: Ctypes.unsigned_long;
  BEGIN
    TRY
      WITH gca = st.tintGC DO
        FOR i := 0 TO LAST(gca) DO
          WITH gcr = gca[i] DO
            IF gcr.op = op THEN
              res := gcr.gc;
              IF i # 0 THEN
                temp := gcr;
                SUBARRAY(gca, 1, i) := SUBARRAY(gca, 0, i);
                gca[0] := temp
              END;
              RETURN res
            END
          END
        END;
        res := gca[LAST(gca)].gc;
        SUBARRAY(gca, 1, LAST(gca)) := SUBARRAY(gca, 0, LAST(gca));
        IF (op >= 0) AND (st.optable # NIL) AND (op < NUMBER(st.optable^)) THEN
          mask := X.GCFunction + X.GCPlaneMask + X.GCForeground;
          WITH tbl = st.optable[op] DO
            gcv.function := tbl.function;
            gcv.plane_mask := tbl.plane_mask;
            gcv.foreground := tbl.foreground
          END
        ELSE
          mask := X.GCFunction;
          gcv.function := X.GXnoop
        END;
        IF res = NIL THEN
          res := X.XCreateGC(dpy, w, mask, ADR(gcv))
        ELSE
          X.XChangeGC(dpy, res, mask, ADR(gcv))
        END;
        WITH gcr = gca[0] DO gcr.op := op; gcr.gc := res END;
        RETURN res
      END
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END ResolveTintGC;

PROCEDURE ResolveTextureGC (         dpy: X.DisplayStar;
                                     w  : X.Window;
                                     st : XScreenType.T;
                                     op : PaintPrivate.PaintOp;
                                     pm : PaintPrivate.Pixmap;
                            READONLY del: Point.T               ): X.GC
  RAISES {TrestleComm.Failure} =
  VAR
    temp : TextureGCRec;
    gcv  : X.XGCValues;
    res  : X.GC;
    mask : Ctypes.unsigned_long;
    pst  : XScreenType.T;
    apm                         := pm;
    delta                       := del;
    pmb  : Rect.T;
  BEGIN
    TRY
      IF pm = XScrnTpRep.SolidPixmap THEN
        RETURN ResolveTintGC(dpy, w, st, op)
      END;
      IF pm < 0 THEN
        pm := XScrnTpRep.SolidPixmap - pm;
        pst := st.bits
      ELSE
        pst := st
      END;
      IF delta # Point.Origin THEN
        pmb := XScrnPxmp.PixmapDomain(st, apm);
        IF NOT Rect.IsEmpty(pmb) THEN delta := Rect.Mod(delta, pmb) END
      END;
      WITH gca = st.textureGC DO
        FOR i := 0 TO LAST(gca) DO
          WITH gcr = gca[i] DO
            IF (gcr.op = op) AND (gcr.pm = apm) AND (gcr.delta = delta) THEN
              res := gcr.gc;
              IF i # 0 THEN
                temp := gcr;
                SUBARRAY(gca, 1, i) := SUBARRAY(gca, 0, i);
                gca[0] := temp
              END;
              RETURN res
            END
          END
        END;
        res := gca[LAST(gca)].gc;
        SUBARRAY(gca, 1, LAST(gca)) := SUBARRAY(gca, 0, LAST(gca));
        IF (op >= 0) AND (st.optable # NIL) AND (op < NUMBER(st.optable^))
             AND (pst.pmtable # NIL) AND (pm < NUMBER(pst.pmtable^)) THEN
          XPaint.ForceCapturePM(pst, dpy, pm);
          mask :=
            X.GCFunction + X.GCPlaneMask + X.GCForeground + X.GCBackground
              + X.GCFillStyle + X.GCTileStipXOrigin + X.GCTileStipYOrigin;
          WITH tbl = st.optable[op] DO
            gcv.function := tbl.function;
            gcv.plane_mask := tbl.plane_mask;
            gcv.foreground := tbl.foreground;
            gcv.background := tbl.background;
            gcv.fill_style := tbl.fill_style;
            IF tbl.fill_style = X.FillSolid THEN
              RETURN ResolveTintGC(dpy, w, st, op)
            ELSIF tbl.fill_style = X.FillTiled THEN
              INC(mask, X.GCTile);
              gcv.tile := pst.pmtable[pm].pixmap
            ELSE
              INC(mask, X.GCStipple);
              gcv.stipple := pst.pmtable[pm].pixmap;
              IF st.empty = apm THEN
                DEC(mask, X.GCStipple);
                gcv.fill_style := X.FillSolid;
                IF tbl.fill_style = X.FillStippled THEN
                  gcv.function := X.GXnoop
                ELSIF tbl.fill_style = X.FillOpaqueStippled THEN
                  gcv.foreground := tbl.background
                END
              END
            END;
            gcv.ts_x_origin := delta.h + pst.pmtable[pm].domain.west;
            gcv.ts_y_origin := delta.v + pst.pmtable[pm].domain.north
          END;
        ELSE
          mask := X.GCFunction;
          gcv.function := X.GXnoop
        END;
        IF res = NIL THEN
          res := X.XCreateGC(dpy, w, mask, ADR(gcv))
        ELSE
          X.XChangeGC(dpy, res, mask, ADR(gcv))
        END;
        WITH gcr = gca[0] DO
          gcr.op := op;
          gcr.pm := apm;
          gcr.delta := delta;
          gcr.gc := res
        END;
        RETURN res
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END ResolveTextureGC;

PROCEDURE ResolveFillGC (         dpy : X.DisplayStar;
                                  w   : X.Window;
                                  st  : XScreenType.T;
                                  op  : PaintPrivate.PaintOp;
                                  pm  : PaintPrivate.Pixmap;
                         READONLY del : Point.T;
                                  wind: VBT.WindingCondition  ): X.GC
  RAISES {TrestleComm.Failure} =
  VAR
    temp : FillGCRec;
    gcv  : X.XGCValues;
    res  : X.GC;
    mask : Ctypes.unsigned_long;
    pst                         := st;
    apm                         := pm;
    delta                       := del;
    pmb  : Rect.T;
  BEGIN
    TRY
      IF pm < 0 THEN pm := XScrnTpRep.SolidPixmap - pm; pst := st.bits END;
      IF delta # Point.Origin THEN
        pmb := XScrnPxmp.PixmapDomain(st, apm);
        IF NOT Rect.IsEmpty(pmb) THEN delta := Rect.Mod(delta, pmb) END
      END;
      WITH gca = st.fillGC DO
        FOR i := 0 TO LAST(gca) DO
          WITH gcr = gca[i] DO
            IF (gcr.op = op) AND (gcr.pm = apm) AND (gcr.delta = delta)
                 AND (gcr.wind = wind) THEN
              res := gcr.gc;
              IF i # 0 THEN
                temp := gcr;
                SUBARRAY(gca, 1, i) := SUBARRAY(gca, 0, i);
                gca[0] := temp
              END;
              RETURN res
            END
          END
        END;
        res := gca[LAST(gca)].gc;
        SUBARRAY(gca, 1, LAST(gca)) := SUBARRAY(gca, 0, LAST(gca));
        IF (op >= 0) AND (st.optable # NIL) AND (op < NUMBER(st.optable^))
             AND (pst.pmtable # NIL) AND (pm < NUMBER(pst.pmtable^)) THEN
          XPaint.ForceCapturePM(pst, dpy, pm);
          mask := X.GCFunction + X.GCPlaneMask + X.GCForeground
                    + X.GCBackground + X.GCFillStyle + X.GCTileStipXOrigin
                    + X.GCTileStipYOrigin + X.GCFillRule;
          WITH tbl = st.optable[op] DO
            gcv.function := tbl.function;
            gcv.plane_mask := tbl.plane_mask;
            gcv.foreground := tbl.foreground;
            gcv.background := tbl.background;
            gcv.fill_style := tbl.fill_style;
            IF tbl.fill_style = X.FillTiled THEN
              INC(mask, X.GCTile);
              gcv.tile := pst.pmtable[pm].pixmap
            ELSE
              INC(mask, X.GCStipple);
              gcv.stipple := pst.pmtable[pm].pixmap;
              IF apm = XScrnTpRep.SolidPixmap THEN
                DEC(mask, X.GCStipple);
                gcv.fill_style := X.FillSolid
              ELSIF apm = st.empty THEN
                DEC(mask, X.GCStipple);
                gcv.fill_style := X.FillSolid;
                IF tbl.fill_style = X.FillStippled THEN
                  gcv.function := X.GXnoop
                ELSIF tbl.fill_style = X.FillOpaqueStippled THEN
                  gcv.foreground := tbl.background
                END
              END
            END;
            gcv.ts_x_origin := delta.h + pst.pmtable[pm].domain.west;
            gcv.ts_y_origin := delta.v + pst.pmtable[pm].domain.north
          END
        ELSE
          mask := X.GCFunction;
          gcv.function := X.GXnoop
        END;
        gcv.fill_rule := FillStyle[wind];
        IF res = NIL THEN
          res := X.XCreateGC(dpy, w, mask, ADR(gcv))
        ELSE
          X.XChangeGC(dpy, res, mask, ADR(gcv))
        END;
        WITH gcr = gca[0] DO
          gcr.op := op;
          gcr.pm := apm;
          gcr.delta := delta;
          gcr.gc := res;
          gcr.wind := wind
        END;
        RETURN res
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END ResolveFillGC;

CONST
  FillStyle = ARRAY VBT.WindingCondition OF
                INTEGER{X.EvenOddRule, X.WindingRule};
  EndStyle = ARRAY VBT.EndStyle OF
               INTEGER{X.CapRound, X.CapNotLast, X.CapProjecting};
  JoinStyle = ARRAY VBT.JoinStyle OF
                INTEGER{X.JoinRound, X.JoinBevel, X.JoinMiter};

PROCEDURE ResolveStrokeGC (         dpy  : X.DisplayStar;
                                    w    : X.Window;
                                    st   : XScreenType.T;
                                    op   : PaintPrivate.PaintOp;
                                    pm   : PaintPrivate.Pixmap;
                           READONLY del  : Point.T;
                                    width: CARDINAL;
                                    end  : VBT.EndStyle;
                                    join : VBT.JoinStyle         ): X.GC
  RAISES {TrestleComm.Failure} =
  VAR
    temp : StrokeGCRec;
    gcv  : X.XGCValues;
    res  : X.GC;
    mask : Ctypes.unsigned_long;
    pst                         := st;
    apm                         := pm;
    delta                       := del;
    pmb  : Rect.T;
  BEGIN
    TRY
      IF pm < 0 THEN pm := XScrnTpRep.SolidPixmap - pm; pst := st.bits END;
      IF delta # Point.Origin THEN
        pmb := XScrnPxmp.PixmapDomain(st, apm);
        IF NOT Rect.IsEmpty(pmb) THEN delta := Rect.Mod(delta, pmb) END
      END;
      WITH gca = st.strokeGC DO
        FOR i := 0 TO LAST(gca) DO
          WITH gcr = gca[i] DO
            IF (gcr.op = op) AND (gcr.pm = apm) AND (gcr.delta = delta)
                 AND (gcr.width = width) AND (gcr.end = end)
                 AND (gcr.join = join) THEN
              res := gcr.gc;
              IF i # 0 THEN
                temp := gcr;
                SUBARRAY(gca, 1, i) := SUBARRAY(gca, 0, i);
                gca[0] := temp
              END;
              RETURN res
            END
          END
        END;
        res := gca[LAST(gca)].gc;
        SUBARRAY(gca, 1, LAST(gca)) := SUBARRAY(gca, 0, LAST(gca));
        IF (op >= 0) AND (st.optable # NIL) AND (op < NUMBER(st.optable^))
             AND (pst.pmtable # NIL) AND (pm < NUMBER(pst.pmtable^)) THEN
          XPaint.ForceCapturePM(pst, dpy, pm);
          mask :=
            X.GCFunction + X.GCPlaneMask + X.GCForeground + X.GCBackground
              + X.GCFillStyle + X.GCTileStipXOrigin + X.GCTileStipYOrigin
              + X.GCCapStyle + X.GCJoinStyle + X.GCLineWidth;
          WITH tbl = st.optable[op] DO
            gcv.function := tbl.function;
            gcv.plane_mask := tbl.plane_mask;
            gcv.foreground := tbl.foreground;
            gcv.background := tbl.background;
            gcv.fill_style := tbl.fill_style;
            IF tbl.fill_style = X.FillTiled THEN
              INC(mask, X.GCTile);
              gcv.tile := pst.pmtable[pm].pixmap
            ELSE
              INC(mask, X.GCStipple);
              gcv.stipple := pst.pmtable[pm].pixmap;
              IF apm = XScrnTpRep.SolidPixmap THEN
                DEC(mask, X.GCStipple);
                gcv.fill_style := X.FillSolid
              ELSIF apm = st.empty THEN
                DEC(mask, X.GCStipple);
                gcv.fill_style := X.FillSolid;
                IF tbl.fill_style = X.FillStippled THEN
                  gcv.function := X.GXnoop
                ELSIF tbl.fill_style = X.FillOpaqueStippled THEN
                  gcv.foreground := tbl.background
                END
              END
            END;
            gcv.ts_x_origin := delta.h + pst.pmtable[pm].domain.west;
            gcv.ts_y_origin := delta.v + pst.pmtable[pm].domain.north
          END
        ELSE
          mask := X.GCFunction;
          gcv.function := X.GXnoop
        END;
        gcv.line_width := width;
        gcv.cap_style := EndStyle[end];
        gcv.join_style := JoinStyle[join];
        IF res = NIL THEN
          res := X.XCreateGC(dpy, w, mask, ADR(gcv))
        ELSE
          X.XChangeGC(dpy, res, mask, ADR(gcv))
        END;
        WITH gcr = gca[0] DO
          gcr.op := op;
          gcr.pm := apm;
          gcr.delta := delta;
          gcr.gc := res;
          gcr.width := width;
          gcr.end := end;
          gcr.join := join
        END;
        RETURN res
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END ResolveStrokeGC;

PROCEDURE ResolvePixmapGC (            dpy  : X.DisplayStar;
                                       w    : X.Window;
                                       st   : XScreenType.T;
                                       op   : PaintPrivate.PaintOp;
                                       pm   : PaintPrivate.Pixmap;
                           READONLY    delta: Point.T;
                           VAR (*OUT*) mode : XMode;
                           VAR (*OUT*) src  : X.Pixmap              ): X.GC
  RAISES {TrestleComm.Failure} =
  VAR
    temp: PixmapGCRec;
    gcv : X.XGCValues;
    res : X.GC;
    mask: Ctypes.unsigned_long;
    pst : XScreenType.T;
    apm : PaintPrivate.Pixmap;
  BEGIN
    TRY
      IF pm = XScrnTpRep.SolidPixmap THEN
        res := ResolveTintGC(dpy, w, st, op);
        mode := XMode.UseFillRect;
        src := X.None;
        RETURN res
      END;
      apm := pm;
      IF pm < 0 THEN
        pm := XScrnTpRep.SolidPixmap - pm;
        pst := st.bits
      ELSE
        pst := st
      END;
      WITH gca = st.pixmapGC DO
        FOR i := 0 TO LAST(gca) DO
          WITH gcr = gca[i] DO
            IF (gcr.op = op)
                 AND ((gcr.mode # XMode.UseFillRect)
                        OR (gcr.pm = apm) AND (gcr.delta = delta)) THEN
              res := gcr.gc;
              mode := gcr.mode;
              IF apm = gcr.pm THEN
                src := gcr.src
              ELSIF mode = XMode.UseFillRect OR (pst.pmtable = NIL)
                      OR (pm >= NUMBER(pst.pmtable^)) THEN
                src := X.None
              ELSE
                src := pst.pmtable[pm].pixmap;
                gcr.src := src;
                gcr.pm := apm
              END;
              IF i # 0 THEN
                temp := gcr;
                SUBARRAY(gca, 1, i) := SUBARRAY(gca, 0, i);
                gca[0] := temp
              END;
              RETURN res
            END
          END
        END;
        res := gca[LAST(gca)].gc;
        SUBARRAY(gca, 1, LAST(gca)) := SUBARRAY(gca, 0, LAST(gca));
        IF (op >= 0) AND (st.optable # NIL) AND (op < NUMBER(st.optable^))
             AND (pst.pmtable # NIL) AND (pm < NUMBER(pst.pmtable^)) THEN
          mask :=
            X.GCFunction + X.GCPlaneMask + X.GCForeground + X.GCBackground;
          WITH tbl = st.optable[op] DO
            gcv.function := tbl.function;
            gcv.plane_mask := tbl.plane_mask;
            gcv.foreground := tbl.foreground;
            gcv.background := tbl.background;
            IF tbl.fill_style = X.FillSolid THEN
              res := ResolveTintGC(dpy, w, st, op);
              mode := XMode.UseFillRect;
              src := X.None;
              RETURN res
            ELSIF tbl.fill_style = X.FillStippled THEN
              INC(mask,
                  X.GCStipple + X.GCTileStipXOrigin + X.GCTileStipYOrigin);
              XPaint.ForceCapturePM(pst, dpy, pm);
              gcv.stipple := pst.pmtable[pm].pixmap;
              gcv.ts_x_origin := delta.h + pst.pmtable[pm].domain.west;
              gcv.ts_y_origin := delta.v + pst.pmtable[pm].domain.north;
              mode := XMode.UseFillRect;
              src := X.None
            ELSE
              src := pst.pmtable[pm].pixmap;
              IF tbl.fill_style = X.FillOpaqueStippled THEN
                mode := XMode.UseCopyPlane
              ELSE
                mode := XMode.UseCopyArea
              END
            END
          END
        ELSE
          mask := X.GCFunction;
          mode := XMode.UseFillRect;
          src := X.None;
          gcv.function := X.GXnoop
        END;
        IF res = NIL THEN
          INC(mask, X.GCGraphicsExposures + X.GCFillStyle);
          gcv.graphics_exposures := X.False;
          gcv.fill_style := X.FillStippled;
          res := X.XCreateGC(dpy, w, mask, ADR(gcv))
        ELSE
          X.XChangeGC(dpy, res, mask, ADR(gcv))
        END;
        WITH gcr = gca[0] DO
          gcr.op := op;
          gcr.pm := apm;
          gcr.delta := delta;
          gcr.mode := mode;
          gcr.src := src;
          gcr.gc := res
        END;
        RETURN res
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END ResolvePixmapGC;

PROCEDURE ResolveScrollGC (dpy: X.DisplayStar;
                           w  : X.Window;
                           st : XScreenType.T;
                           op : PaintPrivate.PaintOp): X.GC
  RAISES {TrestleComm.Failure} =
  VAR
    temp: ScrollGCRec;
    gcv : X.XGCValues;
    res : X.GC;
    mask: Ctypes.unsigned_long;
  BEGIN
    TRY
      WITH gca = st.scrollGC DO
        FOR i := 0 TO LAST(gca) DO
          WITH gcr = gca[i] DO
            IF gcr.op = op THEN
              res := gcr.gc;
              IF i # 0 THEN
                temp := gcr;
                SUBARRAY(gca, 1, i) := SUBARRAY(gca, 0, i);
                gca[0] := temp
              END;
              RETURN res
            END
          END
        END;
        res := gca[LAST(gca)].gc;
        SUBARRAY(gca, 1, LAST(gca)) := SUBARRAY(gca, 0, LAST(gca));
        IF (op >= 0) AND (st.optable # NIL) AND (op < NUMBER(st.optable^)) THEN
          mask := X.GCFunction + X.GCPlaneMask;
          WITH tbl = st.optable[op] DO
            gcv.function := tbl.function;
            gcv.plane_mask := tbl.plane_mask
          END
        ELSE
          mask := X.GCFunction;
          gcv.function := X.GXnoop
        END;
        IF res = NIL THEN
          res := X.XCreateGC(dpy, w, mask, ADR(gcv))
        ELSE
          X.XChangeGC(dpy, res, mask, ADR(gcv))
        END;
        WITH gcr = gca[0] DO gcr.op := op; gcr.gc := res END;
        RETURN res
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END ResolveScrollGC;

PROCEDURE ResolveTextGC (            dpy    : X.DisplayStar;
                                     w      : X.Window;
                                     st     : XScreenType.T;
                                     op     : PaintPrivate.PaintOp;
                                     clipped: BOOLEAN;
                                     fnt    : PaintPrivate.Font;
                         VAR (*OUT*) mode   : XMode                 ): X.GC
  RAISES {TrestleComm.Failure} =
  VAR
    temp      : TextGCRec;
    gcv       : X.XGCValues;
    res       : X.GC;
    mask      : Ctypes.unsigned_long;
    wasClipped: BOOLEAN;
  BEGIN
    TRY
      WITH gca = st.textGC DO
        FOR i := 0 TO LAST(gca) DO
          WITH gcr = gca[i] DO
            IF (gcr.op = op) AND (gcr.fnt = fnt)
                 AND (gcr.clipped = clipped) THEN
              res := gcr.gc;
              mode := gcr.mode;
              IF i # 0 THEN
                temp := gcr;
                SUBARRAY(gca, 1, i) := SUBARRAY(gca, 0, i);
                gca[0] := temp
              END;
              RETURN res
            END
          END
        END;
        res := gca[LAST(gca)].gc;
        wasClipped := gca[LAST(gca)].clipped;
        SUBARRAY(gca, 1, LAST(gca)) := SUBARRAY(gca, 0, LAST(gca));
        IF (op >= 0) AND (st.optable # NIL) AND (op < NUMBER(st.optable^)) THEN
          mask := X.GCFunction + X.GCPlaneMask + X.GCForeground + X.GCFont;
          WITH tbl = st.optable[op] DO
            gcv.function := tbl.function;
            gcv.plane_mask := tbl.plane_mask;
            gcv.foreground := tbl.foreground;
            gcv.font := fnt;
            IF tbl.fill_style = X.FillOpaqueStippled THEN
              INC(mask, X.GCBackground);
              gcv.background := tbl.background;
              mode := XMode.UseImageString
            ELSE
              mode := XMode.UseDrawString
            END
          END
        ELSE
          mask := X.GCFunction;
          mode := XMode.UseDrawString;
          gcv.function := X.GXnoop
        END;
        IF wasClipped AND NOT clipped THEN
          INC(mask, X.GCClipMask);
          gcv.clip_mask := X.None
        END;
        IF res = NIL THEN
          res := X.XCreateGC(dpy, w, mask, ADR(gcv))
        ELSE
          X.XChangeGC(dpy, res, mask, ADR(gcv))
        END;
        WITH gcr = gca[0] DO
          gcr.op := op;
          gcr.fnt := fnt;
          gcr.clipped := clipped;
          gcr.mode := mode;
          gcr.gc := res
        END;
        RETURN res
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END ResolveTextGC;

BEGIN
END XGC.
