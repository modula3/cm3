(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Mon Apr  3 15:24:56 PDT 1995 by msm      *)
(*      modified on Tue Jan 31 09:07:21 PST 1995 by kalsow   *)
(*      modified on Fri Mar 11 17:00:28 PST 1994 by gnelson  *)
(*      modified on Mon Nov 22 13:38:01 PST 1993 by steveg   *)
(*      modified on Mon Oct  4 14:29:53 PDT 1993 by sfreeman *)
(* modified on Fri May 7 17:34:54 PDT 1993 by mjordan *)
(* modified on Mon Feb 24 13:59:46 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE MODULE XPaint;

IMPORT Batch, BatchRep, BatchUtil, Completion, PaintExt, PaintPrivate,
       Path, PathPrivate, Point, PolyRegion, Rect, Region, Trapezoid,
       TrestleComm, VBT, VBTRep, Word, X, XClientF, XScreenType,
       XScrollQueue, TrestleOnX, XGC, TrestleClass, ScrnPixmap, Trestle,
       Ctypes, VBTClass, XScrnPxmp, XPicture, Xft, ScrnColorMap;

FROM PaintPrivate IMPORT CommandPtr;
FROM XScrnPxmp IMPORT PixmapDomain;

REVEAL
  T = TrestleOnX.Display BRANDED OBJECT
      OVERRIDES
        paintbatch    := PaintBatch;
        capture       := Capture;
        captureScreen := CaptureScreen
      END;

TYPE PC = PaintPrivate.PaintCommand;

CONST ComSize = ADRSIZE(PaintPrivate.CommandRec);

PROCEDURE PaintBatch (v: T; ch: VBT.T; ba: Batch.T) RAISES {} =
  VAR
    cmd : CommandPtr;
    ur  : XClientF.Child := ch.upRef;
    dpy : X.DisplayStar;
    w   : X.Window;
    pAdr: ADDRESS        := ADR(ba.b[0]);
    endP                 := ba.next;
    st  : XScreenType.T  := ch.st;
  BEGIN
    IF ba.clip.west >= ba.clip.east OR st = NIL THEN
      Batch.Free(ba);
      RETURN
    END;
    IF ba.clipped = BatchUtil.ClipState.Unclipped THEN
      BatchUtil.Clip(ba)
    END;
    TRY
      TrestleOnX.Enter(v);
      TRY
        dpy := v.dpy;
        w := ur.w;
        IF ur.captureOnWrite # NIL THEN
          ForceCapturePM(st, dpy, ur.captureOnWrite.id);
          ur.captureOnWrite := NIL
        END;
        WHILE pAdr < endP DO
          cmd := pAdr;
          CASE cmd.command OF
            PC.TintCom => pAdr := TintCom(cmd, pAdr, endP, dpy, w, st);
          | PC.TextureCom =>
              pAdr := TextureCom(cmd, pAdr, endP, dpy, w, st);
          | PC.PixmapCom => pAdr := PixmapCom(cmd, pAdr, endP, dpy, w, st);
          | PC.ScrollCom => pAdr := ScrollCom(cmd, pAdr, dpy, w, ur, st);
          | PC.TrapCom => pAdr := TrapCom(cmd, pAdr, endP, dpy, w, st);
          | PC.TextCom => pAdr := TextCom(cmd, pAdr, endP, dpy, w, st, ba);
          | PC.ExtensionCom =>
              pAdr := ExtensionCom(cmd, pAdr, endP, dpy, w, v, st);
          | PC.RepeatCom => INC(pAdr, ComSize)
          ELSE
            RETURN
          END
        END
      FINALLY
        Batch.Free(ba);
        TrestleOnX.Exit(v)
      END
    EXCEPT
      TrestleComm.Failure, X.Error =>     (* skip *)
    END
  END PaintBatch;

PROCEDURE TintCom (cmd       : CommandPtr;
                   pAdr, endP: ADDRESS;
                   dpy       : X.DisplayStar;
                   w         : X.Window;
                   st        : XScreenType.T  ): CommandPtr
  RAISES {TrestleComm.Failure} =
  VAR rpt: CommandPtr;
  BEGIN
    WITH op = LOOPHOLE(cmd, PaintPrivate.TintPtr),
         gc = XGC.ResolveTintGC(dpy, w, st, op.op) DO
      INC(pAdr, ADRSIZE(op^));
      FillRect(dpy, w, gc, op.clip);
      LOOP
        IF pAdr >= endP THEN EXIT END;
        rpt := pAdr;
        IF rpt.command # PC.RepeatCom THEN EXIT END;
        INC(pAdr, ComSize);
        FillRect(dpy, w, gc, rpt.clip)
      END
    END;
    RETURN pAdr;
  END TintCom;

PROCEDURE TextureCom (cmd       : CommandPtr;
                      pAdr, endP: ADDRESS;
                      dpy       : X.DisplayStar;
                      w         : X.Window;
                      st        : XScreenType.T  ): CommandPtr
  RAISES {TrestleComm.Failure} =
  VAR rpt: CommandPtr;
  BEGIN
    WITH op = LOOPHOLE(cmd, PaintPrivate.PixmapPtr),
         gc = XGC.ResolveTextureGC(dpy, w, st, op.op, op.pm, op.delta) DO
      INC(pAdr, ADRSIZE(op^));
      FillRect(dpy, w, gc, op.clip);
      LOOP
        IF pAdr >= endP THEN EXIT END;
        rpt := pAdr;
        IF rpt.command # PC.RepeatCom THEN EXIT END;
        INC(pAdr, ComSize);
        FillRect(dpy, w, gc, rpt.clip)
      END
    END;
    RETURN pAdr;
  END TextureCom;

PROCEDURE PixmapCom (cmd       : CommandPtr;
                     pAdr, endP: ADDRESS;
                     dpy       : X.DisplayStar;
                     w         : X.Window;
                     st        : XScreenType.T  ): CommandPtr
  RAISES {TrestleComm.Failure} =
  VAR
    rpt : CommandPtr;
    mode: XGC.XMode;
    src : X.Drawable;
  BEGIN
    WITH op = LOOPHOLE(cmd, PaintPrivate.PixmapPtr),
         gc = XGC.ResolvePixmapGC(
                dpy, w, st, op.op, op.pm, op.delta, mode, src) DO
      INC(pAdr, ADRSIZE(op^));
      IF mode = XGC.XMode.UseCopyPlane THEN
        VAR delta := op.delta;
        BEGIN
          IF NOT XScrnPxmp.IsLazy(st, op.pm) THEN
            delta :=
              Point.Add(delta, Rect.NorthWest(PixmapDomain(st, op.pm)))
          END;
          CopyPlane(dpy, src, w, gc, op.clip, delta);
          LOOP
            IF pAdr >= endP THEN EXIT END;
            rpt := pAdr;
            IF rpt.command # PC.RepeatCom THEN EXIT END;
            INC(pAdr, ComSize);
            CopyPlane(dpy, src, w, gc, rpt.clip, delta)
          END
        END
      ELSIF mode = XGC.XMode.UseCopyArea THEN
        VAR delta := op.delta;
        BEGIN
          IF NOT XScrnPxmp.IsLazy(st, op.pm) THEN
            delta :=
              Point.Add(delta, Rect.NorthWest(PixmapDomain(st, op.pm)))
          END;
          EVAL CopyArea(dpy, src, w, gc, op.clip, delta);
          LOOP
            IF pAdr >= endP THEN EXIT END;
            rpt := pAdr;
            IF rpt.command # PC.RepeatCom THEN EXIT END;
            INC(pAdr, ComSize);
            EVAL CopyArea(dpy, src, w, gc, rpt.clip, delta)
          END
        END
      ELSE
        WITH dom = Rect.Add(PixmapDomain(st, op.pm), op.delta) DO
          FillRect(dpy, w, gc, Rect.Meet(op.clip, dom));
          LOOP
            IF pAdr >= endP THEN EXIT END;
            rpt := pAdr;
            IF rpt.command # PC.RepeatCom THEN EXIT END;
            INC(pAdr, ComSize);
            FillRect(dpy, w, gc, Rect.Meet(rpt.clip, dom))
          END
        END
      END
    END;
    RETURN pAdr;
  END PixmapCom;

PROCEDURE ScrollCom (cmd : CommandPtr;
                     pAdr: ADDRESS;
                     dpy : X.DisplayStar;
                     w   : X.Window;
                     ur  : XClientF.Child;
                     st  : XScreenType.T   ): CommandPtr
  RAISES {TrestleComm.Failure} =
  BEGIN
    WITH op = LOOPHOLE(cmd, PaintPrivate.ScrollPtr),
         gc = XGC.ResolveScrollGC(dpy, w, st, op.op) DO
      INC(pAdr, ADRSIZE(op^));
      IF CopyArea(dpy, w, w, gc, op.clip, op.delta) THEN
        XScrollQueue.Insert(ur.scrollQ, op^);
        IF Region.OverlapRect(Rect.Sub(op.clip, op.delta), ur.badR)
             AND NOT Region.SubsetRect(op.clip, ur.badR) THEN
          ur.badR :=
            Region.Join(Region.MeetRect(
                          op.clip, Region.Add(ur.badR, op.delta)), ur.badR)
        END
      END
    END;
    RETURN pAdr;
  END ScrollCom;

PROCEDURE TrapCom (cmd       : CommandPtr;
                   pAdr, endP: ADDRESS;
                   dpy       : X.DisplayStar;
                   w         : X.Window;
                   st        : XScreenType.T  ): CommandPtr
  RAISES {TrestleComm.Failure} =
  VAR rpt: CommandPtr;
  BEGIN
    WITH op = LOOPHOLE(cmd, PaintPrivate.TrapPtr),
         gc = XGC.ResolveTextureGC(dpy, w, st, op.op, op.pm, op.delta) DO
      INC(pAdr, ADRSIZE(op^));
      IF op.m1.n < 0 THEN
        op.m1.n := -op.m1.n;
        op.m1.d := -op.m1.d
      ELSIF op.m1.n = 0 THEN
        RETURN pAdr;
      END;
      IF op.m2.n < 0 THEN
        op.m2.n := -op.m2.n;
        op.m2.d := -op.m2.d
      ELSIF op.m2.n = 0 THEN
        RETURN pAdr;
      END;
      Trap(dpy, w, gc, op, op.clip);
      LOOP
        IF pAdr >= endP THEN EXIT END;
        rpt := pAdr;
        IF rpt.command # PC.RepeatCom THEN EXIT END;
        INC(pAdr, ComSize);
        Trap(dpy, w, gc, op, rpt.clip)
      END
    END;
    RETURN pAdr;
  END TrapCom;

PROCEDURE TextCom (cmd       : CommandPtr;
                   pAdr, endP: ADDRESS;
                   dpy       : X.DisplayStar;
                   w         : X.Window;
                   st        : XScreenType.T;
                   ba        : Batch.T        ): CommandPtr
  RAISES {TrestleComm.Failure} =
  VAR
    pr  : PolyRegion.T;
    rpt : CommandPtr;
    mode: XGC.XMode;
  BEGIN
    WITH op      = LOOPHOLE(cmd, PaintPrivate.TextPtr),
         (* xft font from the op. If its NIL use old X paint *)
         xftFont = LOOPHOLE(op.xftFnt,Xft.XftFontStar),
         clipped = PaintPrivate.Prop.Clipped IN op.props,
         gc = XGC.ResolveTextGC(dpy, w, st, op.op, clipped, op.fnt, mode),
         subbed = (mode = XGC.XMode.UseImageString)
                    AND PaintPrivate.Prop.FontSub IN op.props DO

      INC(pAdr, op.szOfRec * ADRSIZE(Word.T));
      IF op.byteOrder # PaintPrivate.HostByteOrder THEN
        BatchUtil.ByteSwap(ba)
      END;
      IF NOT clipped THEN
        IF op.clip.west < op.clip.east THEN
          IF subbed THEN FillRect(dpy, w, gc, op.clip) END;
          PaintString(dpy, st, xftFont, w, gc, op, mode)
        END
      ELSE
        pr := PolyRegion.Empty;
        PolyRegion.JoinRect(pr, op.clip);
        LOOP
          IF pAdr >= endP THEN EXIT END;
          rpt := pAdr;
          IF rpt.command # PC.RepeatCom THEN EXIT END;
          INC(pAdr, ComSize);
          IF PolyRegion.OverlapRect(pr, rpt.clip) THEN
            WITH rgn = PolyRegion.ToRegion(pr) DO
              IF NOT Region.IsEmpty(rgn) THEN
                SetClipRegion(dpy, gc, rgn);
                IF subbed THEN FillRect(dpy, w, gc, rgn.r) END;
                PaintString(dpy, st, xftFont, w, gc, op, mode)
              END
            END;
            pr := PolyRegion.Empty
          END;
          PolyRegion.JoinRect(pr, rpt.clip)
        END;
        WITH rgn = PolyRegion.ToRegion(pr) DO
          IF NOT Region.IsEmpty(rgn) THEN
            SetClipRegion(dpy, gc, rgn);
            IF subbed THEN FillRect(dpy, w, gc, rgn.r) END;
            PaintString(dpy, st, xftFont, w, gc, op, mode)
          END
        END
      END
    END;
    RETURN pAdr;
  END TextCom;

PROCEDURE ExtensionCom (cmd       : CommandPtr;
                        pAdr, endP: ADDRESS;
                        dpy       : X.DisplayStar;
                        w         : X.Window;
                        v         : T;
                        st        : XScreenType.T  ): CommandPtr
  RAISES {TrestleComm.Failure} =
  <* FATAL Path.Malformed *>
  VAR
    rpt: CommandPtr;
    pr : PolyRegion.T;
  BEGIN
    TRY
      WITH op = LOOPHOLE(cmd, PaintPrivate.ExtensionPtr) DO
        INC(pAdr, op.szOfRec * ADRSIZE(Word.T));
        CASE op.subCommand OF
        | PaintExt.FillCommand, PaintExt.StrokeCommand,
            PaintExt.LineCommand =>
            VAR
              fillP   := LOOPHOLE(op, PaintExt.FillPtr);
              strokeP := LOOPHOLE(op, PaintExt.StrokePtr);
              lineP   := LOOPHOLE(op, PaintExt.LinePtr);
              pathP: PaintExt.PathPtr;
              path : Path.T;
              gc   : X.GC;
            BEGIN
              IF op.subCommand = PaintExt.LineCommand THEN
                gc := XGC.ResolveStrokeGC(
                        dpy, w, st, op.op, op.pm,
                        Point.Add(op.delta, lineP.delta), lineP.width,
                        lineP.end, VBT.JoinStyle.Round);
                IF op.delta # Point.Origin THEN
                  lineP.p := Point.Add(lineP.p, op.delta);
                  lineP.q := Point.Add(lineP.q, op.delta)
                END
              ELSE
                IF op.subCommand = PaintExt.FillCommand THEN
                  pathP := ADR(fillP.path);
                  gc := XGC.ResolveFillGC(
                          dpy, w, st, op.op, op.pm,
                          Point.Add(op.delta, fillP.delta), fillP.wind)
                ELSIF op.subCommand = PaintExt.StrokeCommand THEN
                  pathP := ADR(strokeP.path);
                  gc := XGC.ResolveStrokeGC(
                          dpy, w, st, op.op, op.pm,
                          Point.Add(op.delta, strokeP.delta),
                          strokeP.width, strokeP.end, strokeP.join)
                END;
                path := NEW(Path.T);
                path.curveCount := pathP.curveCount;
                path.start := pathP + ADRSIZE(pathP^);
                path.next := pAdr;
                path.end := pAdr;
                path.current := pAdr;
                IF op.delta # Point.Origin THEN
                  path := Path.Translate(path, op.delta);
                END;
                IF path.curveCount # 0 THEN path := Path.Flatten(path) END
              END;
              pr := PolyRegion.Empty;
              PolyRegion.JoinRect(pr, op.clip);
              LOOP
                IF pAdr >= endP THEN EXIT END;
                rpt := pAdr;
                IF rpt.command # PC.RepeatCom THEN EXIT END;
                INC(pAdr, ComSize);
                IF PolyRegion.OverlapRect(pr, rpt.clip) THEN
                  WITH rgn = PolyRegion.ToRegion(pr) DO
                    IF NOT Region.IsEmpty(rgn) THEN
                      SetClipRegion(dpy, gc, rgn);
                      IF op.subCommand = PaintExt.LineCommand THEN
                        X.XDrawLine(dpy, w, gc, lineP.p.h, lineP.p.v,
                                    lineP.q.h, lineP.q.v)
                      ELSIF op.subCommand = PaintExt.FillCommand THEN
                        FillPath(v, dpy, w, gc, path)
                      ELSE
                        StrokePath(v, dpy, w, gc, path)
                      END
                    END
                  END;
                  pr := PolyRegion.Empty
                END;
                PolyRegion.JoinRect(pr, rpt.clip)
              END;
              WITH rgn = PolyRegion.ToRegion(pr) DO
                IF NOT Region.IsEmpty(rgn) THEN
                  SetClipRegion(dpy, gc, rgn);
                  IF op.subCommand = PaintExt.LineCommand THEN
                    X.XDrawLine(dpy, w, gc, lineP.p.h, lineP.p.v,
                                lineP.q.h, lineP.q.v)
                  ELSIF op.subCommand = PaintExt.FillCommand THEN
                    FillPath(v, dpy, w, gc, path)
                  ELSE
                    StrokePath(v, dpy, w, gc, path)
                  END
                END
              END
            END;
        | PaintExt.PictureCommand =>
            VAR
              pictureP := LOOPHOLE(cmd, PaintExt.PicturePtr);
              delta    := pictureP.ext.delta;
              picture := LOOPHOLE(pictureP.picture, XPicture.T); (* see
                                                                    PaintExt.i3 *)
              completion := LOOPHOLE(pictureP.completion, Completion.T);
            BEGIN
              picture.put(
                dpy, w, st.imageGC, pictureP.ext.clip, delta, completion);
              LOOP
                IF pAdr >= endP THEN EXIT END;
                rpt := pAdr;
                IF rpt.command # PC.RepeatCom THEN EXIT END;
                INC(pAdr, ComSize);
                picture.put(
                  dpy, w, st.imageGC, rpt.clip, delta, completion);
              END;
            END;
        ELSE
          LOOP
            IF pAdr >= endP THEN EXIT END;
            rpt := pAdr;
            IF rpt.command # PC.RepeatCom THEN EXIT END;
            INC(pAdr, ComSize);
          END;
        END;
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
    RETURN pAdr;
  END ExtensionCom;

<*INLINE*> PROCEDURE Div (n: INTEGER; d: CARDINAL): INTEGER =
  BEGIN
    RETURN n DIV d
  END Div;

<*INLINE*> PROCEDURE Mod (n: INTEGER; d: CARDINAL): INTEGER =
  BEGIN
    RETURN n MOD d
  END Mod;

(* Steve: M2+E requires these versions of Div and Mod:

   PROCEDURE Div(n: INTEGER; d: CARDINAL): INTEGER; BEGIN IF n >= 0 THEN
   RETURN n DIV d ELSE RETURN -1 - (-n - 1) DIV d END END Div;

   PROCEDURE Mod(n: INTEGER; d: CARDINAL): INTEGER; BEGIN IF n >= 0 THEN
   RETURN n MOD d ELSE RETURN d - 1 - (-n - 1) MOD d END END Mod; *)

PROCEDURE HW (READONLY m: Trapezoid.Rational;
              READONLY p: Point.T;
                       v: INTEGER             ): INTEGER =
  (* Return ceiling of the h-coordinate of the intersection of the
     trapezoid edge determined by (m, p) with the horizontal line at height
     v. *)
  BEGIN
    RETURN p.h + Div(m.d * (v - p.v) + m.n - 1, m.n)
  END HW;

PROCEDURE HF (READONLY m: Trapezoid.Rational;
              READONLY p: Point.T;
                       v: INTEGER             ): INTEGER =
  (* Return fractional part of (ceiling - actual) of intersection above *)
  BEGIN
    RETURN Mod(-m.d * (v - p.v), m.n)
  END HF;

<* INLINE *> PROCEDURE FillRect (         dpy: X.DisplayStar;
                                          d  : X.Drawable;
                                          gc : X.GC;
                                 READONLY r  : Rect.T         )
  RAISES {TrestleComm.Failure} =
  BEGIN
    TRY
    IF r.west < r.east THEN
      X.XFillRectangle(
        dpy, d, gc, r.west, r.north, r.east - r.west, r.south - r.north)
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END FillRect;

<* INLINE *> PROCEDURE CopyPlane (         dpy   : X.DisplayStar;
                                           src, w: X.Drawable;
                                           gc    : X.GC;
                                  READONLY clip  : Rect.T;
                                  READONLY delta : Point.T        )
  RAISES {TrestleComm.Failure} =
  BEGIN
    TRY
    IF clip.west < clip.east THEN
      X.XCopyPlane(dpy, src, w, gc, clip.west - delta.h,
                   clip.north - delta.v, clip.east - clip.west,
                   clip.south - clip.north, clip.west, clip.north, 1)
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END CopyPlane;

<* INLINE *> PROCEDURE CopyArea (         dpy   : X.DisplayStar;
                                          src, w: X.Drawable;
                                          gc    : X.GC;
                                 READONLY clip  : Rect.T;
                                 READONLY delta : Point.T        ): BOOLEAN
  RAISES {TrestleComm.Failure} =
  BEGIN
    TRY
    IF clip.west < clip.east THEN
      X.XCopyArea(dpy, src, w, gc, clip.west - delta.h,
                  clip.north - delta.v, clip.east - clip.west,
                  clip.south - clip.north, clip.west, clip.north);
      RETURN TRUE
    ELSE
      RETURN FALSE
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END CopyArea;

TYPE XRectList = UNTRACED REF ARRAY OF X.XRectangle;

PROCEDURE SetClipRegion (dpy: X.DisplayStar; gc: X.GC; rgn: Region.T)
  RAISES {TrestleComm.Failure} =
  VAR
    rect : X.XRectangle;
    rectl: XRectList;
    rl   : REF ARRAY OF Rect.T;
  BEGIN
    TRY
      IF rgn.p = NIL THEN
        rect.x := rgn.r.west;
        rect.y := rgn.r.north;
        rect.width := rgn.r.east - rgn.r.west;
        rect.height := rgn.r.south - rgn.r.north;
        X.XSetClipRectangles(dpy, gc, 0, 0, ADR(rect), 1, X.YXBanded)
      ELSE
        rl := Region.ToRects(rgn);
        rectl := NEW(XRectList, NUMBER(rl^));
        FOR i := 0 TO LAST(rl^) DO
          WITH rect = rectl[i],
               r    = rl[i]     DO
            rect.x := r.west;
            rect.y := r.north;
            rect.width := r.east - r.west;
            rect.height := r.south - r.north
          END
        END;
        TRY
          X.XSetClipRectangles(
            dpy, gc, 0, 0, ADR(rectl[0]), NUMBER(rectl^), X.YXBanded)
        FINALLY
          DISPOSE(rectl)
        END
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END SetClipRegion;

PROCEDURE DrawXftText(dpy  : X.DisplayStar;
                      st   : XScreenType.T;
                      d    : X.Drawable;
                      op   : PaintPrivate.TextPtr;
                      font : Xft.XftFontStar;
                      n    : INTEGER;
                      VAR a: ARRAY OF X.XTextItem;
                      )   RAISES {TrestleComm.Failure} =
  VAR
    screen : INTEGER;
    visual : X.VisualStar;
    colmap : X.Colormap;
    draw : Xft.DrawStar;
    renderColor : Xft.RenderColorStar := NEW(Xft.RenderColorStar);
    xftColor : Xft.XftColorStar := NEW(Xft.XftColorStar);
    ent : ARRAY[0..0] OF ScrnColorMap.Entry;
    res : BOOLEAN;
  BEGIN
    screen := st.screenID;
    visual := st.visual;
    ent[0].pix := st.optable[op.op].foreground;
    st.cmap.standard().read(ent);

    WITH e = ent[0].xrgb, r = renderColor DO
      r.red := e.red;
      r.blue := e.blue;
      r.green := e.green;
      r.alpha := e.alpha;
    END;

    res := Xft.ColorAllocValue (dpy, visual, colmap, renderColor, xftColor);
    IF NOT res THEN RAISE TrestleComm.Failure END;

    draw := Xft.DrawCreate(dpy, d, visual, colmap );
    IF draw = NIL THEN RAISE TrestleComm.Failure END;

    FOR i := 0 TO n - 1 DO
      Xft.DrawString8(draw, xftColor, font, op.refpt.h, op.refpt.v, a[i].chars, a[i].nchars);
    END;

    Xft.DrawDestroy(draw);
  END DrawXftText;

CONST
  ValidRect = Rect.T{west := -32768, east := 32768, north := -32768,
                     south := 32768};

PROCEDURE PaintString (dpy     : X.DisplayStar;
                       st      : XScreenType.T;
                       xftFont : Xft.XftFontStar;
                       d       : X.Drawable;
                       gc      : X.GC;
                       op      : PaintPrivate.TextPtr;
                       mode: XGC.XMode             )
  RAISES {TrestleComm.Failure} =
  TYPE TextArray = UNTRACED REF ARRAY OF X.XTextItem;
  VAR
    xti  : ARRAY [0 .. 15] OF X.XTextItem;
    xtip : TextArray;
    txtsz                                 := op.txtsz;

  PROCEDURE PaintString2 (VAR a: ARRAY OF X.XTextItem)
    RAISES {TrestleComm.Failure} =
    VAR
      n             := 0;
      i             := 0;
      newi: INTEGER;
      dlp: UNTRACED REF VBT.Displacement := op + ADRSIZE(
                                                   PaintPrivate.TextRec);
      endp: UNTRACED REF VBT.Displacement := dlp
                                               + ADRSIZE(VBT.Displacement)
                                                   * op.dlsz;
      txtp           := LOOPHOLE(endp, Ctypes.char_star);
      delta: INTEGER;
    BEGIN
      TRY
      WHILE i < txtsz DO
        a[n].chars := txtp;
        a[n].font := X.None;
        delta := 0;
        WHILE (dlp # endp) AND (dlp.index = i) DO
          INC(delta, dlp.dh);
          dlp := dlp + ADRSIZE(VBT.Displacement)
        END;
        a[n].delta := delta;
        IF (dlp = endp) OR (dlp.index >= txtsz) THEN
          newi := txtsz
        ELSE
          newi := dlp.index
        END;
        a[n].nchars := newi - i;
        i := newi;
        INC(n)
      END;
      IF n # 0 THEN
        IF xftFont # NIL THEN
          DrawXftText(dpy, st, d, op, xftFont, n, a);
        ELSE
          X.XDrawText(dpy, d, gc, op.refpt.h, op.refpt.v, ADR(a[0]), n);
        END

      END;
      EXCEPT X.Error => RAISE TrestleComm.Failure END;
    END PaintString2;

  BEGIN
    TRY
    IF (txtsz = 0) OR NOT Rect.Member(op.refpt, ValidRect) THEN RETURN END;
    IF mode = XGC.XMode.UseImageString THEN
      IF op.dlsz = 0 THEN
        X.XDrawImageString(dpy, d, gc, op.refpt.h, op.refpt.v,
                           op + ADRSIZE(PaintPrivate.TextRec), txtsz)
      END
    ELSIF op.dlsz <= NUMBER(xti) THEN
      PaintString2(xti)
    ELSE
      xtip := NEW(TextArray, op.dlsz);
      TRY PaintString2(xtip^) FINALLY DISPOSE(xtip) END
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END PaintString;

TYPE
  StrokeMap = Path.MapObject OBJECT
                trsl: T;
                dpy : X.DisplayStar;
                d   : X.Drawable;
                gc  : X.GC;
                a   : Points;
                n   : CARDINAL        := 0
              OVERRIDES
                line  := StrokeLine;
                move  := StrokeMove;
                close := StrokeLine
              END;
  Points = UNTRACED REF ARRAY OF X.XPoint;

PROCEDURE StrokePath (v   : T;
                      dpy : X.DisplayStar;
                      d   : X.Drawable;
                      gc  : X.GC;
                      path: Path.T         ) RAISES {TrestleComm.Failure} =
  VAR
    sm := NEW(StrokeMap, trsl := v, dpy := dpy, d := d, gc := gc,
              a := NEW(Points, 50));
  <*FATAL Path.Malformed*>
  BEGIN
    Path.Map(path, sm);
    IF sm.n # 0 THEN EmitXStroke(sm) END;
    DISPOSE(sm.a);
    IF v.dead THEN RAISE TrestleComm.Failure END
  END StrokePath;

PROCEDURE StrokeMove (self: StrokeMap; READONLY p: Point.T) =
  BEGIN
    IF self.n # 0 THEN EmitXStroke(self) END;
    self.a[0].x := p.h;
    self.a[0].y := p.v;
    self.n := 1
  END StrokeMove;

PROCEDURE StrokeLine (                    self: StrokeMap;
                      <*UNUSED*> READONLY p   : Point.T;
                                 READONLY q   : Point.T    ) =
  VAR m := NUMBER(self.a^);
  BEGIN
    IF self.n = m THEN
      VAR newa := NEW(Points, 2 * m);
      BEGIN
        SUBARRAY(newa^, 0, m) := self.a^;
        DISPOSE(self.a);
        self.a := newa
      END
    END;
    self.a[self.n].x := q.h;
    self.a[self.n].y := q.v;
    INC(self.n)
  END StrokeLine;

PROCEDURE EmitXStroke (sm: StrokeMap) =
  BEGIN
    IF sm.n = 1 THEN sm.a[1] := sm.a[0]; sm.n := 2 END;
    IF NOT sm.trsl.dead THEN
      TRY
        X.XDrawLines(
          sm.dpy, sm.d, sm.gc, ADR(sm.a[0]), sm.n, X.CoordModeOrigin)
      EXCEPT
        X.Error =>   (* skip *)
      END
    END;
    sm.n := 0
  END EmitXStroke;

TYPE
  FillMap = Path.MapObject OBJECT
              trsl         : T;
              a            : Points;
              n            : CARDINAL := 0;
              origin, start: Point.T
            OVERRIDES
              line  := FillLine;
              move  := FillMove;
              close := FillLine
            END;

PROCEDURE FillPath (v   : T;
                    dpy : X.DisplayStar;
                    d   : X.Drawable;
                    gc  : X.GC;
                    path: Path.T         ) RAISES {TrestleComm.Failure} =
  VAR sm := NEW(FillMap, trsl := v, a := NEW(Points, 50));
  <*FATAL Path.Malformed*>
  BEGIN
    TRY
      TRY
        Path.Map(path, sm);
        IF sm.n # 0 THEN
          FillMove(sm, sm.start);
          IF v.dead THEN RAISE TrestleComm.Failure END;
          X.XFillPolygon(
            dpy, d, gc, ADR(sm.a[0]), sm.n, X.Complex, X.CoordModeOrigin)
        END
      FINALLY
        DISPOSE(sm.a)
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END FillPath;

PROCEDURE FillMove (self: FillMap; READONLY p: Point.T) =
  BEGIN
    IF self.n = 0 THEN
      self.origin := p
    ELSE
      FillLine(self, Point.Origin, self.start);
      FillLine(self, self.start, self.origin)
    END;
    FillLine(self, self.origin, p);
    self.start := p
  END FillMove;

PROCEDURE FillLine (                    self: FillMap;
                    <*UNUSED*> READONLY p   : Point.T;
                               READONLY q   : Point.T  ) =
  VAR m := NUMBER(self.a^);
  BEGIN
    IF self.n = m THEN
      VAR newa := NEW(Points, 2 * m);
      BEGIN
        SUBARRAY(newa^, 0, m) := self.a^;
        DISPOSE(self.a);
        self.a := newa
      END
    END;
    self.a[self.n].x := q.h;
    self.a[self.n].y := q.v;
    INC(self.n)
  END FillLine;

PROCEDURE Trap (         dpy : X.DisplayStar;
                         d   : X.Drawable;
                         gc  : X.GC;
                         tr  : PaintPrivate.TrapPtr;
                READONLY clip: Rect.T                )
  RAISES {TrestleComm.Failure} =
  VAR
    vlo, vhi, hw1, hw2, hf1, hf2, mw1, mw2, mf1, mf2, lft, rit: INTEGER;
    empty                                                     : BOOLEAN;
  BEGIN
    IF clip.west >= clip.east THEN RETURN END;
    vlo := clip.north;
    vhi := clip.south;
    IF (tr.m1.d = 0) AND (tr.m2.d = 0) THEN
      FillRect(dpy, d, gc,
               Rect.Meet(clip, Rect.FromEdges(tr.p1.h, tr.p2.h, vlo, vhi)));
      RETURN
    END;
    hw1 := HW(tr.m1, tr.p1, vlo);
    IF (hw1 >= clip.east) AND (HW(tr.m1, tr.p1, vhi - 1) >= clip.east) THEN
      RETURN
    END;
    hw2 := HW(tr.m2, tr.p2, vlo);
    IF (hw2 <= clip.west) AND (HW(tr.m2, tr.p2, vhi - 1) <= clip.west) THEN
      RETURN
    END;
    hf1 := HF(tr.m1, tr.p1, vlo);
    hf2 := HF(tr.m2, tr.p2, vlo);
    mw1 := Div(tr.m1.d, tr.m1.n);
    mf1 := Mod(tr.m1.d, tr.m1.n);
    mw2 := Div(tr.m2.d, tr.m2.n);
    mf2 := Mod(tr.m2.d, tr.m2.n);
    empty := TRUE;               (* set to false as soon as something is
                                    painted *)
    WHILE vlo # vhi DO
      lft := MAX(hw1, clip.west);
      rit := MIN(hw2, clip.east);
      IF lft < rit THEN
        FillRect(dpy, d, gc, Rect.FromEdges(lft, rit, vlo, vlo + 1));
        empty := FALSE
      ELSIF (lft > rit) AND NOT empty THEN
        (* Generated some painting and then found [lft ..  rit) empty by
           more than one pixel; hence all the remaining lines will be
           empty, hence: *)
        RETURN
      END;
      (* Advance to next scan line: *)
      INC(vlo);
      INC(hw1, mw1);
      DEC(hf1, mf1);
      IF hf1 < 0 THEN INC(hf1, tr.m1.n); INC(hw1) END;
      INC(hw2, mw2);
      DEC(hf2, mf2);
      IF hf2 < 0 THEN INC(hf2, tr.m2.n); INC(hw2) END
    END
  END Trap;

PROCEDURE Capture (            v   : T;
                               ch  : VBT.T;
                   READONLY    rect: Rect.T;
                   VAR (*out*) br  : Region.T): ScrnPixmap.T =
  VAR
    xpm: X.Pixmap;
    dpy: X.DisplayStar;
    w  : X.Window;
    ur : XClientF.Child := ch.upRef;
  BEGIN
    IF rect.west >= rect.east OR ch.st = NIL THEN
      br := Region.FromRect(rect);
      RETURN NIL
    END;
    TRY
      TrestleOnX.Enter(v);
      TRY
        dpy := v.dpy;
        w := ur.w;
        IF ur.captureOnWrite # NIL THEN
          ForceCapturePM(ch.st, dpy, ur.captureOnWrite.id);
          ur.captureOnWrite := NIL
        END;
        IF ur.xcage = X.None THEN
          ur.captureOnWrite := XScrnPxmp.FakeCapture(ch.st, w, rect,
                                                     ch.st.depth);
          br := ur.badR;
          RETURN ur.captureOnWrite
        ELSE
          xpm := CapturePM(v, ch.st, dpy, w, FALSE, rect, br);
          br := Region.Join(br, ur.badR);
          RETURN XScrnPxmp.FromXPixmap(ch.st, xpm, rect, ch.st.depth)
        END
      FINALLY
        TrestleOnX.Exit(v)
      END
    EXCEPT
      X.Error, TrestleComm.Failure =>
        br := Region.FromRect(rect);
        RETURN NIL
    END
  END Capture;

PROCEDURE CaptureOffscreenPM (         st  : XScreenType.T;
                                       dpy : X.DisplayStar;
                                       w   : X.Drawable;
                              READONLY rect: Rect.T;        ): X.Pixmap
  RAISES {X.Error} =
  VAR
    xpm := X.XCreatePixmap(dpy, w, rect.east - rect.west,
                           rect.south - rect.north, st.depth);
    gcv: X.XGCValues;
  BEGIN
    IF st.noExposeCaptureGC = NIL THEN
      gcv.graphics_exposures := X.False;
      st.noExposeCaptureGC :=
        X.XCreateGC(dpy, w, X.GCGraphicsExposures, ADR(gcv))
    END;
    X.XCopyArea(dpy, w, xpm, st.noExposeCaptureGC, rect.west, rect.north,
                rect.east - rect.west, rect.south - rect.north, 0, 0);
    RETURN xpm
  END CaptureOffscreenPM;

PROCEDURE ForceCapturePM (st : XScreenType.T;
                          dpy: X.DisplayStar;
                          pm : PaintPrivate.Pixmap)
  RAISES {X.Error} =
  BEGIN
    IF XScrnPxmp.IsLazy(st, pm) THEN
      XScrnPxmp.FinishCapture(
        st, pm, CaptureOffscreenPM(st, dpy, XScrnPxmp.GetDrawable(st, pm),
                                   PixmapDomain(st, pm)))
    END
  END ForceCapturePM;

PROCEDURE CapturePM (              v        : T;
                                   st       : XScreenType.T;
                                   dpy      : X.DisplayStar;
                                   w        : X.Drawable;
                                   offscreen: BOOLEAN;
                     READONLY      rect     : Rect.T;
                     VAR (* OUT *) br       : Region.T       ): X.Pixmap
  RAISES {X.Error, TrestleComm.Failure}
  <* LL.sup = v *> =
  VAR
    wf : XClientF.SimpleWaitFor;
    xpm: X.Pixmap;
  BEGIN
    br := Region.Empty;
    IF offscreen THEN
      xpm := CaptureOffscreenPM(st, dpy, w, rect)
    ELSE
     xpm := X.XCreatePixmap(dpy, w, rect.east - rect.west,
                           rect.south - rect.north, st.depth);
     IF st.captureGC = NIL THEN
        st.captureGC := X.XCreateGC(dpy, w, 0, NIL)
      END;
      wf := NEW(XClientF.SimpleWaitFor, d := xpm,
                reqno := X.XNextRequest(dpy));
      wf.types[0] := 0;
      wf.types[1] := X.GraphicsExpose;
      wf.types[2] := X.NoExpose;
      X.XCopyArea(dpy, w, xpm, st.captureGC, rect.west, rect.north,
                  rect.east - rect.west, rect.south - rect.north, 0, 0);
      LOOP
        WITH type = XClientF.Await(v, wf) DO
          IF type <= 1 THEN
            br := Region.FromRect(rect);
            RETURN X.None
          ELSIF type = X.NoExpose THEN
            EXIT
          ELSE                   (* type = GraphicsExpose *)
            WITH ev = LOOPHOLE(ADR(wf.ev), X.XGraphicsExposeEventStar) DO
              br :=
                Region.JoinRect(
                  XClientF.ToRect(ev.x, ev.y, ev.width, ev.height), br);
              IF ev.count = 0 THEN EXIT END
            END
          END
        END
      END
    END;
    RETURN xpm
  END CapturePM;

PROCEDURE CaptureScreen (         trsl: T;
                                  id  : Trestle.ScreenID;
                         READONLY clip: Rect.T;
                         VAR      br  : Region.T          ): ScrnPixmap.T
  RAISES {TrestleComm.Failure} =
  BEGIN
    TrestleOnX.Enter(trsl);
    TRY
      VAR
        st                := NARROW(trsl.screens[id], XScreenType.T);
        dpy               := trsl.dpy;
        w                 := st.root;
        rect              := Rect.Meet(clip, st.rootDom);
        xpm : X.Pixmap;
        gcv : X.XGCValues;
      BEGIN
        TRY
          br := Region.Difference(
                  Region.FromRect(clip), Region.FromRect(rect));
          IF rect.west >= rect.east THEN RETURN NIL END;
          xpm := X.XCreatePixmap(dpy, w, rect.east - rect.west,
                                 rect.south - rect.north, st.depth);
          IF st.noExposeCaptureGC = NIL THEN
            gcv.graphics_exposures := X.False;
            st.noExposeCaptureGC :=
              X.XCreateGC(dpy, w, X.GCGraphicsExposures, ADR(gcv))

          END;
          X.XCopyArea(
            dpy, w, xpm, st.noExposeCaptureGC, rect.west, rect.north,
            rect.east - rect.west, rect.south - rect.north, 0, 0);
          RETURN XScrnPxmp.FromXPixmap(st, xpm, rect, st.depth);
        EXCEPT
          X.Error => RAISE TrestleComm.Failure
        END;
      END
    FINALLY
      TrestleOnX.Exit(trsl)
    END
  END CaptureScreen;

BEGIN
END XPaint.
