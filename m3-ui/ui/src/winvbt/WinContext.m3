(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Nov  4 14:10:43 PST 1996 by najork                   *)
(*       Created on Wed Feb 15 16:03:36 PST 1995 by najork                   *)

UNSAFE MODULE WinContext;

IMPORT Ctypes, OSWin32, PaintPrivate, Point, Rect, VBT, VBTRep, WinDef, WinGDI,
       WinScrnPixmap, WinScreenType, WinScreenTypePrivate;

CONST
  False = 0;

VAR
  Windows95 := OSWin32.Win95();

(* Alternatively, I could use "SaveDC" and "RestoreDC" to save and restore
   the device context. I'm not sure which is more efficient -- does "SaveDC"
   perform heap allocations every time? *)


PROCEDURE PushTint (hdc: WinDef.HDC;
                    st : WinScreenType.T;
                    op : PaintPrivate.PaintOp): T =
  VAR
    ctxt : T;
    brush: WinDef.HBRUSH;
  BEGIN
    ctxt.hdc := hdc;

    IF op >= 0 AND st.optable # NIL AND op < NUMBER (st.optable^) THEN
      WITH tbl = st.optable[op] DO
        ctxt.rop2 := WinGDI.SetROP2 (hdc, tbl.rop2);
        <* ASSERT ctxt.rop2 # 0 *>
        brush := WinGDI.CreateSolidBrush (tbl.fop.col);
        <* ASSERT brush # NIL *>
        ctxt.brush := WinGDI.SelectObject (hdc, brush);
        <* ASSERT ctxt.brush # NIL *>
      END;
    ELSE
      ctxt.rop2 := WinGDI.SetROP2 (hdc, WinGDI.R2_NOP);
      <* ASSERT ctxt.rop2 # 0 *>
    END;
    RETURN ctxt;
  END PushTint;


PROCEDURE PushTexture (hdc  : WinDef.HDC;
                       st   : WinScreenType.T;
                       op   : PaintPrivate.PaintOp;
                       pm   : PaintPrivate.Pixmap;
                       delta: Point.T): T =
  VAR
    ctxt  : T;
    pst   : WinScreenType.T;
    color : WinDef.COLORREF;
    brush : WinDef.HBRUSH;
    oldOrg: WinDef.POINT;
    status: WinDef.BOOL;
  BEGIN
    ctxt.hdc := hdc;

    IF pm = WinScrnPixmap.SolidPixmap THEN
      RETURN PushTint (hdc, st, op);
    END;

    IF delta # Point.Origin THEN
      WITH pmb = WinScrnPixmap.PixmapDomain (st, pm) DO
        IF NOT Rect.IsEmpty (pmb) THEN
          delta := Rect.Mod (delta, pmb);
        END;
      END;
    END;

    IF pm < 0 THEN
      pm := WinScrnPixmap.SolidPixmap - pm;
      pst := st.bits;
    ELSE
      pst := st;
    END;

    IF op >= 0 AND st.optable # NIL AND op < NUMBER(st.optable^) AND
       pst.pmtable # NIL AND pm < NUMBER (pst.pmtable^) THEN
      WITH tbl = st.optable[op] DO
        ctxt.rop2 := WinGDI.SetROP2 (hdc, tbl.rop2);
        <* ASSERT ctxt.rop2 # 0 *>

        brush := WinGDI.CreatePatternBrush (pst.pmtable[pm].hbmp);
        <* ASSERT brush # NIL *>
        status := WinGDI.SetBrushOrgEx(hdc,
                                       delta.h + pst.pmtable[pm].domain.west,
                                       delta.v + pst.pmtable[pm].domain.north,
                                       ADR(oldOrg));
        <* ASSERT status # False *>

        ctxt.brush := WinGDI.SelectObject (hdc, brush);
        <* ASSERT ctxt.brush # NIL *>

        (* In Windows, '0' pixels of the bitmap in the pattern brush are
           drawn in the current text color, so the text color should be
           "tbl.bop.col". '1' pixels are drawn in the current background color,
           so this color should be "tbl.fop.col". Counterintuive? Well, after
           all, this is Windows! *)
        color := WinGDI.SetTextColor (hdc, tbl.bop.col);
        <* ASSERT color # WinGDI.CLR_INVALID *>
        color := WinGDI.SetBkColor (hdc, tbl.fop.col);
        <* ASSERT color # WinGDI.CLR_INVALID *>
      END;
    ELSE
      ctxt.rop2 := WinGDI.SetROP2 (hdc, WinGDI.R2_NOP);
      <* ASSERT ctxt.rop2 # 0 *>
    END;
    RETURN ctxt;
  END PushTexture;


PROCEDURE PushPixmap (hdc  : WinDef.HDC;
                      st   : WinScreenType.T;
                      op   : PaintPrivate.PaintOp;
                      pm   : PaintPrivate.Pixmap;
                      delta: Point.T): T =
  VAR
    ctxt : T;
    pst  : WinScreenType.T;
    color: WinDef.COLORREF;
    brush: WinDef.HBRUSH;
    oldOrg: WinDef.POINT;
    status: WinDef.BOOL;
  BEGIN
    ctxt.hdc := hdc;

    (* If the pixmap is solid, then we can treat the texture as a tint. *)
    IF pm = WinScrnPixmap.SolidPixmap THEN
      RETURN PushTint (hdc, st, op);
    END;

    WITH dom = WinScrnPixmap.PixmapDomain (st, pm) DO
      IF NOT Rect.IsEmpty (dom) THEN
        delta := Point.T {(delta.h + dom.west) MOD (dom.east - dom.west),
                          (delta.v + dom.north) MOD (dom.south - dom.north)};
        IF st.depth = 24 THEN
          INC (delta.h);  (* John Karnak 21-Jul-1998 *)
        END;
      END;
    END;

    (* pm < 0 indicates that the pixmap does not belong to st,
       but to st.bits, the monochrome screentype associated with st. *)
    IF pm < 0 THEN
      pm := WinScrnPixmap.SolidPixmap - pm;
      pst := st.bits;
    ELSE
      pst := st;
    END;

    IF op >= 0 AND st.optable # NIL AND op < NUMBER(st.optable^) AND
       pst.pmtable # NIL AND pm < NUMBER (pst.pmtable^) AND
       pst.pmtable[pm].hbmp # NIL THEN
      WITH tbl = st.optable[op] DO
        ctxt.rop2 := WinGDI.SetROP2 (hdc, tbl.rop2);
        <* ASSERT ctxt.rop2 # 0 *>

        brush := WinGDI.CreatePatternBrush (pst.pmtable[pm].hbmp);
        <* ASSERT brush # NIL *>

        status := WinGDI.SetBrushOrgEx(hdc, delta.h, delta.v, ADR(oldOrg));
        <* ASSERT status # False *>

        ctxt.brush := WinGDI.SelectObject (hdc, brush);
        <* ASSERT ctxt.brush # NIL *>

        (* In Windows, '0' pixels of the bitmap in the pattern brush are
           drawn in the current text color, so the text color should be
           "tbl.bop.col". '1' pixels are drawn in the current background color,
           so this color should be "tbl.fop.col". Counterintuive? Well, after
           all, this is Windows! *)
        color := WinGDI.SetTextColor (hdc, tbl.bop.col);
        <* ASSERT color # WinGDI.CLR_INVALID *>
        color := WinGDI.SetBkColor (hdc, tbl.fop.col);
        <* ASSERT color # WinGDI.CLR_INVALID *>
      END;
    ELSE
      ctxt.rop2 := WinGDI.SetROP2 (hdc, WinGDI.R2_NOP);
      <* ASSERT ctxt.rop2 # 0 *>
    END;
    RETURN ctxt;
  END PushPixmap;


PROCEDURE PushFill (hdc  : WinDef.HDC;
                    st   : WinScreenType.T;
                    op   : PaintPrivate.PaintOp;
                    pm   : PaintPrivate.Pixmap;
                    delta: Point.T;
                    wind : VBT.WindingCondition): T =
  CONST
    FillStyle = ARRAY VBT.WindingCondition OF INTEGER {WinGDI.ALTERNATE,
                                                       WinGDI.WINDING};
  VAR
    ctxt  : T;
    pen   : WinDef.HPEN;
    brush : WinDef.HBRUSH;
    status: Ctypes.int;
    pst   : WinScreenType.T     := st;  (* pixmap screen type *)
    apm   : PaintPrivate.Pixmap := pm;  (* backup of original "pm" argument *)
    color : WinDef.COLORREF;
  BEGIN
    ctxt.hdc := hdc;

    IF pm < 0 THEN
      pm := WinScrnPixmap.SolidPixmap - pm;
      pst := st.bits;
    END;
    IF delta # Point.Origin THEN
      WITH pmb = WinScrnPixmap.PixmapDomain(st, apm) DO
        IF NOT Rect.IsEmpty(pmb) THEN
          delta := Rect.Mod (delta, pmb);
        END;
      END;
    END;
    (* Note: we could also have a "fast path" for empty pixmaps *)

    IF op >= 0 AND st.optable # NIL AND op < NUMBER(st.optable^)
       AND pst.pmtable # NIL AND pm < NUMBER (pst.pmtable^) THEN

      WITH tbl = st.optable[op] DO
        ctxt.rop2 := WinGDI.SetROP2 (hdc, tbl.rop2);
        <* ASSERT ctxt.rop2 # 0 *>

        status := WinGDI.SetPolyFillMode (hdc, FillStyle[wind]);
        <* ASSERT status # False *>
        (* Note: "Pop" does not try to reestablish the previous fill style.
           So, there is no need to save it in "ctxt". *)

        pen := WinGDI.GetStockObject (WinGDI.NULL_PEN);
        ctxt.pen := WinGDI.SelectObject (hdc, pen);
        <* ASSERT ctxt.pen # NIL *>

        IF apm = WinScrnPixmap.SolidPixmap THEN
          brush := WinGDI.CreateSolidBrush (tbl.fop.col);
          <* ASSERT brush # NIL *>
          ctxt.brush := WinGDI.SelectObject (hdc, brush);
          <* ASSERT ctxt.brush # NIL *>
        ELSE

          brush := WinGDI.CreatePatternBrush (pst.pmtable[pm].hbmp);
          <* ASSERT brush # NIL *>
          ctxt.brush := WinGDI.SelectObject (hdc, brush);
          <* ASSERT ctxt.brush # NIL *>

          (* In Windows, '0' pixels of the bitmap in the pattern brush are
             drawn in the current text color, so the text color should be
             "tbl.bop.col". '1' pixels are drawn in the current background
             color, so this color should be "tbl.fop.col". Counterintuive?
             Well, after all, this is Windows! *)
          color := WinGDI.SetTextColor (hdc, tbl.bop.col);
          <* ASSERT color # WinGDI.CLR_INVALID *>
          color := WinGDI.SetBkColor (hdc, tbl.fop.col);
          <* ASSERT color # WinGDI.CLR_INVALID *>
        END;
      END;
    ELSE
      ctxt.rop2 := WinGDI.SetROP2 (hdc, WinGDI.R2_NOP);
      <* ASSERT ctxt.rop2 # 0 *>
    END;
    RETURN ctxt;
  END PushFill;


PROCEDURE PushStroke (hdc  : WinDef.HDC;
                      st   : WinScreenType.T;
                      op   : PaintPrivate.PaintOp;
                      pm   : PaintPrivate.Pixmap;
                      delta: Point.T;
                      width: CARDINAL;
                      end  : VBT.EndStyle;
                      join : VBT.JoinStyle): T =
  CONST
    EndStyle = ARRAY VBT.EndStyle OF INTEGER {WinGDI.PS_ENDCAP_ROUND,
                                              WinGDI.PS_ENDCAP_FLAT,
                                              WinGDI.PS_ENDCAP_SQUARE};
    JoinStyle = ARRAY VBT.JoinStyle OF INTEGER {WinGDI.PS_JOIN_ROUND,
                                                WinGDI.PS_JOIN_BEVEL,
                                                WinGDI.PS_JOIN_MITER};
  VAR
    ctxt    : T;
    pen     : WinDef.HPEN;
    style   : WinDef.DWORD;
    logbrush: WinGDI.LOGBRUSH;
    pst     : WinScreenType.T     := st; (* pixmap screen type *)
    apm     : PaintPrivate.Pixmap := pm; (* backup of original "pm" argument *)
    color   : WinDef.COLORREF;
  BEGIN
    ctxt.hdc := hdc;

    IF pm < 0 THEN
      pm := WinScrnPixmap.SolidPixmap - pm;
      pst := st.bits;
    END;
    IF delta # Point.Origin THEN
      WITH pmb = WinScrnPixmap.PixmapDomain(st, apm) DO
        IF NOT Rect.IsEmpty(pmb) THEN
          delta := Rect.Mod (delta, pmb);
        END;
      END;
    END;

    (*
     * The main omission is that I don't deal with pixmaps!
     *
     * Refer back to XGC.ResolveStrokeGC to see just how much functionality
     * is missing!
     *)

    IF op >= 0 AND st.optable # NIL AND op < NUMBER (st.optable^)
       AND pst.pmtable # NIL AND pm < NUMBER(pst.pmtable^) THEN
      WITH tbl = st.optable[op] DO

        ctxt.rop2 := WinGDI.SetROP2 (hdc, tbl.rop2);
        <* ASSERT ctxt.rop2 # 0 *>

        style := WinGDI.PS_GEOMETRIC + WinGDI.PS_SOLID +
                     EndStyle[end] + JoinStyle[join];

        IF Windows95 THEN

          (* The "Quick Info" button of the "ExtCreatePen" page of the
             "Win32 SDK Help" online documentation says among other things:

             Platform Notes:   Windows 95: Only supports solid colors
                               (e.g. BS_SOLID brushes); ...

             The same note can be found in "msdev\lib\win32api.csv" and
             "mstools\lib\win32api.csv", two Excel spreadsheets that come on
             the MSVC 4 CD-ROM and the Win32 SDK CD-ROM, respectively.

             This information seems to be accurate: when I try to create a pen
             with a BS_PATTERN brush, the call to "ExtCreatePen" fails.

             Typical Microsoft: The main pages of "ExtCreatePen" and "LOGBRUSH"
             mention many other limitations of Windows 95, but not this
             particular one.

             The simplest workaround is to default the pattern to Pixmap.Solid
             when running on Windows 95. Obviously, this workaround does not
             quite live up to the Trestle specification. *)

          logbrush.lbStyle := WinGDI.BS_SOLID;
          logbrush.lbColor := tbl.fop.col;

        ELSIF apm = WinScrnPixmap.SolidPixmap THEN

          logbrush.lbStyle := WinGDI.BS_SOLID;
          logbrush.lbColor := tbl.fop.col;

        ELSE (* The pixmap is not solid, and we are running Windows NT *)

          logbrush.lbStyle := WinGDI.BS_PATTERN;
          logbrush.lbHatch := LOOPHOLE (pst.pmtable[pm].hbmp, INTEGER);
          (*
           * From the documentation, it is not clear how to select the
           * background and foreground colors for the bitmap.  An experiment
           * showed that (at least on NT 3.1) "SetTextColor" selected the
           * background color, and "SetBkColor" selected the foreground color.
           *)
          color := WinGDI.SetTextColor (hdc, tbl.bop.col);
          <* ASSERT color # WinGDI.CLR_INVALID *>
          color := WinGDI.SetBkColor (hdc, tbl.fop.col);
          <* ASSERT color # WinGDI.CLR_INVALID *>
        END;

        pen := WinGDI.ExtCreatePen (style, width, ADR(logbrush), 0, NIL);
        <* ASSERT pen # NIL *>
        ctxt.pen := WinGDI.SelectObject (hdc, pen);
        <* ASSERT ctxt.pen # NIL *>
      END;
    ELSE
      ctxt.rop2 := WinGDI.SetROP2 (hdc, WinGDI.R2_NOP);
      <* ASSERT ctxt.rop2 # 0 *>
    END;

    RETURN ctxt;
  END PushStroke;


PROCEDURE Pop (READONLY ctxt: T) =
  VAR
    rop2 : Ctypes.int;
    pen  : WinDef.HPEN;
    brush: WinDef.HBRUSH;
    bool : WinDef.BOOL;
  BEGIN
    (* If necessary, reset the binary raster operation mode. *)
    IF ctxt.rop2 # 0 THEN
      rop2 := WinGDI.SetROP2 (ctxt.hdc, ctxt.rop2);
      <* ASSERT rop2 # 0 *>
    END;

    (* If necessary, reset the pen and free up the pen handle *)
    IF ctxt.pen # NIL THEN
      pen := WinGDI.SelectObject (ctxt.hdc, ctxt.pen);
      <* ASSERT pen # NIL *>
      bool := WinGDI.DeleteObject (pen);
      <* ASSERT bool # False *>
    END;

    (* If necessary, reset the brush and free up the brush handle *)
    IF ctxt.brush # NIL THEN
      brush := WinGDI.SelectObject (ctxt.hdc, ctxt.brush);
      <* ASSERT brush # NIL *>
      bool := WinGDI.DeleteObject (brush);
      <* ASSERT bool # False *>
    END;
  END Pop;

BEGIN
END WinContext.
