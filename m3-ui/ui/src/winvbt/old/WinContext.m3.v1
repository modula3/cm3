(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Wed Jun 28 19:18:06 PDT 1995 by najork                   *)
(*       Created on Wed Feb 15 16:03:36 PST 1995 by najork                   *)

UNSAFE MODULE WinContext;

IMPORT Ctypes, PaintPrivate, Point, Rect, VBT, VBTRep, WinDef, WinGDI, 
       WinScrnPixmap, WinScreenType, WinScreenTypePrivate;

CONST 
  True = 1;

(* Alternatively, I could use "SaveDC" and "RestoreDC" to save and restore
   the device context. I'm not sure which is more efficient -- does "SaveDC"
   perform heap allocations every time? *)



(* PushTint is functioanlly equivalent to XGC.ResolveTintGC, except that 
   it does not deal with the "plane_mask". *)


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


(* Unresolved issues:
     o The origin of the pattern lies somewhere different than in the X world.
     o I don't think I handle complex PaintOps well
*)

PROCEDURE PushTexture (hdc  : WinDef.HDC;
                       st   : WinScreenType.T;
                       op   : PaintPrivate.PaintOp;
                       pm   : PaintPrivate.Pixmap;
                       delta: Point.T): T =
  VAR
    ctxt : T;
    pst  : WinScreenType.T;
    apm  : PaintPrivate.Pixmap := pm;
    color: WinDef.COLORREF;
    brush: WinDef.HBRUSH;
    oldOrg: WinDef.POINT;
    status: WinDef.BOOL;
  BEGIN
    ctxt.hdc := hdc;

    IF pm = WinScrnPixmap.SolidPixmap THEN
      RETURN PushTint (hdc, st, op);
    END;
    IF pm < 0 THEN
      pm := WinScrnPixmap.SolidPixmap - pm;
      pst := st.bits;
    ELSE
      pst := st;
    END;
    IF delta # Point.Origin THEN
      WITH pmb = WinScrnPixmap.PixmapDomain (st, apm) DO
        IF NOT Rect.IsEmpty (pmb) THEN
          delta := Rect.Mod (delta, pmb);
        END;
      END;
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
        <* ASSERT status = True *>

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
        <* ASSERT status # 0 *>
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

        IF apm = WinScrnPixmap.SolidPixmap THEN
          logbrush.lbStyle := WinGDI.BS_SOLID;  
          logbrush.lbColor := tbl.fop.col;
          logbrush.lbHatch := WinGDI.HS_HORIZONTAL; 
            (* ignored if style is solid *)
        ELSE
          logbrush.lbStyle := WinGDI.BS_PATTERN;
          logbrush.lbHatch := LOOPHOLE (pst.pmtable[pm].hbmp, WinDef.LONG);
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
      <* ASSERT bool = True *>
    END;

    (* If necessary, reset the brush and free up the brush handle *)
    IF ctxt.brush # NIL THEN
      brush := WinGDI.SelectObject (ctxt.hdc, ctxt.brush);
      <* ASSERT brush # NIL *>
      bool := WinGDI.DeleteObject (brush);
      <* ASSERT bool = True *>
    END;
  END Pop;

BEGIN
END WinContext.
