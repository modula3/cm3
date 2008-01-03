(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug  6 11:28:38 PDT 1996 by najork                   *)
(*       Created on Tue Jan 17 11:23:11 PST 1995 by najork                   *)


UNSAFE MODULE WinScreenType EXPORTS WinScreenType, WinScreenTypePrivate;

IMPORT Axis, Ctypes, PaintOp, Pixmap, Rect, TrestleImpl, VBTRep, WinDef, 
       WinGDI, WinScrnColorMap, WinScrnCursor, WinScrnFont, WinScrnPaintOp, 
       WinScrnPixmap, WinTrestle, WinUser;

REVEAL
  T = Private BRANDED OBJECT END;

(* New returns a new color screen type of a depth corresponding to the number 
   of planes supported by the current Windows desktop.
   It is supposed to fill in the following fields:
     depth, color, bg, fg, bits, font, cmap
   It also calls InnerNew to fill in the remaining fields. *)

PROCEDURE New(trsl: WinTrestle.T): T =
  VAR res := NEW(T);  n_colors := GetDeviceCaps (WinGDI.NUMCOLORS);
  BEGIN
    LOCK trsl DO
      res.trsl  := trsl;
      res.bg    := WinGDI.RGB (255, 255, 255); (* white *)
      res.fg    := WinGDI.RGB (0, 0, 0);       (* black *)
      (** res.depth := BITSIZE (WinDef.COLORREF);  **)
      res.depth := GetDeviceCaps(WinGDI.BITSPIXEL);  (* John Karnak 8/3/98 *)
      res.bits  := NewBits(trsl);
      res.color := (n_colors > 2) OR (n_colors < 0); (* Amazing Win32! *)
      res.cmap  := WinScrnColorMap.NewOracle();
      InnerNew (res);
    END;
    RETURN res;
  END New;


(* NewBits returns a new screen type of depth 1 (i.e. a bitmap screen type).
   It is supposed to fill in the following fields:
     depth, color, bg, fg, bits, font, cmap
   It also calls InnerNew to fill in the remaining fields.
   LL = trsl *)

PROCEDURE NewBits(trsl: WinTrestle.T): T =
  VAR
    res := NEW (T);
  BEGIN
    res.trsl  := trsl;
    res.bg    := 0;
    res.fg    := 1;
    res.depth := 1;
    res.bits  := res;
    res.cmap  := NIL;
    res.color := FALSE;
    InnerNew(res);
    RETURN res;
  END NewBits;


PROCEDURE InnerNew ((* IN-OUT *) res: T) =
  BEGIN
    WITH pix_hor = WinUser.GetSystemMetrics(WinUser.SM_CXSCREEN),
         pix_ver = WinUser.GetSystemMetrics(WinUser.SM_CYSCREEN),
         mm_hor  = GetDeviceCaps (WinGDI.HORZSIZE),
         mm_ver  = GetDeviceCaps (WinGDI.VERTSIZE) DO
      res.rootDom := Rect.FromSize(pix_hor, pix_ver);
      res.res[Axis.T.Hor] := FLOAT(pix_hor) / FLOAT(mm_hor);
      res.res[Axis.T.Ver] := FLOAT(pix_ver) / FLOAT(mm_ver);
    END;
    res.op := WinScrnPaintOp.NewOracle (res);
    res.cursor := WinScrnCursor.NewOracle (res);
    res.pixmap := WinScrnPixmap.NewOracle (res);
    res.ops := NIL;
    res.cursors := NIL;
    res.pixmaps := NIL;
    res.fonts := NIL;

    res.optable := NEW (REF ARRAY OF WinScrnPaintOp.OpRecord, 
                        NUMBER (PaintOp.Predefined));
    res.pmtable := NEW (REF ARRAY OF WinScrnPixmap.PixmapRecord, 
                        NUMBER (Pixmap.Predefined));
    res.font  := WinScrnFont.NewOracle ();
  END InnerNew;

PROCEDURE GetDeviceCaps (cap: Ctypes.int): INTEGER =
  VAR
    hwnd   := WinUser.GetDesktopWindow ();
    hdc    := WinUser.GetDC (hwnd);
    res    := WinGDI.GetDeviceCaps (hdc, cap);
    status := WinUser.ReleaseDC (hwnd, hdc);
  BEGIN
    <* ASSERT status # 0 *>
    RETURN res;
  END GetDeviceCaps;


BEGIN
END WinScreenType.
