(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 09:14:47 PST 1994 by kalsow    *)

MODULE SilBox;

IMPORT Point, Rect;
IMPORT WinDef, WinGDI;
IMPORT SilWindow, SilObject, SilPen, SilRd, SilWr;
  
REVEAL
  T = Tx BRANDED "SilBox" OBJECT
    width : INTEGER;
  OVERRIDES
    init     := Init;
    draw     := Draw;
    clone    := Clone;
    write    := Write;
    setWidth := SetWidth;
  END;

PROCEDURE Init (t: T;  READONLY p0, p1: Point.T;  width: INTEGER): T =
  BEGIN
    t.width := width;
    t.state := SilObject.Selected;
    t.next  := NIL;
    t.bbox  := Rect.Inset (Rect.FromCorners (p0, p1), -width);
    RETURN t;
  END Init;
  
PROCEDURE Write (t: T;  wr: SilWr.T) =
  VAR xx := 2 * t.width;
  BEGIN
    IF (t.bbox.south - t.bbox.north > xx)
      AND (t.bbox.east - t.bbox.west > xx)
      AND (t.state >= SilObject.Visible) THEN
      wr.putText ("r ");
      SilObject.WriteBBox (t, wr);
      wr.putInt (t.width);
      wr.endLine ();
    END;
  END Write;
  
PROCEDURE Read (rd: SilRd.T): REFANY =
  VAR t := NEW (T);
  BEGIN
    <*ASSERT rd.n_strs = 0 AND rd.n_ints = 5 *>
    SilObject.ReadBBox (t, rd);
    t.width := rd.ints[4];
    RETURN t;
  END Read;

PROCEDURE Clone (t: T): SilObject.T =
  BEGIN
    RETURN NEW (T, width := t.width);
  END Clone;
  
PROCEDURE Draw (t: T;  READONLY p: Point.T;  sel: BOOLEAN;  w: SilWindow.T) =
  VAR pen: WinDef.HGDIOBJ;
  BEGIN
    IF t.state >= SilObject.Visible THEN
      pen := SilPen.Push (t, sel, t.width, w);
      EVAL WinGDI.SelectObject (w.dc,
              WinGDI.GetStockObject (WinGDI.HOLLOW_BRUSH));
      EVAL WinGDI.Rectangle (w.dc, t.bbox.west  + t.width + p.h,
                                   t.bbox.north + t.width + p.v,
                                   t.bbox.east  - t.width + p.h,
                                   t.bbox.south - t.width + p.v);
      SilPen.Pop (pen, w);
    END;
  END Draw;
  
PROCEDURE SetWidth (t: T;  width: INTEGER;  sel: BOOLEAN): BOOLEAN =
  BEGIN
    IF (t.state = SilObject.Selected) OR sel THEN
      t.bbox := Rect.Inset (t.bbox, t.width - width);
      t.width := width;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END SetWidth;

BEGIN
  SilWindow.RegisterReader ('r', 'r', Read);
END SilBox.
