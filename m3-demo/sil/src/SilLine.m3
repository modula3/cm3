(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 13:34:29 PST 1994 by kalsow    *)

MODULE SilLine;

IMPORT Point, Rect;
IMPORT WinDef, WinGDI;
IMPORT SilWindow, SilPen, SilObject, SilWr, SilRd;

REVEAL
  T = Tx BRANDED "SilLine" OBJECT
    a, b  : Point.T;
    width : INTEGER;
  OVERRIDES
    init     := Init;
    draw     := Draw;
    select   := Select;
    clone    := Clone;
    write    := Write;
    setWidth := SetWidth;
  END;

PROCEDURE Init (t: T;  READONLY a, b: Point.T;  width: INTEGER): T =
  VAR nw, se: Point.T;
  BEGIN
    nw.h := MIN (a.h, b.h) - width;  nw.v := MIN (a.v, b.v) - width;
    se.h := MAX (a.h, b.h) + width;  se.v := MAX (a.v, b.v) + width;
    t.a     := Point.Sub (a, nw);
    t.b     := Point.Sub (b, nw);
    t.width := width;
    t.next  := NIL;
    t.state := SilObject.Visible;
    t.bbox  := Rect.FromEdges (nw.h, se.h, nw.v, se.v);
    RETURN t;
  END Init;
  
PROCEDURE Clone (t: T): SilObject.T =
  BEGIN
    RETURN NEW (T, a := t.a, b := t.b, width := t.width);
  END Clone;
  
PROCEDURE Write (t: T;  wr: SilWr.T) =
  BEGIN
    IF (t.state >= SilObject.Visible) AND (t.a # t.b) THEN
      (* only write visible lines*)
      wr.putText ("l ");
      SilObject.WriteBBox (t, wr);
      wr.putInt (t.a.h);
      wr.putInt (t.a.v);
      wr.putInt (t.b.h);
      wr.putInt (t.b.v);
      wr.putInt (t.width);
      wr.endLine ();
    END;
  END Write;
  
PROCEDURE Read (rd: SilRd.T): REFANY =
  VAR t := NEW (T);
  BEGIN
    <*ASSERT rd.n_strs = 0 AND rd.n_ints = 9 *>
    SilObject.ReadBBox (t, rd);
    t.a.h   := rd.ints[4];
    t.a.v   := rd.ints[5];
    t.b.h   := rd.ints[6];
    t.b.v   := rd.ints[7];
    t.width := rd.ints[8];
    RETURN t;
  END Read;

PROCEDURE Draw (t: T;  READONLY p: Point.T;  sel: BOOLEAN;  w: SilWindow.T) =
  VAR pen: WinDef.HGDIOBJ;  h, v: INTEGER;
  BEGIN
    IF t.state >= SilObject.Visible THEN
      pen := SilPen.Push (t, sel, t.width, w);
      h := t.bbox.west + p.h; 
      v := t.bbox.north + p.v;
      EVAL WinGDI.MoveToEx (w.dc, h + t.a.h, v + t.a.v, NIL);
      EVAL WinGDI.LineTo (w.dc, h + t.b.h, v + t.b.v);
      SilPen.Pop (pen, w);
    END;
  END Draw;
  
PROCEDURE Select (t          : T;
         READONLY clip       : Rect.T; 
                  pointMode  : BOOLEAN;
                  dropOthers : BOOLEAN;
       VAR(*OUT*) bbox       : Rect.T): BOOLEAN = 
  (* this is NOT the standard selection method, since lines
     are selected by pointing at their endpoints*)
  VAR hit: BOOLEAN;  a, b: Point.T;
  BEGIN
    IF t.state >= SilObject.Visible THEN
      a.v := t.bbox.north + t.a.v;
      a.h := t.bbox.west  + t.a.h;
      b.v := t.bbox.north + t.b.v;
      b.h := t.bbox.west  + t.b.h;
      IF (NOT pointMode) THEN
        (*does the line lie within the bounding box?*)
        hit := (clip.west  <= a.h) AND (a.h <= clip.east) AND
               (clip.north <= a.v) AND (a.v <= clip.south) AND
               (clip.west  <= b.h) AND (b.h <= clip.east) AND
               (clip.north <= b.v) AND (b.v <= clip.south);
      ELSE
        (*is an endpoint within 6 units of the select point?*)
        hit := ((ABS(clip.north - a.v) < 6) AND (ABS(clip.west - a.h) < 6))
            OR ((ABS(clip.north - b.v) < 6) AND (ABS(clip.west - b.h) < 6));
      END;
  
      IF hit THEN
        bbox := t.bbox;
        t.state := SilObject.Selected;
        RETURN TRUE;
      ELSIF dropOthers AND (t.state = SilObject.Selected) THEN
        bbox := t.bbox;
        t.state := SilObject.Visible;
        RETURN TRUE;
      END;
    END;

    RETURN FALSE;
  END Select;
  
PROCEDURE SetWidth (t: T;  width: INTEGER;  sel: BOOLEAN): BOOLEAN =
  VAR delta: INTEGER;
  BEGIN
    IF (t.state = SilObject.Selected) OR sel THEN
      delta := t.width - width;
      t.bbox := Rect.Inset (t.bbox, delta);
      DEC (t.a.h, delta);  DEC (t.a.v, delta);
      DEC (t.b.h, delta);  DEC (t.b.v, delta);
      t.width := width;
      RETURN TRUE;
    END;
    RETURN FALSE;
  END SetWidth;

BEGIN
  SilWindow.RegisterReader ('l', 'l', Read);
END SilLine.
