(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 12:58:45 PST 1994 by kalsow    *)

UNSAFE MODULE SilString;

IMPORT Text, Point, Rect, M3toC;
IMPORT WinGDI, WinDef;
IMPORT SilObject, SilFont, SilWindow, SilRd, SilWr, SilError;
  
REVEAL
  T = Tx BRANDED "String" OBJECT
    font  : SilFont.T;
    body  : TEXT;
  OVERRIDES
    init     := Init;
    draw     := Draw;
    takeChar := TakeChar;
    clone    := Clone;
    write    := Write;
    setFont  := SetFont;
  END;
  
PROCEDURE Init (t: T;  READONLY p: Point.T;  body: TEXT;
                f: SilFont.T;  w: SilWindow.T): T = 
  BEGIN
    t.body  := body;
    t.font  := f;
    t.state := SilObject.Selected;
    t.next  := NIL;
    t.bbox.west  := p.h;
    t.bbox.south := p.v;
    SetBBox (t, w);
    RETURN t;
  END Init;
  
PROCEDURE Write (t: T;  wr: SilWr.T) =
  VAR x: INTEGER;
  BEGIN
    IF (Text.Length (t.body) > 0) AND (t.state >= SilObject.Visible) THEN
      x := SilFont.Write (t.font, wr);
      wr.putText ("s ");
      SilObject.WriteBBox (t, wr);
      wr.putInt (x);
      wr.putStr (t.body);
      wr.endLine ();
    END;
  END Write;
  
PROCEDURE Read (rd: SilRd.T): REFANY =
  VAR t := NEW (T);
  BEGIN
    <*ASSERT rd.n_strs = 1 AND rd.n_ints = 5 *>
    SilObject.ReadBBox (t, rd);
    t.body := rd.strs[0];
    t.font := SilFont.FromID (rd, rd.ints[4]);
    RETURN t;
  END Read;

PROCEDURE Clone (t: T): SilObject.T =
  BEGIN
    RETURN NEW (T, body := t.body, font := t.font);
  END Clone;
  
PROCEDURE Draw (t:T; READONLY p: Point.T; sel: BOOLEAN; w: SilWindow.T) =
  VAR color := 0;
  BEGIN
    IF t.state >= SilObject.Visible THEN
      IF (t.state = SilObject.Selected) OR sel THEN color := 16_ff; END;
      SilFont.Select (t.font, w);
      EVAL WinGDI.SetTextColor (w.dc, color);
      EVAL WinGDI.SetBkMode (w.dc, WinGDI.TRANSPARENT);
      EVAL WinGDI.TextOut (w.dc, t.bbox.west + p.h, t.bbox.north + p.v,
                           M3toC.TtoS (t.body), Text.Length (t.body));
    END;
  END Draw;
  
PROCEDURE TakeChar (t: T;  ch: CHAR;  w: SilWindow.T;
                    VAR(*OUT*) bbox: Rect.T): BOOLEAN =
  VAR len: INTEGER;
  BEGIN
    IF (t.state < SilObject.Selected) THEN RETURN FALSE; END;

    IF (ch = '\010') THEN
      (* backspace *)
      len := Text.Length (t.body);
      IF (len > 0) THEN  t.body := Text.Sub (t.body, 0, len-1);  END;
      bbox := t.bbox; (* zap the "before" box *)
      SetBBox (t, w);
    ELSIF (ch = '\r') OR (ch = '\n') THEN
      (* ignore returns and linefeeds *)
      SetBBox (t, w);
      bbox := t.bbox;
    ELSE
      (* add the character *)
      t.body := t.body & Text.FromChar (ch);
      SetBBox (t, w);
      bbox := t.bbox;
    END;

    RETURN TRUE;
  END TakeChar;
  
PROCEDURE SetFont (t: T; f: SilFont.T; sel: BOOLEAN; w: SilWindow.T): BOOLEAN =
  BEGIN
    IF (t.state = SilObject.Selected) OR sel THEN
      t.font := f;
      SetBBox (t, w);
      RETURN TRUE;
    END;
    RETURN FALSE;
  END SetFont;

PROCEDURE SetBBox (t: T;  w: SilWindow.T) =
  VAR sz: WinDef.SIZE;
  BEGIN
    SilFont.Select (t.font, w);
    IF WinGDI.GetTextExtentPoint (w.dc, M3toC.TtoS (t.body),
                                  Text.Length (t.body), ADR (sz)) = 0 THEN
      SilError.Put (NIL, "unable to get string size");
    END;
    t.bbox.north := t.bbox.south - 1 - sz.cy;
    t.bbox.east  := t.bbox.west  + 2 + sz.cx;
  END SetBBox;

BEGIN
  SilWindow.RegisterReader ('s', 's', Read);
END SilString.
