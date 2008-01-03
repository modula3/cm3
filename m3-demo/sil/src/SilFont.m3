(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Wed Nov  9 08:07:09 PST 1994 by kalsow    *)

UNSAFE MODULE SilFont;

IMPORT Fmt, Text, Cstring, Ctypes, M3toC, WinDef, WinGDI, WinUser;
IMPORT SilWr, SilRd, SilWindow;

REVEAL
  T = BRANDED "SilFont" REF RECORD
    next    : T                := NIL;
    name    : TEXT             := NIL;
    age     : INTEGER          := 0;
    file_id : INTEGER          := 0;
    handle  : WinDef.HFONT     := NIL;
    desc    : WinGDI.PLOGFONT  := NIL;
    menu    : Ctypes.char_star := NIL;
  END;

VAR all_fonts: T := NIL;

PROCEDURE New (name: TEXT;  height, weight: INTEGER;  italics: BOOLEAN): T =
  VAR t := all_fonts;
  BEGIN
    (* check for an existing font that matches *)
    WHILE (t # NIL) DO
      IF (t.desc.lfHeight = height)
        AND (t.desc.lfWeight = weight)
        AND (t.desc.lfItalic = ORD (italics))
        AND Text.Equal (t.name, name) THEN
        RETURN t;
      END;
      t := t.next;
    END;

    (* no match => build a new font *)
    t := NEW (T, name := name, desc := NEW (WinGDI.PLOGFONT));
    WITH x = t.desc^ DO
      x.lfHeight        := height;
      x.lfWidth         := 0;
      x.lfEscapement    := 0;
      x.lfOrientation   := 0;
      x.lfWeight        := weight;
      x.lfItalic        := ORD (italics);
      x.lfUnderline     := 0;
      x.lfStrikeOut     := 0;
      x.lfCharSet       := WinGDI.ANSI_CHARSET;
      x.lfOutPrecision  := WinGDI.OUT_DEFAULT_PRECIS;
      x.lfClipPrecision := WinGDI.CLIP_DEFAULT_PRECIS;
      x.lfQuality       := WinGDI.PROOF_QUALITY;
      x.lfPitchAndFamily:= WinGDI.VARIABLE_PITCH + WinGDI.FF_ROMAN;
      EVAL Cstring.strncpy (ADR (x.lfFaceName[0]), M3toC.TtoS (name),
                            NUMBER (x.lfFaceName));

      (* build a menu entry *)
      VAR
        pts := ABS(height) * 200 DIV 270;
        tag := name & " " & Fmt.Int (pts) & " ";
      BEGIN
        IF weight >= 700 THEN tag := tag & "B"; END;
        IF italics THEN tag := tag & "I"; END;
        t.menu := M3toC.CopyTtoS (tag);
      END;
    END;
    t.handle := WinGDI.CreateFontIndirect (t.desc);
    t.next := all_fonts;
    all_fonts := t;
    RETURN t;
  END New;

PROCEDURE Write (t: T;  wr: SilWr.T): INTEGER =
  BEGIN
    IF (t.age < wr.generation) THEN
      t.age     := wr.generation;
      t.file_id := wr.next_font_id;  INC (wr.next_font_id);
      wr.putText ("f ");
      wr.putStr  (t.name);
      wr.putInt  (t.desc.lfHeight);
      wr.putInt  (t.desc.lfWeight);
      wr.putInt  (t.desc.lfItalic);
      wr.endLine ();
    END;
    RETURN t.file_id;
  END Write;

TYPE
  RdInfo = REF RECORD
    cnt   : INTEGER;
    fonts : REF ARRAY OF T;
  END;

PROCEDURE Read (rd: SilRd.T): REFANY =
  VAR info : RdInfo := rd.fonts;  t: T;
  BEGIN
    <*ASSERT rd.n_strs = 1 AND rd.n_ints = 3*>
    t := New (rd.strs[0], rd.ints[0], rd.ints[1], rd.ints[2] # 0);
    IF (info = NIL) OR (info.cnt >= NUMBER (info.fonts^)) THEN
      info := ExpandInfo (info);
      rd.fonts := info;
    END;
    info.fonts [info.cnt] := t;
    INC (info.cnt);
    RETURN NIL;
  END Read;

PROCEDURE FromID (rd: SilRd.T;  id: INTEGER): T =
  VAR info : RdInfo := rd.fonts;
  BEGIN
    RETURN info.fonts[id];
  END FromID;

PROCEDURE ExpandInfo (info: RdInfo): RdInfo =
  VAR n: INTEGER;  new: REF ARRAY OF T;
  BEGIN
    IF (info = NIL) THEN
      info := NEW (RdInfo);
      info.fonts := NEW (REF ARRAY OF T, 100);
    ELSE
      n := NUMBER (info.fonts^);
      new := NEW (REF ARRAY OF T, n+n);
      SUBARRAY (new^, 0, n) := info.fonts^;
      info.fonts := new;
    END;
    RETURN info;
  END ExpandInfo;

PROCEDURE Select (t: T;  w: SilWindow.T) =
  BEGIN
    EVAL WinGDI.SelectObject (w.dc, t.handle);
  END Select;

PROCEDURE BuildMenu (msg: INTEGER): WinDef.HMENU =
  VAR np := WinUser.CreatePopupMenu ();  t: T;
  BEGIN
    EVAL WinUser.AppendMenu (np, WinUser.MF_ENABLED + WinUser.MF_STRING,
                             msg, M3toC.TtoS ("&Select..."));
    INC (msg);
    	
    t := all_fonts;
    WHILE (t # NIL) DO
      EVAL WinUser.AppendMenu (np, WinUser.MF_ENABLED + WinUser.MF_STRING,
                               msg, t.menu);
      t := t.next; INC (msg);
    END;

    RETURN np;
  END BuildMenu;

PROCEDURE FromMenu (x: INTEGER): T =
  VAR t := all_fonts;
  BEGIN
    WHILE (x > 0) AND (t # NIL) AND (t.next # NIL) DO
      t := t.next;
      DEC (x);
    END;
    RETURN t;
  END FromMenu;

PROCEDURE Reset () =
  VAR t := all_fonts;
  BEGIN
    WHILE (t # NIL) DO
      EVAL WinGDI.DeleteObject (t.handle);
      M3toC.FreeCopiedS (t.menu);
      DISPOSE (t.desc);
      t.handle := NIL;
      t.desc   := NIL;
      t.name   := NIL;
      t.menu   := NIL;
      t := t.next;
    END;
    all_fonts := NIL;
  END Reset;

BEGIN
  SilWindow.RegisterReader ('f', 'f', Read);
END SilFont.
