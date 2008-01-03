(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Fri Oct 18 15:22:34 PDT 1996 by najork                   *)
(*       Created on Tue Jan 17 16:51:19 PST 1995 by najork                   *)


UNSAFE MODULE WinScrnPixmap;

IMPORT Axis, Palette, Pixmap, Point, Rect, ScrnPixmap, TrestleImpl, VBTRep, 
       WinDef, WinGDI, WinScreenType, WinScreenTypePrivate, WinUser;

IMPORT Ctypes, Fmt, IO;

CONST
  True = 1;

<* PRAGMA LL *>

EXCEPTION FatalError;
<* FATAL FatalError *>

TYPE
  T = ScrnPixmap.T BRANDED OBJECT 
    st: WinScreenType.T;
  OVERRIDES
    localize := Localize;
    unload   := Unload;
    free     := Free;
  END;

PROCEDURE Localize (self: T; READONLY rect: Rect.T): ScrnPixmap.Raw =
  VAR
    id    := self.id;
    st    := self.st;
    hwnd  : WinDef.HWND;
    hdc   : WinDef.HDC;
    hbmp  : WinDef.HBITMAP;
    status: INTEGER;
    res   : ScrnPixmap.Raw;
    bmi   : WinGDI.BITMAPINFO;
    pixels: REF ARRAY OF WinGDI.RGBQUAD;
    k     : INTEGER;
    r     : Rect.T;
  BEGIN
    IF id = SolidPixmap THEN RETURN rawSolid END;

    r := Rect.Meet(rect, self.bounds);
    IF Rect.IsEmpty (r) THEN RETURN NIL END;

    IF id < 0 THEN 
      id := SolidPixmap - id;
      st := st.bits;
    END;

    LOCK st.trsl DO
      WITH pmr = st.pmtable[id] DO
        <* ASSERT pmr.domain = self.bounds *>
        (* ... if that's true, can we do away with pmr.domain? *)
        hbmp := pmr.hbmp;
      END;
    END;

    (* Examine the depth *)
    hwnd := WinUser.GetDesktopWindow ();
    <* ASSERT hwnd # NIL *>
    hdc := WinUser.GetDC (hwnd);
    <* ASSERT hdc # NIL *>

    WITH height = Rect.VerSize (self.bounds),
         width  = Rect.HorSize (self.bounds),
         bmih   = bmi.bmiHeader DO
      bmi.bmiHeader.biSize := BYTESIZE(WinGDI.BITMAPINFOHEADER);
      bmi.bmiHeader.biBitCount := 0;
      status := WinGDI.GetDIBits (hdc, 
                                  hbmp,      
                                  0,          (* start at scan line 0 *)
                                  height,     (* copy "height" lines *)
                                  NIL,        (* ... that is, don't copy *)
                                  ADR (bmi),  (* ... just fill in bmi *)
                                  WinGDI.DIB_RGB_COLORS);
      <* ASSERT status = True *>

      <* ASSERT bmih.biWidth = width *>
      <* ASSERT bmih.biHeight = height *>

      IF bmih.biBitCount = 1 THEN
        res := ScrnPixmap.NewRaw (1, r);
      ELSE
        res := ScrnPixmap.NewRaw (BITSIZE (WinDef.COLORREF), r); 
      END;

      bmih.biBitCount := 32;
      bmih.biCompression := WinGDI.BI_RGB;

      pixels := NEW (REF ARRAY OF WinGDI.RGBQUAD, height * width);
      status := WinGDI.GetDIBits (hdc, 
                                  hbmp,      
                                  0,             (* start at scan line 0 *)
                                  height,        (* copy "height" lines *)
                                  ADR(pixels[0]),(* into "pixels" *)
                                  ADR (bmi),
                                  WinGDI.DIB_RGB_COLORS);
      <* ASSERT status = height *>

      <* ASSERT bmih.biBitCount = BITSIZE (WinDef.COLORREF) *>
      <* ASSERT bmih.biWidth = Rect.HorSize (self.bounds) *>
      <* ASSERT bmih.biHeight = Rect.VerSize (self.bounds) *>
    END;

    (* Copy "pixels" into "res" *)
    k := 0;
    FOR v := self.bounds.south - 1 TO self.bounds.north BY -1 DO
      FOR h := self.bounds.west TO self.bounds.east - 1 DO
        WITH pt = Point.T {h, v} DO
          IF Rect.Member (pt, r) THEN
            IF res.depth = 1 THEN
              IF pixels[k] = WinGDI.RGBQUAD {0, 0, 0, 0} THEN
                res.set (pt, 0);
              ELSE
                res.set (pt, 1);
              END;
            ELSE
              WITH p   = pixels[k],
                   col = WinGDI.RGB (p.rgbRed, p.rgbGreen, p.rgbBlue) DO
                res.set (pt, col);
              END;
            END;
          END;
          INC (k);
        END;
      END;
    END;

    status := WinUser.ReleaseDC (hwnd, hdc);
    <* ASSERT status = True *>

    RETURN res;
  END Localize;

(*-----------------------------------------------------------------------------
   The spec in ScrnPixmap.i3 states:

       The method call "pm.unload()" causes "pm" to become anonymous.

   The X version (XScrnPxmp.Unregister) doesn't do anything. 
   So, we do the same.
-----------------------------------------------------------------------------*)


PROCEDURE Unload (<*UNUSED*> self: T) =
  BEGIN
    (* do nothing *)
  END Unload;


PROCEDURE Free (self: T) =
  VAR
    id     := self.id;
    st     := self.st;
    status : WinDef.BOOL;
  BEGIN
    IF id = SolidPixmap THEN RETURN; END;
    IF id < 0 THEN 
      id := SolidPixmap - id;
      st := st.bits;
    END;
    LOCK st.trsl DO
      WITH z = st.pmtable[id] DO
        z.domain.north := st.pmfree;
        st.pmfree := id;
        IF z.hbmp # NIL THEN
          status := WinGDI.DeleteObject (z.hbmp);
          <* ASSERT status = True *>
          z.hbmp := NIL;
        END;
      END;
    END;
  END Free;

(*****************************************************************************)

TYPE
  Oracle = ScrnPixmap.Oracle BRANDED OBJECT
    st: WinScreenType.T;
  OVERRIDES
    load    := Load;
    list    := List;
    lookup  := Lookup;
    builtIn := BuiltIn;
  END;

PROCEDURE Load (                    self: Oracle;
                           READONLY pm  : ScrnPixmap.Raw;
                <*UNUSED*>          nm  : TEXT := NIL): ScrnPixmap.T =
  BEGIN
    WITH st = self.st DO
      LOCK st.trsl DO
        IF pm.depth # 1 AND pm.depth # st.depth THEN
          RAISE FatalError;
        END;
        RETURN NewPixmap (st, PixmapFromRaw (st, pm), pm.bounds, pm.depth);
      END;
    END;
  END Load;

PROCEDURE DumpPixmap (pm: ScrnPixmap.T) =
  BEGIN
    WITH st = NARROW (pm, T).st DO
      IO.Put ("WinPixmap.T {\n");
      IO.Put ("  id := " & Fmt.Int (pm.id) & "\n");
      IO.Put ("  depth := " & Fmt.Int (pm.depth) & "\n");
      IO.Put ("  bounds := " & Fmt_Rect(pm.bounds) & "\n");
      IF st = st.bits
        THEN IO.Put ("  st := a monochrome screen type\n");
        ELSE IO.Put ("  st := a color screen type\n");
      END;
    END;
  END DumpPixmap;

PROCEDURE DumpPixmapRecord (pmr: PixmapRecord) =
  VAR
    bitmap: WinGDI.BITMAP;
    sz    : Ctypes.int;
  BEGIN
    IO.Put ("WinScrnPixmap.PixmapRecord{\n");
    IO.Put ("  hbmp   := " & Fmt_Addr (pmr.hbmp)   & "\n");
    IO.Put ("  domain := " & Fmt_Rect (pmr.domain) & "\n");
    IO.Put ("}\n");
    sz := WinGDI.GetObject(pmr.hbmp, BYTESIZE (bitmap), ADR(bitmap));
    IF sz = 0 THEN
      IO.Put ("could not get dimensions of bitmap!\n");
    ELSE
      IO.Put ("bmWidth  = " & Fmt.Int(bitmap.bmWidth));
      IO.Put ("bmHeight = " & Fmt.Int(bitmap.bmHeight));
    END;
  END DumpPixmapRecord;


PROCEDURE DumpRaw (pm: ScrnPixmap.Raw) =
  VAR dom := pm.bounds;
  BEGIN
    IO.Put ("ScrnPixmap.Raw: \n");
    IO.Put ("   depth = " & Fmt.Int (pm.depth) & "\n");
    IO.Put ("   bounds = {" & Fmt_Rect (dom) & "\n");
    IO.Put ("   bitsPerPixel = " & Fmt.Int (pm.bitsPerPixel) & "\n");
    IO.Put ("   wordsPerRow = " & Fmt.Int (pm.wordsPerRow) & "\n");
    IO.Put ("   One row of pixels from the middle:\n");
    IF pm.pixelOrder = ScrnPixmap.ByteOrder.MSBFirst THEN
      IO.Put ("   byteOrder = MSBFirst\n");
    ELSE
      IO.Put ("   byteOrder = LSBFirst\n");
    END;
    IO.Put ("   westRounded = " & Fmt.Int (pm.westRounded) & "\n");
    FOR v := dom.north TO MIN (dom.north + 19, dom.south - 1) DO
      IO.Put("   row " & Fmt.Pad (Fmt.Int(v),2) & ": ");
      FOR h := dom.west TO MIN (dom.west + 19, dom.east - 1) DO
        IO.Put(Fmt.Pad(Fmt.Int(pm.get(Point.T{h,v}), base := 16),2,'0') & " ");
      END;
      IO.Put ("\n");
    END;
  END DumpRaw;

PROCEDURE Fmt_Rect (r: Rect.T): TEXT =
  BEGIN
    RETURN "Rect.T{" & 
           Fmt.Int(r.west) & "," & 
           Fmt.Int(r.east) & "," & 
           Fmt.Int(r.north) & "," & 
           Fmt.Int(r.south) & "}";
  END Fmt_Rect;

PROCEDURE Fmt_Addr (a: ADDRESS): TEXT =
  BEGIN
    WITH i = LOOPHOLE (a, INTEGER) DO
      RETURN Fmt.Int (i, base := 16);
    END;
  END Fmt_Addr;


(*-----------------------------------------------------------------------------
   The spec in ScrnPixmap.i3 states:

       The method call "st.pixmap.list(pat, maxResults)" returns the names
       of all pixmaps owned by "st" that match the pattern "pat".  The list
       of results may be truncated to length "maxResults".  A "*" matches
       any number of characters and a "?" matches any single character.

   The X version (XScrnPxmp.PixmapList), however, simply always returns NIL.
   For now, I do the same ... 
-----------------------------------------------------------------------------*)

PROCEDURE List (<*UNUSED*> self      : Oracle;
                <*UNUSED*> pat       : TEXT;
                <*UNUSED*> maxResults: CARDINAL): REF ARRAY OF TEXT =
  BEGIN
    RETURN NIL
  END List;


(*-----------------------------------------------------------------------------
   The spec in ScrnPixmap.i3 states:

       The method call "st.pixmap.lookup(name)" return the pixmap with the
       given name, or "NIL" if no pixmap has this name.

   The X version (XScrnPxmp.PixmapLookup), however, simply always returns NIL.
   For now, I do the same ... 
-----------------------------------------------------------------------------*)

PROCEDURE Lookup (<*UNUSED*> self: Oracle; 
                  <*UNUSED*> name: TEXT): ScrnPixmap.T =
  BEGIN
    RETURN NIL;
  END Lookup;


PROCEDURE BuiltIn (self: Oracle; pm: Pixmap.Predefined): ScrnPixmap.T =
  BEGIN
    IF self.st.bits # self.st THEN
      RETURN Palette.ResolvePixmap (self.st.bits, Pixmap.T {pm});
    END;
    CASE pm OF
    | Pixmap.Solid.pm =>
      WITH res = Load (self, rawSolid) DO
        res.id := SolidPixmap;
        RETURN res;
      END;
    | Pixmap.Gray.pm =>
      RETURN Load (self, rawGray);
    | Pixmap.Empty.pm =>
      RETURN Load (self, rawEmpty);
    ELSE
      RAISE FatalError;
    END;
  END BuiltIn;

(*****************************************************************************)
(* Exported procedures                                                       *)
(*****************************************************************************)


PROCEDURE NewOracle(st : WinScreenType.T): ScrnPixmap.Oracle =
  BEGIN
    RETURN NEW (Oracle, st := st);
  END NewOracle;

PROCEDURE PixmapDomain (st: WinScreenType.T; pmId: INTEGER): Rect.T =
  BEGIN
    IF pmId = SolidPixmap THEN RETURN rawSolid.bounds END;
    IF pmId < 0 THEN
      pmId := SolidPixmap - pmId;
      st := st.bits;
    END;
    IF pmId < NUMBER (st.pmtable^)
      THEN RETURN st.pmtable[pmId].domain;
      ELSE RETURN Rect.Empty;
    END;
  END PixmapDomain;


(*
 * The xvbt version of this function is quite a hack: The actual image data
 * of a "ScrnPixmap.Raw" is stored in a field "pixels". It just so happens
 * that the memory layout of "pixels" is identical to the layout expected by 
 * the "data" field of an "X.XImage" record.  So, the xvbt version simply
 * creates an XImage, loopholes the "pixels" field into the "data" field,
 * then creates an "X.Pixmap", paints the "X.XImage" onto the "X.Pixmap",
 * and returns the X pixmap.
 *
 * The Windows version currently deals only with monochrome bitmaps 
 * (which makes sense, since I didn't implement colors yet either)
 *)

PROCEDURE PixmapFromRaw (st: WinScreenType.T; 
                         pm: ScrnPixmap.Raw): WinDef.HBITMAP =
  <* LL.sup = st.trsl *> 

  PROCEDURE ConvertMonochrome (pm: ScrnPixmap.Raw): WinDef.HBITMAP =
    TYPE 
      WinWord = Ctypes.unsigned_short;
      Bit     = BITS 1 FOR [0..1];
      Byte    = BITS 8 FOR ARRAY [0..7] OF Bit;
      TwoByte = BITS 16 FOR ARRAY [0..1] OF Byte;
    CONST
      WinWordSize = BITSIZE (WinWord);
    BEGIN
      WITH pix_width  = pm.bounds.east  - pm.bounds.west,
           pix_height = pm.bounds.south - pm.bounds.north,
           word_width = (pix_width - 1) DIV WinWordSize + 1,
           words      = NEW (REF ARRAY OF WinWord, word_width * pix_height) DO
        (* first, let's blank the array *)
        FOR i := 0 TO word_width * pix_height - 1 DO
          words[i] := 0;
        END;
        (* Next, let's transfer the bits from pm.pixels to bits *)
        FOR v := 0 TO pix_height - 1 DO
          FOR h := 0 TO pix_width - 1 DO
            WITH pt  = Point.T{pm.bounds.west + h, pm.bounds.north + v},
                 word = v * word_width + h DIV WinWordSize,
                 byte = (h MOD WinWordSize) DIV 8,
                 bit  = 7 - h MOD 8 DO
              LOOPHOLE (words[word], TwoByte)[byte][bit] := pm.get(pt);
            END;
          END;
        END;
        WITH res = WinGDI.CreateBitmap (pm.bounds.east - pm.bounds.west,
                                        pm.bounds.south - pm.bounds.north,
                                        1, 1, ADR(words[0])) DO
          <* ASSERT res # NIL *>
          RETURN res;
        END;
      END;
    END ConvertMonochrome;


  PROCEDURE ConvertColor (st: WinScreenType.T; 
                          pm: ScrnPixmap.Raw): WinDef.HBITMAP =
    VAR
      hwnd   : WinDef.HWND;
      hdc    : WinDef.HDC;
      hbmp   : WinDef.HBITMAP;
      pixels : REF ARRAY OF WinGDI.RGBQUAD;
      k      : INTEGER;
      status : WinDef.BOOL;
      bmi    : WinGDI.BITMAPINFO;
    BEGIN
      hwnd := WinUser.GetDesktopWindow ();
      <* ASSERT hwnd # NIL *>
      hdc := WinUser.GetDC (hwnd);
      <* ASSERT hdc # NIL *>
      
      pixels := NEW (REF ARRAY OF WinGDI.RGBQUAD,
                     (pm.bounds.south - pm.bounds.north) * 
                     (pm.bounds.east - pm.bounds.west));
      k := 0;
      FOR i := pm.bounds.south - 1 TO pm.bounds.north BY -1 DO
        FOR j := pm.bounds.west TO pm.bounds.east - 1 DO
          WITH pixel = pm.get(Point.T{j,i}),
               red   = WinGDI.GetRValue (pixel),
               green = WinGDI.GetGValue (pixel),
               blue  = WinGDI.GetBValue (pixel) DO
            pixels[k] := WinGDI.RGBQUAD {blue, green, red, 0};
          END;
          INC (k);
        END;
      END;

      WITH bmih = bmi.bmiHeader DO
        bmih.biSize          := BYTESIZE(WinGDI.BITMAPINFOHEADER);
        bmih.biWidth         := pm.bounds.east - pm.bounds.west;
        (* Windows NT bug: According to the doc, a negative value for
           biHeight indicates a top-down bitmap, that is, a bitmap that 
           starts in the upper-left corner. However, if I actually pass 
           a negative value, the bitmap comes out solid black most of the
           time (although at some point, it came out ok ...) *)
        bmih.biHeight        := pm.bounds.south - pm.bounds.north;
        bmih.biPlanes        := 1;   (* always 1 *)
        bmih.biBitCount      := 32;
        bmih.biCompression   := WinGDI.BI_RGB;
        bmih.biSizeImage     := 0;   (* 0 is valid only for BI_RGB *)
        bmih.biXPelsPerMeter := ROUND (st.res[Axis.T.Hor] * 1000.0);
        bmih.biYPelsPerMeter := ROUND (st.res[Axis.T.Ver] * 1000.0);
        bmih.biClrUsed       := 0;   (* bitmap uses all the colors *)
        bmih.biClrImportant  := 0;   (* all colors are important *)
        
        hbmp := WinGDI.CreateDIBitmap (hdc, 
                                       ADR(bmih), 
                                       WinGDI.CBM_INIT, 
                                       LOOPHOLE (ADR (pixels[0]), 
                                                 WinDef.LPVOID),
                                       ADR (bmi),
                                       WinGDI.DIB_RGB_COLORS);
      END;

      status := WinUser.ReleaseDC (hwnd, hdc);
      <* ASSERT status = True *>
      RETURN hbmp;
    END ConvertColor;

  BEGIN
    IF Rect.IsEmpty (pm.bounds) THEN 
      RETURN NIL ;
    ELSIF pm.depth = 1 AND pm.bitsPerPixel = 1 THEN
      RETURN ConvertMonochrome (pm);
    ELSE
      RETURN ConvertColor (st, pm);
    END;
  END PixmapFromRaw;


(*****************************************************************************)
(* Private procedures                                                        *)
(*****************************************************************************)


PROCEDURE NewPixmap (         st    : WinScreenType.T;
                              hbmp  : WinDef.HBITMAP;
                     READONLY domain: Rect.T;
                              depth : INTEGER): ScrnPixmap.T =
  <* LL.sup = st.trsl *> 
  VAR id, slot: INTEGER;
  BEGIN
    IF depth = 1 THEN st := st.bits END;
    IF (st.pmfree >= 0) THEN
      slot := st.pmfree;
      st.pmfree := st.pmtable[slot].domain.north;
    ELSE
      slot := st.pmcount;
      INC(st.pmcount);
      IF (slot = NUMBER (st.pmtable^)) THEN ExpandPixmapTable (st); END;
    END;
    IF st.bits = st THEN
      id := SolidPixmap - slot;
    ELSE
      id := slot;
    END;
    st.pmtable[slot] := PixmapRecord {hbmp, domain};
    RETURN NEW (T, st := st, id := id, depth := depth, bounds := domain);
  END NewPixmap;

PROCEDURE ExpandPixmapTable (st: WinScreenType.T) =
  VAR n := NUMBER (st.pmtable^);  new := NEW (REF ARRAY OF PixmapRecord, n+n);
  BEGIN
    SUBARRAY (new^, 0, n) := st.pmtable^;
    st.pmtable := new;
  END ExpandPixmapTable;


VAR
  rawSolid, rawGray, rawEmpty: ScrnPixmap.Raw;

PROCEDURE InitPredefRaws () =
  BEGIN
    WITH r = Rect.FromSize (8, 8) DO
      rawSolid := ScrnPixmap.NewRaw (1, r);
      rawEmpty := ScrnPixmap.NewRaw (1, r);
      rawGray  := ScrnPixmap.NewRaw (1, r);
      FOR x := r.west TO r.east - 1 DO
        FOR y := r.north TO r.south - 1 DO
          WITH p = Point.T {x, y} DO
            rawSolid.set (p, 1);
            rawEmpty.set (p, 0);
            rawGray.set (p, (x + y) MOD 2);
          END;
        END;
      END;
    END;    
  END InitPredefRaws;


BEGIN
  InitPredefRaws();
END WinScrnPixmap.
