(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug  6 11:31:42 PDT 1996 by najork                   *)
(*       Created on Tue Jan 17 11:35:36 PST 1995 by najork                   *)


UNSAFE MODULE WinScrnColorMap;

IMPORT Ctypes, Math, ScrnColorMap, WinDef, WinGDI, WinUser;

CONST
  Gamma        = 1.1D0;
  GammaInverse = 1.0D0 / Gamma;

TYPE
  T = ScrnColorMap.T BRANDED OBJECT
  OVERRIDES
    fromRGB := FromRGB;
    read    := Read;
    write   := Write;
    new     := NewCube;
    free    := FreeCube;
  END;

PROCEDURE FromRGB (<*UNUSED*> self: T;
                              rgb : ScrnColorMap.RGB;
                   <*UNUSED*> mode: ScrnColorMap.Mode ): ScrnColorMap.Pixel
    (** RAISES {ScrnColorMap.Failure} **) =
  BEGIN
    (* This is an extremely naive implementation; it only utilizes 
       the colors that come with the standard Windows palette. 

       Using "WinGDI.RGB" causes dithering, and two shades of red in the 
       solitaire cards. *)

    WITH r = ROUND (Math.pow(FLOAT(rgb.r,LONGREAL), GammaInverse) * 255.0D0),
         g = ROUND (Math.pow(FLOAT(rgb.g,LONGREAL), GammaInverse) * 255.0D0),
         b = ROUND (Math.pow(FLOAT(rgb.b,LONGREAL), GammaInverse) * 255.0D0) DO
      RETURN WinGDI.PALETTERGB (r, g, b);
    END;
  END FromRGB;

PROCEDURE Read (<*UNUSED*> self: T;
                VAR res: ARRAY OF ScrnColorMap.Entry) =
  BEGIN
    FOR i := FIRST(res) TO LAST(res) DO
      WITH r = WinGDI.GetRValue (res[i].pix),
           g = WinGDI.GetGValue (res[i].pix),
           b = WinGDI.GetBValue (res[i].pix) DO
        res[i].rgb.r := FLOAT(Math.pow(FLOAT(r, LONGREAL) / 255.0D0, Gamma));
        res[i].rgb.g := FLOAT(Math.pow(FLOAT(g, LONGREAL) / 255.0D0, Gamma));
        res[i].rgb.b := FLOAT(Math.pow(FLOAT(b, LONGREAL) / 255.0D0, Gamma));
      END;
    END;
  END Read;


PROCEDURE Write (<*UNUSED*> self: T;
                 <*UNUSED*> READONLY new : ARRAY OF ScrnColorMap.Entry)
    (** RAISES {ScrnColorMap.Failure} **) =
  BEGIN
    <* ASSERT FALSE *>  (* not yet implemented *)
  END Write;


PROCEDURE NewCube (<*UNUSED*> self: T;
                   <*UNUSED*> d: CARDINAL): ScrnColorMap.Cube
    (** RAISES {ScrnColorMap.Failure} **) =
  BEGIN
    <* ASSERT FALSE *>  (* not yet implemented *)
  END NewCube;


PROCEDURE FreeCube (<*UNUSED*> self: T;
                    <*UNUSED*> READONLY cb: ScrnColorMap.Cube) =
  BEGIN
    <* ASSERT FALSE *>  (* not yet implemented *)
  END FreeCube;

(*****************************************************************************)
(* Oracle                                                                    *)
(*****************************************************************************)

TYPE
  Oracle = ScrnColorMap.Oracle BRANDED OBJECT 
  OVERRIDES
    standard := Standard;
    list     := List;
    lookup   := Lookup;
    new      := NewMap;
  END;


PROCEDURE NewOracle (): ScrnColorMap.Oracle =
  BEGIN
    RETURN NEW (Oracle);
  END NewOracle;


PROCEDURE Standard (<*UNUSED*> self: Oracle): ScrnColorMap.T =
  VAR t := NEW (T);
  BEGIN
    t.depth     := 8;
    t.readOnly  := FALSE;
    t.ramp.base := 0;
    t.ramp.last [ScrnColorMap.Primary.Red]   := 255;
    t.ramp.mult [ScrnColorMap.Primary.Red]   := 16_000001;
    t.ramp.last [ScrnColorMap.Primary.Green] := 255;
    t.ramp.mult [ScrnColorMap.Primary.Green] := 16_000100;
    t.ramp.last [ScrnColorMap.Primary.Blue]  := 255;
    t.ramp.mult [ScrnColorMap.Primary.Blue]  := 16_010000;
    RETURN t;
  END Standard;

(*-----------------------------------------------------------------------------
   The spec in ScrnColormap.i3 states:

       The method call "st.cmap.list(pat, maxResults)" returns the names of 
       colormaps owned by "st" that match the pattern "pat".  The list of 
       results may be truncated to length "maxResults".  A "*" matches any 
       number of characters and a "?" matches any single character.

   However, the X version (XScrnCmap.ColorMapList) always returns NIL.
   Since this seems to be adequate, we do the same ...
-----------------------------------------------------------------------------*)


PROCEDURE List (<*UNUSED*> self      : Oracle;
                <*UNUSED*> pat       : TEXT;
                <*UNUSED*> maxResults: CARDINAL): REF ARRAY OF TEXT =
  BEGIN
    RETURN NIL
  END List;


(*-----------------------------------------------------------------------------
   The spec in ScrnColormap.i3 states:

       The method call "st.cmap.lookup(name)" returns the colormap owned by 
       "st" with the given name, or "NIL" if no colormap has this name.

   However, the X version (XScrnCmap.ColorMapLookup always returns NIL.
   Since this seems to be adequate, we do the same ...
-----------------------------------------------------------------------------*)


PROCEDURE Lookup (<*UNUSED*> self: Oracle;
                  <*UNUSED*> pat : TEXT): ScrnColorMap.T =
  BEGIN
    RETURN NIL
  END Lookup;


PROCEDURE NewMap (<*UNUSED*> self     : Oracle;
                  <*UNUSED*> nm       : TEXT;
                  <*UNUSED*> preLoaded: BOOLEAN): ScrnColorMap.T =
  BEGIN
    RETURN NEW (T);
  END NewMap;

CONST
  MaxPalEntries = 1024;

  (*** for 8x8x8 cube *****
  NPalEntries = 512;
  PalValues   = ARRAY [0..7] OF [0..255] { 0, 36, 72, 108, 145, 182, 219, 255 };
  *************************)

  (*** for 6x6x6 cube *****)
  NPalEntries = 216;
  PalValues   = ARRAY [0..5] OF [0..255] { 0, 51, 102, 153, 204, 255 };

  (*** for 5x5x5 cube *****
  NPalEntries = 125;
  PalValues   = ARRAY [0..4] OF [0..255] { 0, 64, 128, 192, 255 };
  *************************)

VAR
  defaultPal: WinDef.HPALETTE := NIL;
  defaultPalette: RECORD
    palVersion   : WinDef.WORD;
    palNumEntries: WinDef.WORD;
    palPalEntry  : ARRAY [1 .. MaxPalEntries] OF WinGDI.PALETTEENTRY;
  END;

PROCEDURE DefaultPalette (): WinDef.HPALETTE =
  (* Create a logical palette *)
  VAR n_colors: INTEGER;
  BEGIN
    IF (defaultPal = NIL) THEN
      n_colors := NumDeviceColors ();
      IF (64 < n_colors) AND (n_colors <= MaxPalEntries)
        THEN MatchCurrentPalette ();
        ELSE InitDefaultPalette ();
      END;
      defaultPal := WinGDI.CreatePalette (LOOPHOLE (ADR(defaultPalette),
                                                    WinGDI.LPLOGPALETTE));
      <* ASSERT defaultPal # NIL *>
    END;
    RETURN defaultPal;
  END DefaultPalette;

PROCEDURE InitDefaultPalette () =
  (* Fill the colors of a color cube into the "defaultPalette" record. *)
  VAR i := 1;
  BEGIN
    defaultPalette.palVersion    := 16_300;   (* Windows version number *)
    defaultPalette.palNumEntries := NPalEntries;

    WITH pe = defaultPalette.palPalEntry DO
      FOR r := FIRST (PalValues) TO LAST (PalValues) DO
        FOR g := FIRST (PalValues) TO LAST (PalValues) DO
          FOR b := FIRST (PalValues) TO LAST (PalValues) DO
            pe[i] := WinGDI.PALETTEENTRY { PalValues[r], PalValues[g],
                                           PalValues[b],  WinGDI.PC_NOCOLLAPSE};
            INC (i);
          END;
        END;
      END;
    END;
  END InitDefaultPalette;

PROCEDURE MatchCurrentPalette () =
  VAR
    hwnd   := WinUser.GetDesktopWindow ();
    hdc    := WinUser.GetDC (hwnd);
    status : INTEGER;
  BEGIN
    defaultPalette.palVersion    := 16_300;   (* Windows version number *)
    defaultPalette.palNumEntries := 0;

    (* enumerate the solid pens to find the "real" colors that are available *)
    EVAL WinGDI.EnumObjects (hdc, WinGDI.OBJ_PEN, EnumColors, 0);

    status := WinUser.ReleaseDC (hwnd, hdc);
    <*ASSERT status # 0 *>

    IF defaultPalette.palNumEntries <= 0 THEN
      (* we didn't find any solid colors... *)
      InitDefaultPalette ();
    END;
  END MatchCurrentPalette;

<*CALLBACK*>
PROCEDURE EnumColors (a1: WinDef.LPVOID;  <*UNUSED*>a2: WinDef.LPARAM): Ctypes.int =
  VAR pen : WinGDI.LPLOGPEN := a1;  r,g,b: WinDef.BYTE;
  BEGIN
    IF (pen # NIL) AND (pen.lopnStyle = WinGDI.PS_SOLID) THEN
      WITH cnt = defaultPalette.palNumEntries,
           pe  = defaultPalette.palPalEntry     DO
        IF (cnt >= MaxPalEntries) THEN RETURN 0; (* bail out *) END;
        r := WinGDI.GetRValue (pen.lopnColor);
        g := WinGDI.GetGValue (pen.lopnColor);
        b := WinGDI.GetBValue (pen.lopnColor);
        INC (cnt);
        pe [cnt] := WinGDI.PALETTEENTRY {r, g, b, WinGDI.PC_NOCOLLAPSE};
      END;
    END;
    RETURN 1;
  END EnumColors;

PROCEDURE NumDeviceColors (): INTEGER =
  VAR
    hwnd   := WinUser.GetDesktopWindow ();
    hdc    := WinUser.GetDC (hwnd);
    cnt    := WinGDI.GetDeviceCaps (hdc, WinGDI.NUMCOLORS);
    status := WinUser.ReleaseDC (hwnd, hdc);
  BEGIN
    <* ASSERT status # 0 *>
    RETURN cnt;
  END NumDeviceColors;

BEGIN
END WinScrnColorMap.
