(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jun 27 14:22:09 PDT 1995 by najork                   *)
(*       Created on Tue Jan 17 16:36:36 PST 1995 by najork                   *)


UNSAFE MODULE WinScrnFont;

IMPORT ASCII, Ctypes, Fingerprint, FloatMode, Fmt, Font, Lex, PaintPrivate,
       Rect, Scan, ScrnFont, Text, WinDef, WinGDI, WinUser, Word;

IMPORT RTIO, IO; (* for debugging *)
CONST DEBUG = FALSE;

EXCEPTION FatalError; <* FATAL FatalError *>

(*

The WinScrnFont module implements Windows-specific subclasses to
screen-fonts and screen font oracles. The client can ask for all
available fonts that match a certain, and can request a specific
font. Unfortunately, the attributes used by Trestle to characterize a
font are modeled very closely after the corresponding X attributes,
and do not quite match the attributes Windows uses for characterizing
a font.  A second problem is that Windows has no function for looking
up a font that matches a pattern with "wildcards".

There are three formats for describing fonts:
  - X font description strings
  - Trestle ScrnFont.Metrics records
  - Windows WinGDI.LOGFONT records

An X font specification string has the following format:
  -fndry-fmly-wght-slant-sWdth-adstyl-pxlsz-ptsz-resx-resy-spc-avgWdth-rgstry-encdng

The following attributes are (more or less) common to X, Trestle, and Windows:

  X Logical Font Description    ScrnFont.Metrics field:  WinGDI.LOGFONT field:
  Foundry                         foundry                  -----
  Family                          family                   lfFaceName
  Weight                          weightName               lfWeight
  Slant                           slant                    lfItalic
  sWidth                          width                    -----
  adstyl                          -----                    -----
  Pixel Size                      pixelsize                -----
  Point Size                      pointSize                lfHeight
  Hor. Resolution                 hres                     -----
  Ver. Resolution                 vres                     -----
  Spacing                         spacing                  lfPitchAndFamily
  Avg. Width                      averageWidth             lfWidth
  CharSet Registry                charsetRegistry          -----
  CharSet Enconding               charsetEncoding          lfCharSet

There is a way to enumerate all installed Windows fonts, but this method does
not return all the available point sizes for a TrueType font (I don't know if
it simplifies anything else).

We adopt the following policies:
  (1) The following 7 attributes are used to map a font description string to a
      Windows font:
      Family, Weight, Slant, Point Size, Spacing, Width, and Character Set
  (2) At startup, we enumerate all available fonts and build up a list of
      font description strings. For all the X attributes that are unspecified
      in Windows world, we fill in a "*". In addition, if the font is a
      TrueType font, we set the Point Size to "*".
*)



PROCEDURE NewOracle (): ScrnFont.Oracle =
  BEGIN
    RETURN NEW (Oracle);
  END NewOracle;


TYPE
  Oracle = ScrnFont.Oracle BRANDED OBJECT
  OVERRIDES
    list    := List;
    match   := Match;
    lookup  := Lookup;
    builtIn := BuiltIn;
  END;


PROCEDURE List (<*UNUSED*> self      : Oracle;
                           pat       : TEXT;
                           maxResults: INTEGER): REF ARRAY OF TEXT =
  VAR
    res := NEW (REF ARRAY OF TEXT, maxResults);
    cnt := 0;
  BEGIN
    FOR i := FIRST (FontNames^) TO LAST (FontNames^) DO
      IF MatchingNames (pat, FontNames[i]) THEN
        res[cnt] := FontNames[i];
        INC (cnt);
        IF cnt > maxResults THEN
          EXIT;
        END;
      END;
    END;
    IF cnt > maxResults THEN
      RETURN res;
    ELSE
      WITH tmp = NEW (REF ARRAY OF TEXT, cnt) DO
        tmp^ := SUBARRAY (res^, 0, cnt);
        RETURN tmp;
      END;
    END;
  END List;

PROCEDURE ListFonts (pat       : TEXT;
                     maxResults: INTEGER): REF ARRAY OF TEXT =
  BEGIN
    RETURN List(NIL, pat, maxResults);
  END ListFonts;

(*
 * "Match" is almost an exact copy of the "XScrnFont.FontMatch" procedure.
 *)
PROCEDURE Match (self           : Oracle;
                 family         : TEXT;
                 pointSize      : INTEGER;
                 slant          : ScrnFont.Slant;
                 maxResults     : CARDINAL;
                 weightName     : TEXT;
                 version        : TEXT;
                 foundry        : TEXT;
                 width          : TEXT;
                 pixelsize      : INTEGER;
                 hres, vres     : INTEGER;
                 spacing        : ScrnFont.Spacing;
                 averageWidth   : INTEGER;
                 charsetRegistry: TEXT;
                 charsetEncoding: TEXT): REF ARRAY OF TEXT =

  PROCEDURE Num (n: INTEGER): TEXT =
    BEGIN
      IF n < 0 THEN RETURN "*-" ELSE RETURN Fmt.Int(n) & "-" END;
    END Num;

  VAR
    fname: TEXT;
  BEGIN
    IF Text.Length(version) # 0 THEN
      fname := "+" & version
    ELSE
      fname := ""
    END;
    fname := fname & "-" & foundry & "-" & family & "-" & weightName & "-";
    CASE slant OF
      ScrnFont.Slant.Roman => fname := fname & "R"
    | ScrnFont.Slant.Italic => fname := fname & "I"
    | ScrnFont.Slant.Oblique => fname := fname & "O"
    | ScrnFont.Slant.ReverseItalic => fname := fname & "RI"
    | ScrnFont.Slant.ReverseOblique => fname := fname & "RO"
    | ScrnFont.Slant.Other => fname := fname & "OT"
    | ScrnFont.Slant.Any => fname := fname & "*"
    END;
    fname := fname & "-" & width & "-*-" & Num(pixelsize) & Num(pointSize)
               & Num(hres) & Num(vres);
    CASE spacing OF
      ScrnFont.Spacing.Proportional => fname := fname & "P"
    | ScrnFont.Spacing.Monospaced => fname := fname & "M"
    | ScrnFont.Spacing.CharCell => fname := fname & "C"
    | ScrnFont.Spacing.Any => fname := fname & "*"
    END;
    fname := fname & "-" & Num(averageWidth) & charsetRegistry & "-"
               & charsetEncoding;
    RETURN List (self, fname, maxResults)
  END Match;

PROCEDURE Lookup (<*UNUSED*>self: Oracle; name: TEXT; <*UNUSED*>xft := TRUE): ScrnFont.T
    RAISES {ScrnFont.Failure} =
  VAR res: ScrnFont.T;
  BEGIN
    IF DEBUG THEN
      RTIO.PutText ("Lookup: ");
      RTIO.PutText (name);
      RTIO.PutText (" => ");
      RTIO.Flush ();
    END;

    TRY
      res := NameToScrnFont (name);
    EXCEPT BadFontName =>
      IF DEBUG THEN
        RTIO.PutText ("**BAD NAME** ");
        RTIO.Flush ();
      END;
      res := NIL;
    END;

    IF res = NIL THEN
      IF DEBUG THEN
        RTIO.PutText ("***FAILED***\r\n");
        RTIO.Flush ();
      END;
      RAISE ScrnFont.Failure;
    END;

    IF DEBUG THEN
      RTIO.PutAddr (LOOPHOLE (res, ADDRESS));
      RTIO.PutText ("\r\n");
      RTIO.Flush ();
    END;

    RETURN res;
  END Lookup;


(******************************************************************************
 *
 * "BuiltIn" returns a default screen font. The xvbt implementation goes
 * through an array of hardwired font patterns, determines if the X server
 * offers any of these fonts, takes the first match, converts it into a
 * "ScrnFont.T", and returns it. If there is no match, it raises a fatal
 * error. The font selection is protected by the "trsl" mutex.
 *
 * The Windows implementation first checks if one of the fonts from a list
 * of preferred fonts is available, and picks the first match. If there is
 * no match, it will pick any font. (Note that there is always at least one
 * font -- Windows must have some fonts to draw window frames and menu bars).
 * It then converts the Windows font into a "ScrnFont.T", and returns it.
 *
 *****************************************************************************)

CONST
  Preferred = ARRAY [0..3] OF TEXT {
    "-*-Courier New-Normal-R-*-*-*-120-*-*-P-*-iso8859-ANSI",
    "-*-Courier New-Normal-R-*-*-*-100-*-*-P-*-iso8859-ANSI",
    "-*-Arial-Normal-R-*-*-*-120-*-*-P-*-iso8859-ANSI",
    "-*-Arial-*-*-*-*-*-*-*-*-*-*-iso8859-ANSI"
  };

  (* was "-*-Arial-Normal-R-*-*-*-120-*-*-P-*-iso8859-ANSI" *)


PROCEDURE BuiltIn (self: Oracle; id: Font.Predefined): ScrnFont.T =
  BEGIN
    IF id # Font.BuiltIn.fnt THEN
      RAISE FatalError
    END;
    FOR i := 0 TO LAST(Preferred) DO
      TRY
        RETURN Lookup (self, Preferred[i]);
      EXCEPT
      | ScrnFont.Failure => (* skip *)
      END;
    END;
    RETURN NEW(ScrnFont.T, id := 0,
               metrics := NEW(NullMetrics,
                              minBounds := ScrnFont.CharMetric{0,Rect.Empty},
                              maxBounds := ScrnFont.CharMetric{0,Rect.Empty},
                              firstChar := 0,
                              lastChar := 0,
                              selfClearing := TRUE,
                              charMetrics := NIL));
  END BuiltIn;


CONST
  False = 0;
  True  = 1;


PROCEDURE FromFont (font: PaintPrivate.Font): WinDef.HFONT =
  BEGIN
    RETURN LOOPHOLE (font, WinDef.HFONT);
  END FromFont;


(*****************************************************************************)
(* DetermineFontNames                                                        *)
(*                                                                           *)
(* This procedure is called by Init. It enumerates all the available Windows *)
(* fonts, converts them into font names (font description strings), and      *)
(* stores these names in an array.                                           *)
(*****************************************************************************)

TYPE
  EnumRec = RECORD
    hdc: WinDef.HDC;
    ctr: INTEGER := 0;
  END;
  EnumRecPtr = UNTRACED REF EnumRec;

VAR (* CONST after initialization *)
  FontNames : REF ARRAY OF TEXT;

VAR (* CONST after initialization *)
  LogicalPixelsPerVertInch : INTEGER;
  FontScaleFactor          : REAL;
  (* Point sizes vs. a Windows font's "logical height":
       - X uses point sizes scaled by 10, for example 144 => 14.4 pts
       - Windows has some complicated mapping from a logical font height
         to the actual pixel size of the character cells.  See note #Q74299
         in the Visual C++ SDK Knowledge Base. *)


PROCEDURE DetermineFontNames () =
  VAR
    er    : EnumRec;
    status: WinDef.BOOL;
  BEGIN
    er.ctr := 0;

    WITH hwnd = WinUser.GetDesktopWindow() DO
      er.hdc := WinUser.GetDC(hwnd);
      <* ASSERT er.hdc # NIL *>

      (* get the logical pixel size for the display, so we can scale fonts later *)
      LogicalPixelsPerVertInch := WinGDI.GetDeviceCaps(er.hdc, WinGDI.LOGPIXELSY);
      FontScaleFactor := - FLOAT (LogicalPixelsPerVertInch) / 720.0;
      IF DEBUG THEN
        IO.Put("LPPVI: ");
        IO.PutInt(LogicalPixelsPerVertInch);
        IO.Put("\n");
        IO.Put("FontScaleFactor: ");
        IO.PutReal(FontScaleFactor);
        IO.Put("\n");
      END;
      (* First, count how many fonts are installed *)
      EVAL WinGDI.EnumFontFamilies(er.hdc,
                                   NIL,
                                   LOOPHOLE(CountFamProc, WinGDI.FONTENUMPROC),
                                   LOOPHOLE(ADR(er), WinDef.LPARAM));

      (* Create space for them *)
      FontNames := NEW (REF ARRAY OF TEXT, er.ctr);

      (* Reset the counter and fill in the fonts *)
      er.ctr := 0;
      EVAL WinGDI.EnumFontFamilies(er.hdc,
                                   NIL,
                                   LOOPHOLE (InitFamProc, WinGDI.FONTENUMPROC),
                                   LOOPHOLE (ADR(er), WinDef.LPARAM));

      (* release the desktop device context *)
      status := WinUser.ReleaseDC (hwnd, er.hdc);
      <* ASSERT status = True *>
    END;
  END DetermineFontNames;


<* CALLBACK *>
PROCEDURE InitFamProc (             lpelf : WinGDI.LPENUMLOGFONT;
                       <* UNUSED *> lpntm : WinGDI.LPNEWTEXTMETRIC;
                       <* UNUSED *> type  : Ctypes.int;
                                    lparam: WinDef.LPARAM): Ctypes.int =
  VAR
    erp := LOOPHOLE (lparam, EnumRecPtr);
  BEGIN
    EVAL WinGDI.EnumFontFamilies (erp.hdc,
                                  LOOPHOLE (ADR (lpelf.elfLogFont.lfFaceName),
                                            Ctypes.char_star),
                                  LOOPHOLE (InitFontProc, WinGDI.FONTENUMPROC),
                                  lparam);
    RETURN 1;
  END InitFamProc;


<* CALLBACK *>
PROCEDURE InitFontProc (             lpelf : WinGDI.LPENUMLOGFONT;
                        <* UNUSED *> lpntm : WinGDI.LPNEWTEXTMETRIC;
                                     type  : Ctypes.int;
                                     lparam: WinDef.LPARAM): Ctypes.int =
  VAR
    erp := LOOPHOLE (lparam, EnumRecPtr);
  BEGIN
    IF Word.And (type, WinGDI.TRUETYPE_FONTTYPE) # 0 THEN
      FontNames[erp.ctr] := LogFontToName (lpelf.elfLogFont);
      IF DEBUG THEN
        RTIO.PutText (FontNames[erp.ctr]);
        RTIO.PutText ("\r\n");
        RTIO.Flush ();
      END;
      INC (erp.ctr);
    END;
    RETURN 1;
  END InitFontProc;


<* CALLBACK *>
PROCEDURE CountFamProc (             lpelf : WinGDI.LPENUMLOGFONT;
                        <* UNUSED *> lpntm : WinGDI.LPNEWTEXTMETRIC;
                        <* UNUSED *> type  : Ctypes.int;
                                     lparam: WinDef.LPARAM): Ctypes.int =
  VAR
    erp := LOOPHOLE (lparam, EnumRecPtr);
  BEGIN
    EVAL WinGDI.EnumFontFamilies(erp.hdc,
                                 LOOPHOLE (ADR (lpelf.elfLogFont.lfFaceName),
                                           Ctypes.char_star),
                                 LOOPHOLE (CountFontProc, WinGDI.FONTENUMPROC),
                                 lparam);
    RETURN 1;
  END CountFamProc;


<* CALLBACK *>
PROCEDURE CountFontProc (<* UNUSED *> lpelf : WinGDI.LPENUMLOGFONT;
                         <* UNUSED *> lpntm : WinGDI.LPNEWTEXTMETRIC;
                                      type  : Ctypes.int;
                                      lparam: WinDef.LPARAM): Ctypes.int =
  VAR
    erp := LOOPHOLE (lparam, EnumRecPtr);
  BEGIN
    IF Word.And (type, WinGDI.TRUETYPE_FONTTYPE) # 0 THEN
      INC (erp.ctr);
    END;
    RETURN 1;
  END CountFontProc;


PROCEDURE LogFontToName (READONLY lf: WinGDI.LOGFONT): TEXT =
  BEGIN
    RETURN ""                     &   (* Version          *)
           "-*"                   &   (* Foundry          *)
           "-" & ToFamily (lf)    &   (* Family Name      *)
           "-" & ToWeight (lf)    &   (* Weight Name      *)
           "-" & ToSlant (lf)     &   (* Slant            *)
           "-*"                   &   (* Setwidth Name    *)
           "-*"                   &   (* Add Style Name   *)
           "-*"                   &   (* Pixel Size       *)
           "-" & ToPointSize (lf) &   (* Point Size       *)
           "-*"                   &   (* Resolution X     *)
           "-*"                   &   (* Resolution Y     *)
           "-" & ToSpacing (lf)   &   (* Spacing          *)
           "-" & ToWidth (lf)     &   (* Average Width    *)
           "-" & ToRegistry (lf)  &   (* Charset Registry *)
           "-" & ToEncoding (lf);     (* Charset Encoding *)
  END LogFontToName;

CONST
  Fixed = "-*-Courier New-Normal-R-*-*-*-120-*-*-M-*-iso8859-ANSI";

PROCEDURE NameToLogFont (name: TEXT): WinGDI.LOGFONT RAISES {BadFontName} =
  VAR
    parts: ARRAY [1..15] OF TEXT;
  BEGIN
    IF CIEqual (name, "fixed") THEN name := Fixed; END;
    FanoutName (name, parts);
    RETURN WinGDI.LOGFONT {lfHeight        := FromPointSize (parts[9]),
                           lfWidth         := FromWidth (parts[13]),
                           lfEscapement    := 0,
                           lfOrientation   := 0,
                           lfWeight        := FromWeight (parts[4]),
                           lfItalic        := FromSlant (parts[5]),
                           lfUnderline     := False,
                           lfStrikeOut     := False,
                           lfCharSet       := FromEncoding (parts[15]),
                           lfOutPrecision  := WinGDI.OUT_DEFAULT_PRECIS,
                           lfClipPrecision := WinGDI.CLIP_DEFAULT_PRECIS,
                           lfQuality       := WinGDI.DEFAULT_QUALITY,
                           lfPitchAndFamily:= FromSpacing (parts[12]),
                           lfFaceName      := FromFamily (parts[3])};
  END NameToLogFont;


PROCEDURE NameToScrnFont (name: TEXT): ScrnFont.T
    RAISES {BadFontName} =
  BEGIN
    RETURN LogFontToScrnFont (NameToLogFont (name));
  END NameToScrnFont;


PROCEDURE LogFontToScrnFont (READONLY lf: WinGDI.LOGFONT): ScrnFont.T =

  VAR
    hfont   := WinGDI.CreateFontIndirect (ADR(lf));
    res := NEW (ScrnFont.T,
                id := LOOPHOLE (hfont, INTEGER),
                metrics := NEW (NullMetrics));
    tm   : WinGDI.NEWTEXTMETRIC;  (* superset of TEXTMETRIC *)
    abcs : REF ARRAY OF WinGDI.ABC;
    cms  : REF ARRAY OF ScrnFont.CharMetric;
  BEGIN
    IF hfont = NIL THEN
      RETURN NIL;
    END;


    (* Get the TEXTMETRIC or NEWTEXTMETRIC record for the font *)
    VAR
      hdc    : WinDef.HDC;
      oldFont: WinDef.HFONT;
      status : Ctypes.int;
    BEGIN
      WITH hwnd = WinUser.GetDesktopWindow() DO
        hdc := WinUser.GetDC(hwnd);
        <* ASSERT hdc # NIL *>
        oldFont := WinGDI.SelectObject (hdc, hfont);
        <* ASSERT oldFont # NIL *>
        status := WinGDI.GetTextMetrics (hdc, LOOPHOLE (ADR(tm),
                                                        WinGDI.LPTEXTMETRIC));
        <* ASSERT status # False *>

        WITH first = tm.tmFirstChar, last = tm.tmLastChar DO
          abcs := NEW (REF ARRAY OF WinGDI.ABC, last - first + 1);
          status := WinGDI.GetCharABCWidths (hdc, first, last, ADR(abcs[0]));
          IF status = False THEN GetFakeABCs (hdc, first, last, abcs); END;
        END;

        oldFont := WinGDI.SelectObject (hdc, oldFont);
        <* ASSERT oldFont = hfont *>
        status := WinUser.ReleaseDC (hwnd, hdc);
        <* ASSERT status = True *>
      END;
    END;

    WITH m = res.metrics DO
      m.family := ToFamily (lf);
      m.pointSize := HeightToXPoints (lf.lfHeight,tm.tmHeight,tm.tmInternalLeading);
      m.slant := ToScrnFontSlant (lf);
      m.weightName := ToWeight(lf);
      m.version := "";
        (* "version" was intended to indicate the version of the "X Logical
           Font Description Conventions". A blank is ok here. *)
      m.foundry := "Windows";
        (* In X, the "foundry" indicates the manufacturer of a font (e.g.
           "Adobe" or "DEC"). There is no foundry field in a Windows "LOGFONT"
           record (although there is a "Vendor ID" in the "EXTLOGFONT" record).
           Trestle doesn't actually care about the value of foundry. *)
      m.width := "Unknown";
        (* In X, "width" can have values such as "Narrow" or "Condensed".
           Windows "LOGFONT" structures don't have anything similar.
           Trestle doesn't actually seem to care. *)
      m.pixelsize := 0;
      m.hres := 0;
      m.vres := 0;
        (* In X, the pixel size, horizontal resolution, and vertical resolution
           of a font are known. This is not true for Windows logical fonts.
           Trestle does not actually care. *)
      m.spacing := ToScrnFontSpacing (lf);
      m.averageWidth := lf.lfWidth;
        (* X "average width" and Windows LOGFONT "width" seem to be pretty
           much the same. *)
      m.charsetRegistry := "Unknown";
        (* In X, "charsetRegistry" identifies the registration authority
           for the character set. There is no such concept in Windows.
           Trestle doesn't actually care. *)
      m.charsetEncoding := ToEncoding(lf);
        (* In X, "charsetEncoding" is a text property defined by the authority
           that issued the font. Trestle does not care about the content; it is
           used only for font matching. We use it to encode the LOGFONT
           "lfCharSet" field. *)
      m.isAscii := lf.lfCharSet = WinGDI.ANSI_CHARSET;
        (* True if the character set is the aka ANSI, aka ISO8859 character
           set. "isAscii" is actually a misnomor, ASCII is a 7-bit code,
           whereas ANSI and ISO8859 are 8-bit codes. *)
      m.firstChar   := tm.tmFirstChar;
      m.lastChar    := tm.tmLastChar;
      m.defaultChar := tm.tmDefaultChar;
      m.ascent      := tm.tmAscent;
      m.descent     := tm.tmDescent;

      (* Fill in the character metrics. *)
      cms := NEW (REF ARRAY OF ScrnFont.CharMetric, m.lastChar - m.firstChar + 1);
      FOR i := 0 TO LAST (abcs^) DO
        ToCharMetric (abcs[i], m, cms[i]);
      END;

      (* Compute the meet and the join of the CharMetric bounding boxes *)
      m.minBounds := cms[0];
      m.maxBounds := cms[0];
      FOR i := 1 TO LAST (cms^) DO
        MinMaxMetric (cms[i], m.minBounds, m.maxBounds);
      END;

      (* Determine kerning and self-clearing property *)
      m.rightKerning := FALSE;
      m.leftKerning := FALSE;
      FOR i := 0 TO LAST(cms^) DO
        WITH bd = cms[i], bb = bd.boundingBox DO
          IF bd.printWidth >= 0 THEN
            m.rightKerning := m.rightKerning OR bb.east > bd.printWidth;
            m.leftKerning  := m.leftKerning  OR bb.west < 0;
          ELSE
            m.rightKerning := m.rightKerning OR bb.east > 0;
            m.leftKerning  := m.leftKerning  OR bb.west < bd.printWidth;
          END;
        END;
      END;
      m.selfClearing := FALSE;
      (* WinPaint.TextCom always uses a transparent background... *)
      (*********
      m.selfClearing := NOT (m.rightKerning OR m.leftKerning);
      (* This is risky; we don't actually know anything about per-character
         ascent and descent ... *)
      **********)

      (* Save the char metrics array if it contains any non-trivial data. *)
      IF m.minBounds = m.maxBounds THEN
        m.charMetrics := NIL;
      ELSE
        m.charMetrics := cms;
      END;

      m.fprint := Fingerprint.Zero;
        (* The fingerprint is used only by "JoinScreen.MungeBatch".
           I suspect that it is used only when two trestles watch
           the same VBT, in other words, in "Shared Trestle".
           If this is the case, there is no need for fingerprinting
           in Windows world. *)

    END;
    RETURN res;
  END LogFontToScrnFont;

(****  Seems to alway return (0,0,0) for non-TrueType fonts...
PROCEDURE GetFakeABCs (hdc: WinDef.HDC;  first, last: INTEGER;
                       abcs: REF ARRAY OF WinGDI.ABC) =
  VAR widths := NEW (REF ARRAY OF WinGDI.ABCFLOAT, last - first + 1);
  BEGIN
    EVAL WinGDI.GetCharWidthFloat (hdc, first, last, ADR (widths[0]));
    FOR i := FIRST (widths^) TO LAST (widths^) DO
      WITH x = abcs[i], y = widths[i] DO
        x.abcA := ROUND (y.abcfA);
        x.abcB := ROUND (y.abcfA + y.abcfB) - x.abcA;
        x.abcC := ROUND (y.abcfA + y.abcfB + y.abcfC) - x.abcA - x.abcB;
      END;
    END;
  END GetFakeABCs;
******)

PROCEDURE GetFakeABCs (hdc: WinDef.HDC;  first, last: INTEGER;
                       abcs: REF ARRAY OF WinGDI.ABC) =
  VAR widths := NEW (REF ARRAY OF Ctypes.int, last - first + 1);
  BEGIN
    EVAL WinGDI.GetCharWidth (hdc, first, last, ADR (widths[0]));
    FOR i := FIRST (widths^) TO LAST (widths^) DO
      WITH x = abcs[i] DO
        x.abcA := 0;
        x.abcB := widths[i];
        x.abcC := 0;
      END;
    END;
  END GetFakeABCs;


(*****************************************************************************)
(* Procedure MatchingNames                                                   *)
(*****************************************************************************)

EXCEPTION BadFontName;

PROCEDURE FanoutName (t: TEXT; VAR ts: ARRAY [1..15] OF TEXT) =
  VAR start := 0;  next := 1;  dash: INTEGER;
  BEGIN
    WHILE (next < 15) DO
      dash := Text.FindChar (t, '-', start);
      IF (dash < 0) THEN EXIT; END;
      ts[next] := Text.Sub (t, start, dash - start);  INC (next);
      start := dash + 1;
    END;

    (* pretend the last dash is at the end of the string *)
    dash := Text.Length (t);
    IF (dash > start) THEN
      ts[next] := Text.Sub (t, start, dash - start); INC (next);
    END;

    (* fill in the rest of the string with "don't cares". *)
    WHILE (next <= 15) DO  ts[next] := "*";  INC (next);  END;
  END FanoutName;

PROCEDURE MatchingNames (a, b: TEXT): BOOLEAN =

  (* This procedure is simplified. According to the Trestle
     specification, it should also deal with "?" patterns. *)
  PROCEDURE PatMatch (a, b: TEXT): BOOLEAN =
    BEGIN
      RETURN CIEqual (a, "*") OR CIEqual (b, "*") OR CIEqual (a, b);
    END PatMatch;

  VAR
    as, bs : ARRAY [1..15] OF TEXT;
  BEGIN
    FanoutName (a, as);
    FanoutName (b, bs);
    FOR i := 1 TO 15 DO
      IF NOT PatMatch (as[i], bs[i]) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END MatchingNames;


(*****************************************************************************)
(* Conversion Functions                                                      *)
(*****************************************************************************)

PROCEDURE ToRegistry (READONLY lf: WinGDI.LOGFONT): TEXT =
  CONST Map = ARRAY BOOLEAN OF TEXT {"Unknown", "iso8859"};
  BEGIN
    RETURN Map [lf.lfCharSet = WinGDI.ANSI_CHARSET];
  END ToRegistry;

(* In X, instances of "family" are "Times" or "Helvetica".
   In Windows, the closest counterpart is the "typeface". *)

TYPE
  FaceName = ARRAY [0 .. WinGDI.LF_FACESIZE - 1] OF Ctypes.char;
  FaceChars = ARRAY [FIRST (FaceName) .. LAST (FaceName)] OF CHAR;

PROCEDURE ToFamily (READONLY lf: WinGDI.LOGFONT): TEXT =
  VAR buf: FaceChars;  ch: INTEGER;  len := 0;
  BEGIN
    WHILE (len < MIN (NUMBER (buf), NUMBER (lf.lfFaceName))) DO
      ch := MAX (ORD (FIRST (CHAR)), MIN (lf.lfFaceName[len], ORD (LAST (CHAR))));
      IF (ch = 0) THEN EXIT; END;
      IF (ch = ORD ('-')) THEN ch := ORD ('_'); END;
      buf[len] := VAL (ch, CHAR);
      INC (len);
    END;
    WHILE (len > 0) AND (buf [len-1] = ' ') DO DEC (len); END;
    RETURN Text.FromChars (SUBARRAY (buf, 0, len));
  END ToFamily;

PROCEDURE FromFamily (family: TEXT): FaceName =
  VAR
    len := MIN (Text.Length (family), NUMBER (buf));
    res : FaceName;
    buf : FaceChars;
    ch  : CHAR;
  BEGIN
    Text.SetChars (buf, family);
    FOR i := 0 TO len-1 DO
      ch := buf[i];
      IF ch = '_' THEN ch := '-'; END;
      res [i] := ORD (ch);
    END;
    FOR i := len TO LAST (res) DO res[i] := 0; END;
    res [LAST(res)] := 0;
    RETURN res;
  END FromFamily;

(* In Trestle and X, instances of "weight" name are "Bold", "DemiBold",
   and "Medium". Windows has the concept of weights, and predefined
   constants for some weights. *)

PROCEDURE ToWeight (READONLY lf: WinGDI.LOGFONT): TEXT =
  VAR w := lf.lfWeight;
  BEGIN
    IF    w = 0   THEN RETURN "Unknown"
    ELSIF w < 150 THEN RETURN "Thin"
    ELSIF w < 250 THEN RETURN "ExtraLight"
    ELSIF w < 350 THEN RETURN "Light"
    ELSIF w < 450 THEN RETURN "Normal"
    ELSIF w < 550 THEN RETURN "Medium"
    ELSIF w < 650 THEN RETURN "SemiBold"
    ELSIF w < 750 THEN RETURN "Bold"
    ELSIF w < 850 THEN RETURN "ExtraBold"
    ELSE               RETURN "Heavy"
    END;
  END ToWeight;

PROCEDURE FromWeight (weight: TEXT): WinDef.LONG RAISES {BadFontName} =
  BEGIN
    IF    CIEqual (weight, "*"         ) THEN RETURN 400;
    ELSIF CIEqual (weight, "Normal"    ) THEN RETURN 400;
    ELSIF CIEqual (weight, "Bold"      ) THEN RETURN 700;
    ELSIF CIEqual (weight, "Medium"    ) THEN RETURN 500;
    ELSIF CIEqual (weight, "Unknown"   ) THEN RETURN 0;
    ELSIF CIEqual (weight, "Thin"      ) THEN RETURN 100;
    ELSIF CIEqual (weight, "ExtraLight") THEN RETURN 200;
    ELSIF CIEqual (weight, "Light"     ) THEN RETURN 300;
    ELSIF CIEqual (weight, "SemiBold"  ) THEN RETURN 600;
    ELSIF CIEqual (weight, "ExtraBold" ) THEN RETURN 800;
    ELSIF CIEqual (weight, "Heavy"     ) THEN RETURN 900;
    ELSE BadName ("weight", weight); RETURN 400;
    END;
  END FromWeight;

(* X has 6 different "slant" codes ("Roman", "Italic", "Oblique",
   "Reverse Italic", "Reverse Oblique", "Other"). Trestle has those
   six codes plus a 7th ("Any"). It seems that Windows only
   distinguishes between "Roman" and "Italic". *)

PROCEDURE ToScrnFontSlant (READONLY lf: WinGDI.LOGFONT): ScrnFont.Slant =
  TYPE SL = ScrnFont.Slant;
  CONST Map = ARRAY BOOLEAN OF SL { SL.Roman, SL.Italic };
  BEGIN
    RETURN Map [lf.lfItalic = True];
  END ToScrnFontSlant;

PROCEDURE ToSlant (READONLY lf: WinGDI.LOGFONT): TEXT =
  CONST Map = ARRAY BOOLEAN OF TEXT { "R", "I" };
  BEGIN
    RETURN Map [lf.lfItalic = True];
  END ToSlant;

PROCEDURE FromSlant (slant: TEXT): WinDef.BYTE =
  CONST Map = ARRAY BOOLEAN OF WinDef.BYTE { False, True };
  BEGIN
    RETURN Map [CIEqual (slant, "I")];
  END FromSlant;

(* The X term "spacing" and the Windows term "pitch" are roughly
   synonymous. X knows three spacings ("Proportional", "Monospaced",
   and "CharCell"); Windows knows three pitches ("DEFAULT", "FIXED",
   and "VARIABLE". Trestle does not care about spacings; they are used
   only for font matching. *)

PROCEDURE ToScrnFontSpacing (READONLY lf: WinGDI.LOGFONT): ScrnFont.Spacing =
  BEGIN
    IF Word.And (lf.lfPitchAndFamily, WinGDI.FIXED_PITCH) # 0 THEN
      RETURN ScrnFont.Spacing.Monospaced
    ELSIF Word.And (lf.lfPitchAndFamily, WinGDI.VARIABLE_PITCH) # 0 THEN
      RETURN ScrnFont.Spacing.Proportional
    ELSE
      RETURN ScrnFont.Spacing.Any
    END
  END ToScrnFontSpacing;

PROCEDURE ToSpacing (READONLY lf: WinGDI.LOGFONT): TEXT =
  BEGIN
    IF Word.And (lf.lfPitchAndFamily, WinGDI.FIXED_PITCH) # 0 THEN
      RETURN "M";
    ELSIF Word.And (lf.lfPitchAndFamily, WinGDI.VARIABLE_PITCH) # 0 THEN
      RETURN "P";
    ELSE
      RETURN "*";
    END;
  END ToSpacing;

PROCEDURE FromSpacing (spacing: TEXT): WinDef.BYTE RAISES {BadFontName} =
  BEGIN
    IF CIEqual (spacing, "M") THEN
      RETURN WinGDI.FIXED_PITCH + WinGDI.FF_DONTCARE;
    ELSIF CIEqual (spacing, "P") THEN
      RETURN WinGDI.VARIABLE_PITCH + WinGDI.FF_DONTCARE;
    ELSIF CIEqual (spacing, "*") THEN
      RETURN WinGDI.DEFAULT_PITCH + WinGDI.FF_DONTCARE;
    ELSE
      BadName ("spacing", spacing);
      RETURN 0;
    END;
  END FromSpacing;

PROCEDURE ToCharMetric (abc       : WinGDI.ABC;
                        READONLY m: ScrnFont.Metrics;
                        VAR cm    : ScrnFont.CharMetric) =
  BEGIN
    cm.printWidth := abc.abcA + abc.abcB + abc.abcC;
    WITH bb = cm.boundingBox DO
      bb.west  := abc.abcA;
      bb.east  := abc.abcA + abc.abcB;
      bb.north := -m.ascent;
      bb.south := m.descent;
      IF bb.west >= bb.east OR bb.north >= bb.south THEN
        bb := Rect.Empty;
      END;
    END;
  END ToCharMetric;

PROCEDURE MinMaxMetric (READONLY cm  : ScrnFont.CharMetric;
                        VAR min, max : ScrnFont.CharMetric) =
  BEGIN
    min.printWidth := MIN (min.printWidth, cm.printWidth);
    max.printWidth := MAX (max.printWidth, cm.printWidth);
    min.boundingBox.west := MAX (min.boundingBox.west, cm.boundingBox.west);
    max.boundingBox.west := MIN (max.boundingBox.west, cm.boundingBox.west);
    min.boundingBox.east := MIN (min.boundingBox.east, cm.boundingBox.east);
    max.boundingBox.east := MAX (max.boundingBox.east, cm.boundingBox.east);
  END MinMaxMetric;

PROCEDURE HeightToXPoints (<*UNUSED*> logicalHeight: INTEGER;
                           fontHeight, internalLeading: INTEGER): INTEGER =
  BEGIN
    RETURN ABS (ROUND (FLOAT (fontHeight - internalLeading) / FontScaleFactor));
  END HeightToXPoints;

PROCEDURE XPointsToHeight (pointSize: INTEGER): INTEGER =
  BEGIN
    WITH pts =  ROUND (FLOAT (pointSize) * FontScaleFactor) DO
      IF pointSize > 30 THEN
        RETURN pts;
      ELSE
        RETURN pts * 10;
      END;
    END;
  END XPointsToHeight;

PROCEDURE ToPointSize (<*UNUSED*> READONLY lf: WinGDI.LOGFONT): TEXT =
  BEGIN
    RETURN "*"; (* Simplification; need to check if lf is a TrueType font *)

    (*** otherwise, something like this might work:
    RETURN Fmt.Int (HeightToXPoints (lf.lfHeight, lf.lfHeight, lf.lfHeight DIV 7));
    ***)
  END ToPointSize;

PROCEDURE FromPointSize (pointSize: TEXT): WinDef.LONG RAISES {BadFontName} =
  BEGIN
    IF CIEqual (pointSize, "*") THEN RETURN 0; END;
    RETURN XPointsToHeight (ToInt ("pointSize", pointSize));
  END FromPointSize;

PROCEDURE ToEncoding (READONLY lf: WinGDI.LOGFONT): TEXT =
  BEGIN
    CASE lf.lfCharSet OF
    | WinGDI.ANSI_CHARSET        =>  RETURN "ANSI";
    | WinGDI.UNICODE_CHARSET     =>  RETURN "UNICODE";
    | WinGDI.SYMBOL_CHARSET      =>  RETURN "SYMBOL";
    | WinGDI.SHIFTJIS_CHARSET    =>  RETURN "SHIFTJIS";
    | WinGDI.HANGEUL_CHARSET     =>  RETURN "HANGEUL";
    | WinGDI.CHINESEBIG5_CHARSET =>  RETURN "CHINESEBIG5";
    | WinGDI.OEM_CHARSET         =>  RETURN "OEM";
    ELSE                             RETURN "Unknown";
    END;
  END ToEncoding;

PROCEDURE FromEncoding (encoding: TEXT): WinDef.BYTE RAISES {BadFontName} =
  BEGIN
    IF   CIEqual (encoding, "*")
      OR CIEqual (encoding, "ANSI")
      OR CIEqual (encoding, "1") THEN
      RETURN WinGDI.ANSI_CHARSET
    ELSIF CIEqual (encoding, "UNICODE") THEN
      RETURN WinGDI.UNICODE_CHARSET
    ELSIF CIEqual (encoding, "SYMBOL") THEN
      RETURN WinGDI.SYMBOL_CHARSET
    ELSIF CIEqual (encoding, "SHIFTJIS") THEN
      RETURN WinGDI.SHIFTJIS_CHARSET
    ELSIF CIEqual (encoding, "HANGEUL") THEN
      RETURN WinGDI.HANGEUL_CHARSET
    ELSIF CIEqual (encoding, "CHINESEBIG5") THEN
      RETURN WinGDI.CHINESEBIG5_CHARSET
    ELSIF CIEqual (encoding, "OEM") THEN
      RETURN WinGDI.OEM_CHARSET
    ELSE
      BadName ("encoding", encoding);
      RETURN WinGDI.ANSI_CHARSET;
    END;
  END FromEncoding;


PROCEDURE ToWidth (READONLY lf: WinGDI.LOGFONT): TEXT =
  BEGIN
    IF lf.lfWidth = 0 THEN
      RETURN "*";
    ELSE
      RETURN Fmt.Int (lf.lfWidth);
    END;
  END ToWidth;

PROCEDURE FromWidth (width: TEXT): WinDef.LONG RAISES {BadFontName} =
  BEGIN
    IF CIEqual (width, "*") THEN RETURN 0; END;
    RETURN ToInt ("width", width);
  END FromWidth;

PROCEDURE ToInt (tag, val: TEXT): WinDef.LONG RAISES {BadFontName} =
  BEGIN
    TRY
      RETURN Scan.Int (val);
    EXCEPT Lex.Error, FloatMode.Trap =>
      BadName (tag, val);
      RETURN 0;
    END;
  END ToInt;

PROCEDURE BadName (tag, value: TEXT) RAISES {BadFontName} =
  BEGIN
    IF DEBUG THEN
      RTIO.PutText ("(Bad font name: ");
      RTIO.PutText (tag);
      RTIO.PutText (" = ");
      RTIO.PutText (value);
      RTIO.PutText (")");
      RTIO.Flush ();
    END;
    RAISE BadFontName;
  END BadName;

PROCEDURE CIEqual (a, b: TEXT): BOOLEAN =
  (* Case-insensitive TEXT comparisons *)
  VAR
    len1 := Text.Length (a);
    len2 := Text.Length (b);
    c1, c2: CHAR;
    b1, b2: ARRAY [0..63] OF CHAR;
  BEGIN
    IF (len1 # len2) THEN RETURN FALSE; END;
    len2 := 0;
    WHILE (len2 < len1) DO
      Text.SetChars (b1, a, len2);
      Text.SetChars (b2, b, len2);
      FOR i := 0 TO MIN (len1 - len2, NUMBER (b1))-1 DO
        c1 := ASCII.Upper [b1[i]];
        c2 := ASCII.Upper [b2[i]];
        IF (c1 # c2) THEN RETURN FALSE; END
      END;
      INC (len2, NUMBER (b1));
    END;
    RETURN TRUE;
  END CIEqual;


(*****************************************************************************)

TYPE
  NullMetrics = ScrnFont.Metrics BRANDED OBJECT
  OVERRIDES
    intProp  := NullIntProp;
    textProp := NullTextProp;
  END;

(*-----------------------------------------------------------------------------
   The spec in ScrnFont.i3 states:

       The method call "m.intProp(nm)" returns the integer value of the
       font attribute named "nm", or raises "Failure" if this attribute is
       not defined for "m".  The method call "m.intProp(nm, ORD(ch))"
       returns the integer value of the font attribute named "nm" for the
       character "ch", or raises "Failure" if this attribute is not defined
       for "(m, ch)".  The "textProp" method is similar.

   The X implementation (XScrnFont.NullIntProp and XScrnFont.NullTextProp),
   however, always raises "Failure". For now, we do the same ...

-----------------------------------------------------------------------------*)


PROCEDURE NullIntProp (<*UNUSED*> self: NullMetrics;
                       <*UNUSED*> name: TEXT;
                       <*UNUSED*> ch  : INTEGER): INTEGER
    RAISES {ScrnFont.Failure} =
  BEGIN
    RAISE ScrnFont.Failure
  END NullIntProp;


PROCEDURE NullTextProp (<*UNUSED*> self: NullMetrics;
                        <*UNUSED*> name: TEXT;
                        <*UNUSED*> ch  : INTEGER): TEXT
    RAISES {ScrnFont.Failure} =
  BEGIN
    RAISE ScrnFont.Failure
  END NullTextProp;


BEGIN
  DetermineFontNames();
END WinScrnFont.
