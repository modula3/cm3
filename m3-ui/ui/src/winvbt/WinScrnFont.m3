(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Jun 27 14:22:09 PDT 1995 by najork                   *)
(*       Created on Tue Jan 17 16:36:36 PST 1995 by najork                   *)


UNSAFE MODULE WinScrnFont;

IMPORT Ctypes, Fingerprint, FloatMode, Fmt, Font, Lex, M3toC, PaintPrivate, 
       Rect, Scan, ScrnFont, Text, WinDef, WinGDI, WinUser, Word;


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

  X Logigal Font Description    ScrnFont.Metrics field:  WinGDI.LOGFONT field:
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
  <* FATAL BadFontName *>
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
      IF n < 0 THEN 
        RETURN "*-" 
      ELSE 
        RETURN Fmt.Int(n) & "-" 
      END;
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


PROCEDURE Lookup (<* UNUSED *> self: Oracle; name: TEXT): ScrnFont.T 
    RAISES {ScrnFont.Failure} =
  BEGIN
    TRY
      WITH res = NameToScrnFont (name) DO
        IF res = NIL THEN
          RAISE ScrnFont.Failure;
        ELSE
          RETURN res;
        END;
      END;
    EXCEPT
      BadFontName => RAISE ScrnFont.Failure;
    END;
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
  Preferred = "-*-Arial-Normal-R-*-*-*-12-*-*-P-*-iso8859-ANSI";


PROCEDURE BuiltIn (self: Oracle; id: Font.Predefined): ScrnFont.T =
  BEGIN
    IF id # Font.BuiltIn.fnt THEN 
      RAISE FatalError 
    END;
    TRY
      (* 
       * Once "list" is implemented, we should allow for an array of 
       * preferred fonts.
       *)
      RETURN Lookup (self, Preferred);
    EXCEPT
    | ScrnFont.Failure =>
      RETURN NEW(ScrnFont.T, 
                 id := 0,
                 metrics := NEW(NullMetrics,
                                minBounds := ScrnFont.CharMetric{0,Rect.Empty},
                                maxBounds := ScrnFont.CharMetric{0,Rect.Empty},
                                firstChar := 0, 
                                lastChar := 0,
                                selfClearing := TRUE, 
                                charMetrics := NIL));
    END;
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

VAR
  FontNames : REF ARRAY OF TEXT;


PROCEDURE DetermineFontNames () =
  VAR
    er    : EnumRec;
    status: WinDef.BOOL;
  BEGIN
    er.ctr := 0;

    WITH hwnd = WinUser.GetDesktopWindow() DO
      er.hdc := WinUser.GetDC(hwnd);
      <* ASSERT er.hdc # NIL *>

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
      <* ASSERT status = 1 *>
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

  PROCEDURE ToRegistry (READONLY lf: WinGDI.LOGFONT): TEXT =
    BEGIN
      IF lf.lfCharSet = WinGDI.ANSI_CHARSET THEN
        RETURN "iso8859";
      ELSE
        RETURN "Unknown";
      END;
    END ToRegistry;

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


PROCEDURE NameToLogFont (name: TEXT): WinGDI.LOGFONT RAISES {BadFontName} =
  VAR
    parts: ARRAY [1..15] OF TEXT;
  BEGIN
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

  PROCEDURE ToSlant (READONLY lf: WinGDI.LOGFONT): ScrnFont.Slant =
    BEGIN
      IF lf.lfItalic = True THEN
        RETURN ScrnFont.Slant.Italic;
      ELSE
        RETURN ScrnFont.Slant.Roman;
      END;
    END ToSlant;

  PROCEDURE ToSpacing (READONLY lf: WinGDI.LOGFONT): ScrnFont.Spacing =
    BEGIN
      IF Word.And (lf.lfPitchAndFamily, WinGDI.FIXED_PITCH) # 0 THEN
        RETURN ScrnFont.Spacing.Monospaced
      ELSIF Word.And (lf.lfPitchAndFamily, WinGDI.VARIABLE_PITCH) # 0 THEN
        RETURN ScrnFont.Spacing.Proportional
      ELSE
        RETURN ScrnFont.Spacing.Any
      END
    END ToSpacing;

  PROCEDURE ToCharsetEncoding (READONLY lf: WinGDI.LOGFONT): TEXT =
    BEGIN
      CASE lf.lfCharSet OF
      | WinGDI.ANSI_CHARSET =>
        RETURN "ANSI";
      | WinGDI.UNICODE_CHARSET =>
        RETURN "UNICODE";
      | WinGDI.SYMBOL_CHARSET =>
        RETURN "SYMBOL";
      | WinGDI.SHIFTJIS_CHARSET =>
        RETURN "SHIFTJIS";
      | WinGDI.HANGEUL_CHARSET =>
        RETURN "HANGEUL";
      | WinGDI.CHINESEBIG5_CHARSET =>
        RETURN "CHINESEBIG5";
      | WinGDI.OEM_CHARSET =>
        RETURN "OEM";
      ELSE
        RETURN "Unknown";
      END;
    END ToCharsetEncoding;

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
        <* ASSERT status = True *>

        WITH first = tm.tmFirstChar, last = tm.tmLastChar DO
          abcs := NEW (REF ARRAY OF WinGDI.ABC, last - first + 1);
          status := WinGDI.GetCharABCWidths (hdc, first, last, ADR(abcs[0]));
          <* ASSERT status = True *>
        END;

        oldFont := WinGDI.SelectObject (hdc, oldFont);
        <* ASSERT oldFont = hfont *>
        status := WinUser.ReleaseDC (hwnd, hdc);
        <* ASSERT status = 1 *>
      END;
    END;

    WITH m = res.metrics DO
      m.family := M3toC.CopyStoT (LOOPHOLE (ADR (lf.lfFaceName), 
                                            Ctypes.char_star));
        (* In X, instances of "family" are "Times" or "Helvetica".
           In Windows, the closest counterpart is the "typeface". *)
      m.pointSize := lf.lfHeight;
        (* The Windows documentation is vague about point sizes (although it
           uses the term). From what I could make out, the point size of a 
           font is equivalent to the height of the font. *)
      m.slant := ToSlant (lf);
        (* X has 6 different "slant" codes ("Roman", "Italic", "Oblique",
           "Reverse Italic", "Reverse Oblique", "Other"). Trestle has those
           six codes plus a 7th ("Any"). It seems that Windows only 
           distinguishes between "Roman" and "Italic". *)
      m.weightName := ToWeight(lf);
        (* In Trestle and X, instances of "weight" name are "Bold", "DemiBold",
           and "Medium". Windows has the concept of weights, and predefined
           constants for some weights. *)
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
      m.spacing := ToSpacing (lf);
        (* The X term "spacing" and the Windows term "pitch" are roughly 
           synonymous. X knows three spacings ("Proportional", "Monospaced",
           and "CharCell"); Windows knows three pitches ("DEFAULT", "FIXED", 
           and "VARIABLE". Trestle does not care about spacings; they are used
           only for font matching. *)
      m.averageWidth := lf.lfWidth;
        (* X "average width" and Windows LOGFONT "width" seem to be pretty 
           much the same. *)
      m.charsetRegistry := "Unknown";
        (* In X, "charsetRegistry" identifies the registration authority 
           for the character set. There is no such concept in Windows. 
           Trestle doesn't actually care. *)
      m.charsetEncoding := ToCharsetEncoding(lf);
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
      cms := NEW (REF ARRAY OF ScrnFont.CharMetric, 
                  m.lastChar - m.firstChar + 1);
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
      m.selfClearing := NOT (m.rightKerning OR m.leftKerning);
        (* This is risky; we don't actually know anything about per-character
           ascent and descent ... *)

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




(*****************************************************************************)
(* Procedure MatchingNames                                                   *)
(*****************************************************************************)

EXCEPTION BadFontName;

PROCEDURE FanoutName (t: TEXT; VAR ts: ARRAY [1..15] OF TEXT) 
    RAISES {BadFontName} =
  VAR
    start := 0;
  BEGIN
    FOR i := 1 TO 14 DO
      WITH pos = Text.FindChar (t, '-', start) DO
        IF pos = -1 THEN
          RAISE BadFontName;
        END;
        ts[i] := Text.Sub (t, start, pos - start);
        start := pos + 1;
      END;
    END;
    ts[15] := Text.Sub (t, start, Text.Length(t) - start);
  END FanoutName;

PROCEDURE MatchingNames (a, b: TEXT): BOOLEAN RAISES {BadFontName} =

  (* This procedure is simplified. According to the Trestle 
     specification, it should also deal with "?" patterns. *)
  PROCEDURE PatMatch (a, b: TEXT): BOOLEAN =
    BEGIN
      RETURN Text.Equal (a, "*") OR Text.Equal (b, "*") OR Text.Equal (a, b);
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


PROCEDURE ToFamily (READONLY lf: WinGDI.LOGFONT): TEXT =
  BEGIN
    WITH string = LOOPHOLE (ADR (lf.lfFaceName), Ctypes.char_star),
         text   = M3toC.StoT (string),
         chars  = NEW (REF ARRAY OF CHAR, Text.Length (text)) DO
      Text.SetChars (chars^, text);
      FOR i := FIRST (chars^) TO LAST (chars^) DO
        IF chars[i] = '-' THEN
          chars[i] := '_';
        END;
      END;
      RETURN Text.FromChars (chars^);
    END;
  END ToFamily;
  
TYPE FaceName = ARRAY [0 .. WinGDI.LF_FACESIZE - 1] OF Ctypes.char;
     
PROCEDURE FromFamily (family: TEXT): FaceName =
  VAR
    res: FaceName;
  BEGIN
    WITH chars = NEW (REF ARRAY OF CHAR, Text.Length (family)) DO
      Text.SetChars (chars^, family);
      FOR i := FIRST (chars^) TO LAST (chars^) DO
        IF chars[i] = '_' THEN
          chars[i] := '-';
        END;
      END;
      WITH text = Text.FromChars (chars^),
           len  = Text.Length (text) DO
        FOR i := 0 TO MIN (len - 1, LAST(res)) DO
          res[i] := ORD (Text.GetChar (text, i));
        END;
        FOR i := len TO LAST (res) DO
          res[i] := ORD (' ');
        END;
      END;
    END;
    RETURN res;
  END FromFamily;        
      

PROCEDURE ToWeight (READONLY lf: WinGDI.LOGFONT): TEXT =
  BEGIN
    WITH w = lf.lfWeight DO
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
    END;
  END ToWeight;
  
PROCEDURE FromWeight (weight: TEXT): WinDef.LONG RAISES {BadFontName} =
  BEGIN
    IF    Text.Equal (weight, "Unknown"   ) THEN RETURN 0;
    ELSIF Text.Equal (weight, "Thin"      ) THEN RETURN 100;
    ELSIF Text.Equal (weight, "ExtraLight") THEN RETURN 200;
    ELSIF Text.Equal (weight, "Light"     ) THEN RETURN 300;
    ELSIF Text.Equal (weight, "Normal"    ) THEN RETURN 400;
    ELSIF Text.Equal (weight, "Medium"    ) THEN RETURN 500;
    ELSIF Text.Equal (weight, "SemiBold"  ) THEN RETURN 600;
    ELSIF Text.Equal (weight, "Bold"      ) THEN RETURN 700;
    ELSIF Text.Equal (weight, "ExtraBold" ) THEN RETURN 800;
    ELSIF Text.Equal (weight, "Heavy"     ) THEN RETURN 900;
    ELSE
      RAISE BadFontName;
    END;
  END FromWeight;


PROCEDURE ToSlant (READONLY lf: WinGDI.LOGFONT): TEXT =
  BEGIN
    IF lf.lfItalic = True THEN
      RETURN "I";
    ELSE
      RETURN "R";
    END;
  END ToSlant;

PROCEDURE FromSlant (slant: TEXT): WinDef.BYTE =
  BEGIN
    IF Text.Equal (slant, "I") THEN
      RETURN True
    ELSE
      RETURN False;
    END;
  END FromSlant;    
    

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
    IF Text.Equal (spacing, "M") THEN 
      RETURN WinGDI.FIXED_PITCH + WinGDI.FF_DONTCARE;
    ELSIF Text.Equal (spacing, "P") THEN
      RETURN WinGDI.VARIABLE_PITCH + WinGDI.FF_DONTCARE;
    ELSIF Text.Equal (spacing, "*") THEN
      RETURN WinGDI.DEFAULT_PITCH + WinGDI.FF_DONTCARE;
    ELSE
      RAISE BadFontName;
    END;
  END FromSpacing;


PROCEDURE ToPointSize (READONLY lf: WinGDI.LOGFONT): TEXT =
  BEGIN
    IF TRUE THEN (* Simplification; need to check if lf is a TrueType font *)
      RETURN "*";
    ELSE
      RETURN Fmt.Int (lf.lfHeight);
    END;
  END ToPointSize;

PROCEDURE FromPointSize (pointSize: TEXT): WinDef.LONG RAISES {BadFontName} =
  BEGIN
    IF Text.Equal (pointSize, "*") THEN 
      RETURN 0;
    ELSE
      TRY
        RETURN -ABS (Scan.Int (pointSize));
      EXCEPT
        Lex.Error, FloatMode.Trap => RAISE BadFontName;
      END;
    END;
  END FromPointSize;


PROCEDURE ToEncoding (READONLY lf: WinGDI.LOGFONT): TEXT =
  BEGIN
    CASE lf.lfCharSet OF
    | WinGDI.ANSI_CHARSET =>
      RETURN "ANSI";
    | WinGDI.UNICODE_CHARSET =>
      RETURN "UNICODE";
    | WinGDI.SYMBOL_CHARSET =>
      RETURN "SYMBOL";
    | WinGDI.SHIFTJIS_CHARSET =>
      RETURN "SHIFTJIS";
    | WinGDI.HANGEUL_CHARSET =>
      RETURN "HANGEUL";
    | WinGDI.CHINESEBIG5_CHARSET =>
      RETURN "CHINESEBIG5";
    | WinGDI.OEM_CHARSET =>
      RETURN "OEM";
    ELSE
      RETURN "Unknown";
    END;
  END ToEncoding;
  
PROCEDURE FromEncoding (encoding: TEXT): WinDef.BYTE RAISES {BadFontName} =
  BEGIN
    IF Text.Equal (encoding, "ANSI") THEN 
      RETURN WinGDI.ANSI_CHARSET
    ELSIF Text.Equal (encoding, "UNICODE") THEN 
      RETURN WinGDI.UNICODE_CHARSET
    ELSIF Text.Equal (encoding, "SYMBOL") THEN 
      RETURN WinGDI.SYMBOL_CHARSET
    ELSIF Text.Equal (encoding, "SHIFTJIS") THEN 
      RETURN WinGDI.SHIFTJIS_CHARSET
    ELSIF Text.Equal (encoding, "HANGEUL") THEN 
      RETURN WinGDI.HANGEUL_CHARSET
    ELSIF Text.Equal (encoding, "CHINESEBIG5") THEN 
      RETURN WinGDI.CHINESEBIG5_CHARSET
    ELSIF Text.Equal (encoding, "OEM") THEN 
      RETURN WinGDI.OEM_CHARSET
    ELSE
      RAISE BadFontName;
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
    IF Text.Equal (width, "*") THEN
      RETURN 0;
    ELSE
      TRY 
        RETURN Scan.Int (width);
      EXCEPT
        Lex.Error, FloatMode.Trap => RAISE BadFontName;
      END;
    END;
  END FromWidth;


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
