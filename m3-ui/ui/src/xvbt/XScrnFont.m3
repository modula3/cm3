(* Copyright (C) 1992, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* by Steve Glassman, Mark Manasse and Greg Nelson          *)
(* Last modified on Mon Jun 23 22:05:10 PDT 1997 by heydon  *)
(*      modified on Tue Jan 31 10:09:55 PST 1995 by kalsow  *)
(*      modified on Fri May 20 11:45:02 PDT 1994 by msm     *)
(*      modified on Mon Nov 22 14:00:08 PST 1993 by steveg  *)
(*      modified on Fri May  7 17:28:54 PDT 1993 by mjordan *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller  *)
<*PRAGMA LL*>

UNSAFE MODULE XScrnFont;


IMPORT Axis, Ctypes, Fmt, Font, M3toC, Palette, Rect, ScreenType, ScrnFont,
       Text, TrestleComm, X, XClient, XScreenType, XScrnTpRep, TrestleOnX,
       Fingerprint;

TYPE
  DeepFontOracle =
    ScrnFont.Oracle OBJECT
      st: XScreenType.T;
    METHODS
      init (st: XScreenType.T): DeepFontOracle := DeepInitFontOracle;
      (* LL = st.trsl *)
    OVERRIDES
      list    := DeepFontList;
      match   := DeepFontMatch;
      lookup  := DeepFontLookup;
      builtIn := DeepFontBuiltIn
    END;
  FontOracle =
    ScrnFont.Oracle OBJECT
      st: XScreenType.T;
      familyAtm, pointSizeAtm, slantAtm, weightNameAtm, foundryAtm,
        widthAtm, pixelSizeAtm, resXAtm, resYAtm, spacingAtm, aveWidthAtm,
        registryAtm, encodingAtm: X.Atom;
      slants  : ARRAY [0 .. 5] OF X.Atom;
      spacings: ARRAY [0 .. 2] OF X.Atom;
    METHODS
      init (st: XScreenType.T): FontOracle RAISES {TrestleComm.Failure}
        := InitFontOracle;
      (* LL = st.trsl *)
    OVERRIDES
      list    := FontList;
      match   := FontMatch;
      lookup  := FontLookup;
      builtIn := FontBuiltIn
    END;
  XFont = ScrnFont.T;

PROCEDURE NewOracle (scrn: XScreenType.T; depthOne := FALSE): ScrnFont.Oracle
  RAISES {TrestleComm.Failure} =
  BEGIN
    IF depthOne THEN
      RETURN NEW(FontOracle).init(scrn);
    ELSE
      RETURN NEW(DeepFontOracle).init(scrn);
    END;
  END NewOracle;

PROCEDURE DeepFontMatch (orc            : DeepFontOracle;
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
                         charsetEncoding: TEXT              ):
  REF ARRAY OF TEXT RAISES {TrestleComm.Failure} =
  BEGIN
    RETURN orc.st.bits.font.match(
             family, pointSize, slant, maxResults, weightName, version,
             foundry, width, pixelsize, hres, vres, spacing, averageWidth,
             charsetRegistry, charsetEncoding)
  END DeepFontMatch;

PROCEDURE DeepFontList (orc: DeepFontOracle; pat: TEXT; maxResults: INTEGER):
  REF ARRAY OF TEXT RAISES {TrestleComm.Failure} =
  BEGIN
    RETURN orc.st.bits.font.list(pat, maxResults)
  END DeepFontList;

PROCEDURE FontMatch (orc            : FontOracle;
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
                     charsetEncoding: TEXT              ):
  REF ARRAY OF TEXT RAISES {TrestleComm.Failure} =
  VAR fname: TEXT;
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
               & ResNum(hres, orc.st.res[Axis.T.Hor])
               & ResNum(vres, orc.st.res[Axis.T.Ver]);
    CASE spacing OF
      ScrnFont.Spacing.Proportional => fname := fname & "P"
    | ScrnFont.Spacing.Monospaced => fname := fname & "M"
    | ScrnFont.Spacing.CharCell => fname := fname & "C"
    | ScrnFont.Spacing.Any => fname := fname & "*"
    END;
    fname := fname & "-" & Num(averageWidth) & charsetRegistry & "-"
               & charsetEncoding;
    RETURN orc.list(fname, maxResults)
  END FontMatch;

PROCEDURE FontList (orc: FontOracle; pat: TEXT; maxResults: INTEGER):
  REF ARRAY OF TEXT RAISES {TrestleComm.Failure} =
  VAR s: Ctypes.char_star;
  BEGIN
    TRY
      TrestleOnX.Enter(orc.st.trsl);
      TRY
        s := M3toC.SharedTtoS(pat);
        VAR
          xcount: Ctypes.int;
          fonts := X.XListFonts(orc.st.trsl.dpy, s, MIN(maxResults, 32767),
                                ADR(xcount));
          count: INTEGER           := xcount;
          fp                       := fonts;
          res  : REF ARRAY OF TEXT;
        BEGIN
          M3toC.FreeSharedS(pat, s);
          IF fonts = NIL THEN RETURN NIL END;
          res := NEW(REF ARRAY OF TEXT, count);
          FOR i := 0 TO count - 1 DO
            res[i] := M3toC.CopyStoT(fp^);
            fp := fp + ADRSIZE(Ctypes.char_star)
          END;
          X.XFreeFontNames(fonts);
          RETURN res
        END
      FINALLY
        TrestleOnX.Exit(orc.st.trsl)
      END
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END FontList;

PROCEDURE Num (n: INTEGER): TEXT =
  BEGIN
    IF n < 0 THEN RETURN "*-" ELSE RETURN Fmt.Int(n) & "-" END
  END Num;

PROCEDURE ResNum (n: INTEGER; res: REAL): TEXT =
  BEGIN
    (* Gross hack to deal with the fact that all available fonts for X are
       either scaled for 75 pixel per inch or 100 pixel per inch
       displays *)
    IF n = -2 THEN
      RETURN Num(ROUND(res * 25.4 / 25.0) * 25)
    ELSE
      RETURN Num(n)
    END
  END ResNum;

PROCEDURE DeepFontLookup (orc: DeepFontOracle; name: TEXT): ScrnFont.T
  RAISES {ScrnFont.Failure, TrestleComm.Failure} =
  BEGIN
    RETURN orc.st.bits.font.lookup(name)
  END DeepFontLookup;

PROCEDURE FontLookup (orc: FontOracle; name: TEXT): ScrnFont.T
  RAISES {ScrnFont.Failure, TrestleComm.Failure} =
  VAR
    s: Ctypes.char_star;
    uname: TEXT;
  BEGIN
    TRY
    TrestleOnX.Enter(orc.st.trsl);
    TRY
      uname := FindUnscaled(orc.st.trsl.dpy, name); (* Prefer unscaled font *)
      IF uname = NIL THEN uname := name END;
      s := M3toC.SharedTtoS(uname);
      VAR xfs := X.XLoadQueryFont(orc.st.trsl.dpy, s);
      BEGIN
        M3toC.FreeSharedS(uname, s);
        IF xfs = NIL THEN RAISE ScrnFont.Failure END;
        RETURN FontFromXStruct(orc, xfs)
      END
    FINALLY
      TrestleOnX.Exit(orc.st.trsl)
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END FontLookup;

PROCEDURE FindUnscaled(dpy: X.DisplayStar; pat: TEXT): TEXT RAISES {X.Error} =
  (* Return the first matching unscaled font, if any.  Otherwise return NIL. *)
  VAR
    s := M3toC.SharedTtoS(pat);
    xcount: Ctypes.int;
    fonts := X.XListFonts(dpy, s, 32767, ADR(xcount));
    fp := fonts;
    count: INTEGER := xcount;
    xmatch: Ctypes.char_star := NIL;
    match: TEXT := NIL;
  BEGIN
    M3toC.FreeSharedS(pat, s);
    IF count = 0 THEN
      IF fonts # NIL THEN X.XFreeFontNames(fonts) END;
      RETURN NIL;
    END;

    FOR i := 0 TO count - 1 DO	(* Search for an unscaled font *)
      IF NOT IsScaled(M3toC.StoT(fp^)) THEN
	xmatch := fp^;
	EXIT;
      END;
      fp := fp + ADRSIZE(fp^);
    END;

    IF xmatch # NIL THEN	(* Found an unscaled font *)
	match := M3toC.CopyStoT(xmatch);
    END;
    X.XFreeFontNames(fonts);
    RETURN match;
  END FindUnscaled;

PROCEDURE IsScaled(name: TEXT): BOOLEAN =
  (* Return true if font is scaled. *)
  VAR
    len := Text.Length(name);
    fieldNum := 0;
    found0 := FALSE;
    hyphenPos: INTEGER;
  BEGIN
    (* A font is scaled if:
	a. it is in canonical form (starts with '-', and all 14 XLFD fields
	   are present), and
	b. any of the fields pixel size, point size, or average width is 0. *)
    hyphenPos := Text.FindChar(name, '-', 0);
    WHILE hyphenPos # -1 DO
      INC(fieldNum);
      IF fieldNum = 7 OR fieldNum = 8 OR fieldNum = 12 THEN
	IF hyphenPos+2 < len AND
	Text.GetChar(name, hyphenPos+1) = '0' AND
	Text.GetChar(name, hyphenPos+2) = '-' THEN
	  found0 := TRUE;
	END;
      END;
      hyphenPos := Text.FindChar(name, '-', hyphenPos+1);
    END;

    RETURN fieldNum = 14 AND Text.GetChar(name, 0) = '-' AND found0;
  END IsScaled;

CONST
  BuiltInNames = ARRAY OF
                   TEXT{
                   "-adobe-helvetica-medium-r-normal--*-100-*-*-p-*-iso8859-1",
                   "-*-helvetica-medium-r-*-*-*-10?-*-*-*-*-iso8859-1",
                   "-*-times-medium-r-*-*-*-10?-*-*-*-*-iso8859-1",
                   "fixed", "-*-helvetica-*-r-*-*-*-11?-*-*-*-*-iso8859-1",
                   "-*-helvetica-*-r-*-*-*-12?-*-*-*-*-iso8859-1",
                   "-*-helvetica-*-r-*-*-*-1??-*-*-*-*-iso8859-?",
                   "-*-times-*-r-*-*-*-1??-*-*-*-*-iso8859-?", "timrom1?",
                   "times_roman1?", "*"};

PROCEDURE DeepFontBuiltIn (orc: DeepFontOracle; id: Font.Predefined):
  ScrnFont.T =
  BEGIN
    RETURN Palette.ResolveFont(orc.st.bits, Font.T{id})
  END DeepFontBuiltIn;

PROCEDURE FontBuiltIn (orc: FontOracle; id: Font.Predefined): ScrnFont.T =
  VAR xfont: X.XFontStructStar := NIL;
  BEGIN
    IF id # Font.BuiltIn.fnt THEN Crash() END;
    WITH st   = orc.st,
         trsl = st.trsl,
         dpy  = trsl.dpy DO
      TRY
        TrestleOnX.Enter(trsl);
        TRY
          FOR i := FIRST(BuiltInNames) TO LAST(BuiltInNames) DO
            VAR s: Ctypes.char_star;
            BEGIN
              s := M3toC.FlatTtoS(BuiltInNames[i]);
              xfont := X.XLoadQueryFont(dpy, s);
            END;
            IF xfont # NIL THEN RETURN FontFromXStruct(orc, xfont) END
          END;
          Crash();   (* better to return a useless font *)
          <*ASSERT FALSE*>
        FINALLY
          TrestleOnX.Exit(orc.st.trsl)
        END
      EXCEPT
        X.Error, TrestleComm.Failure =>
          RETURN NEW(ScrnFont.T, id := 0,
                     metrics :=
                       NEW(NullMetrics,
                           minBounds := ScrnFont.CharMetric{0, Rect.Empty},
                           maxBounds := ScrnFont.CharMetric{0, Rect.Empty},
                           firstChar := 0, lastChar := 0,
                           selfClearing := TRUE, charMetrics := NIL))
      END
    END;
  END FontBuiltIn;

TYPE
  NullMetrics = ScrnFont.Metrics OBJECT
                OVERRIDES
                  intProp  := NullIntProp;
                  textProp := NullTextProp
                END;

PROCEDURE NullIntProp (<*UNUSED*> self: NullMetrics;
                       <*UNUSED*> name: TEXT;
                       <*UNUSED*> ch  : INTEGER       := -1): INTEGER
  RAISES {ScrnFont.Failure} =
  BEGIN
    RAISE ScrnFont.Failure
  END NullIntProp;

PROCEDURE NullTextProp (<*UNUSED*> self: NullMetrics;
                        <*UNUSED*> name: TEXT;
                        <*UNUSED*> ch  : INTEGER       := -1): TEXT
  RAISES {ScrnFont.Failure} =
  BEGIN
    RAISE ScrnFont.Failure
  END NullTextProp;

PROCEDURE FontFromXStruct (orc: FontOracle; xfs: X.XFontStructStar): XFont
  RAISES {TrestleComm.Failure} <* LL.sup = orc.st.trsl *> =
  (* return font for xfs and free xfs, even if the exception is raised. *)
  VAR
    res := NEW(XFont, id := xfs.fid, metrics := NEW(NullMetrics));
    xcs: X.XCharStructStar;
  BEGIN
    TRY
    TRY
      WITH trsl = orc.st.trsl,
           m    = res.metrics  DO
        m.family := TextProp(trsl, xfs, orc.familyAtm);
        m.pointSize := IntProp(xfs, orc.pointSizeAtm);
        m.slant :=
          VAL(OrdProp(xfs, orc.slantAtm, orc.slants), ScrnFont.Slant);
        m.weightName := TextProp(trsl, xfs, orc.weightNameAtm);
        m.version := "";
        m.foundry := TextProp(trsl, xfs, orc.foundryAtm);
        m.width := TextProp(trsl, xfs, orc.widthAtm);
        m.pixelsize := IntProp(xfs, orc.pixelSizeAtm);
        m.hres := IntProp(xfs, orc.resXAtm);
        m.vres := IntProp(xfs, orc.resYAtm);
        m.spacing := VAL(OrdProp(xfs, orc.spacingAtm, orc.spacings),
                         ScrnFont.Spacing);
        m.averageWidth := IntProp(xfs, orc.aveWidthAtm);
        m.charsetRegistry := TextProp(trsl, xfs, orc.registryAtm);
        m.charsetEncoding := TextProp(trsl, xfs, orc.encodingAtm);
        m.firstChar := xfs.min_char_or_byte2;
        m.lastChar := xfs.max_char_or_byte2;
        m.isAscii := Text.Equal(m.charsetRegistry, "ISO8859");
        m.defaultChar := xfs.default_char;
        m.ascent := xfs.ascent;
        m.descent := xfs.descent;
        m.fprint := Fingerprint.FromText("X font:");
        m.fprint :=
          Fingerprint.FromChars(LOOPHOLE(ADR(m.firstChar), ARRAY OF CHAR),
                                m.fprint);
        m.fprint :=
          Fingerprint.FromChars(LOOPHOLE(ADR(m.lastChar), ARRAY OF CHAR),
                                m.fprint);
        m.fprint := Fingerprint.FromChars(
                      LOOPHOLE(ADR(m.defaultChar), ARRAY OF CHAR),
                      m.fprint);
        m.fprint :=
          Fingerprint.FromChars(LOOPHOLE(ADR(m.ascent), ARRAY OF CHAR),
                                m.fprint);
        m.fprint :=
          Fingerprint.FromChars(LOOPHOLE(ADR(m.descent), ARRAY OF CHAR),
                                m.fprint);
        VAR temp := xfs.min_bounds.lbearing;
        BEGIN
          xfs.min_bounds.lbearing := xfs.max_bounds.lbearing;
          xfs.max_bounds.lbearing := temp
        END;
        ToCharMetric(xfs.min_bounds, m.minBounds);
        ToCharMetric(xfs.max_bounds, m.maxBounds);
        m.fprint :=
          Fingerprint.FromChars(LOOPHOLE(ADR(m.minBounds), ARRAY OF CHAR),
                                m.fprint);
        m.fprint :=
          Fingerprint.FromChars(LOOPHOLE(ADR(m.maxBounds), ARRAY OF CHAR),
                                m.fprint);
        IF (xfs.per_char = NIL) OR (m.minBounds = m.maxBounds) THEN
          m.charMetrics := NIL;
          WITH bd = m.minBounds,
               bb = bd.boundingBox DO
            IF bd.printWidth >= 0 THEN
              m.rightKerning := bb.east > bd.printWidth;
              m.leftKerning := bb.west < 0
            ELSE
              m.rightKerning := bb.east > 0;
              m.leftKerning := bb.west < bd.printWidth;
            END;
            m.selfClearing := NOT (m.rightKerning OR m.leftKerning)
          END
        ELSE
          m.fprint :=
            Fingerprint.FromChars(
              LOOPHOLE(xfs.per_char, ARRAY OF CHAR), m.fprint);
          m.charMetrics :=
            NEW(ScrnFont.CharMetrics, m.lastChar - m.firstChar + 1);
          WITH maxb = m.maxBounds.boundingBox DO
            m.selfClearing :=
              (maxb.north >= -xfs.ascent) AND (maxb.south <= xfs.descent)
          END;
          m.rightKerning := FALSE;
          m.leftKerning := FALSE;
          xcs := xfs.per_char;
          FOR i := 0 TO LAST(m.charMetrics^) DO
            ToCharMetric(xcs^, m.charMetrics[i]);
            WITH bd = m.charMetrics[i],
                 bb = bd.boundingBox    DO
              IF bd.printWidth >= 0 THEN
                m.rightKerning :=
                  m.rightKerning OR (bb.east > bd.printWidth);
                m.leftKerning := m.leftKerning OR (bb.west < 0)
              ELSE
                m.rightKerning := m.rightKerning OR (bb.east > 0);
                m.leftKerning :=
                  m.leftKerning OR (bb.west < bd.printWidth);
              END;
              m.selfClearing :=
                m.selfClearing AND NOT (m.rightKerning OR m.leftKerning)
            END;
            xcs := xcs + ADRSIZE(X.XCharStruct)
          END
        END
      END
    FINALLY
      X.XFreeFontInfo(NIL, xfs, 1)
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
    RETURN res
  END FontFromXStruct;

PROCEDURE ToCharMetric (READONLY xcs: X.XCharStruct;
                        VAR      cm : ScrnFont.CharMetric) =
  BEGIN
    cm.printWidth := xcs.width;
    WITH bb = cm.boundingBox DO
      bb.west := xcs.lbearing;
      bb.east := xcs.rbearing;
      bb.north := -xcs.ascent;
      bb.south := xcs.descent;
      IF (bb.west >= bb.east) OR (bb.north >= bb.south) THEN
        bb := Rect.Empty
      END
    END
  END ToCharMetric;

PROCEDURE TextProp (trsl: XClient.T; xfs: X.XFontStructStar; a: X.Atom):
  TEXT RAISES {TrestleComm.Failure} =
  VAR b: X.Atom;
  BEGIN
    TRY
    IF X.XGetFontProperty(xfs, a, ADR(b)) # X.False THEN
      RETURN XClient.ToName(trsl, b)
    ELSE
      RETURN "*"
    END
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END TextProp;

PROCEDURE IntProp (xfs: X.XFontStructStar; a: X.Atom): INTEGER
  RAISES {TrestleComm.Failure} =
  VAR b: INTEGER;
  BEGIN
    TRY
    IF X.XGetFontProperty(xfs, a, ADR(b)) # X.False THEN
      RETURN b
    ELSE
      RETURN -1
    END
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END IntProp;

PROCEDURE OrdProp (         xfs  : X.XFontStructStar;
                            a    : X.Atom;
                   READONLY names: ARRAY OF X.Atom    ): INTEGER
  RAISES {TrestleComm.Failure} =
  VAR b: X.Atom;
  BEGIN
    TRY
    IF X.XGetFontProperty(xfs, a, ADR(b)) # X.False THEN
      FOR i := 0 TO LAST(names) DO IF names[i] = b THEN RETURN i END END
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
    RETURN NUMBER(names)
  END OrdProp;

PROCEDURE InitFontOracle (orc: FontOracle; st: XScreenType.T): FontOracle
  RAISES {TrestleComm.Failure} =
  BEGIN
    orc.st := st;
    WITH trsl = st.trsl DO
      orc.familyAtm := XClient.ToAtom(trsl, "FAMILY_NAME");
      orc.pointSizeAtm := XClient.ToAtom(trsl, "POINT_SIZE");
      orc.slantAtm := XClient.ToAtom(trsl, "SLANT");
      orc.weightNameAtm := XClient.ToAtom(trsl, "WEIGHT_NAME");
      orc.foundryAtm := XClient.ToAtom(trsl, "FOUNDRY");
      orc.widthAtm := XClient.ToAtom(trsl, "SETWIDTH_NAME");
      orc.pixelSizeAtm := XClient.ToAtom(trsl, "PIXEL_SIZE");
      orc.resXAtm := XClient.ToAtom(trsl, "RESOLUTION_X");
      orc.resYAtm := XClient.ToAtom(trsl, "RESOLUTION_Y");
      orc.spacingAtm := XClient.ToAtom(trsl, "SPACING");
      orc.aveWidthAtm := XClient.ToAtom(trsl, "AVERAGE_WIDTH");
      orc.registryAtm := XClient.ToAtom(trsl, "CHARSET_REGISTRY");
      orc.encodingAtm := XClient.ToAtom(trsl, "CHARSET_ENCODING");
      orc.slants[0] := XClient.ToAtom(trsl, "R");
      orc.slants[1] := XClient.ToAtom(trsl, "I");
      orc.slants[2] := XClient.ToAtom(trsl, "O");
      orc.slants[3] := XClient.ToAtom(trsl, "RI");
      orc.slants[4] := XClient.ToAtom(trsl, "RO");
      orc.slants[5] := XClient.ToAtom(trsl, "OT");
      orc.spacings[0] := XClient.ToAtom(trsl, "P");
      orc.spacings[1] := XClient.ToAtom(trsl, "M");
      orc.spacings[2] := XClient.ToAtom(trsl, "C")
    END;
    RETURN orc
  END InitFontOracle;

PROCEDURE DeepInitFontOracle (orc: DeepFontOracle; st: XScreenType.T):
  DeepFontOracle =
  BEGIN
    orc.st := st;
    RETURN orc
  END DeepInitFontOracle;

EXCEPTION FatalError;

PROCEDURE Crash() =        
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError
  END Crash;

BEGIN
END XScrnFont.
