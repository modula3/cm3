(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman and Greg Nelson           *)
(* Last modified on Tue Aug 22 21:38:10 PDT 1995 by najork *)
(*      modified on Sun Jul 11 15:14:23 PDT 1993 by steveg *)
(*      modified on Sat Feb  6 18:05:22 PST 1993 by meehan *)
(*      modified on Mon Feb  1 12:42:37 PST 1993 by mhb    *)
(*      modified on Tue Jun 16 13:08:22 PDT 1992 by muller *)

MODULE ScaleFilter;

IMPORT Axis, Cursor, Filter, FilterClass, Font, JoinScreen,
       MultiFilter, MultiClass, PaintOp, Palette, Pixmap, Rd,
       Rect, ScrnCursor, ScrnFont, ScrnPaintOp, ScrnPixmap,
       TextRd, TextWr, Thread, TrestleComm, VBT, VBTClass,
       VBTRep, Wr;

REVEAL
  Private = Filter.T BRANDED OBJECT END;
  T = Public BRANDED OBJECT
        stNew                 : VBT.ScreenType := NIL;
        hscale, vscale                         := 1.0;
        auto                                   := FALSE;
        keepAspectRatio       : BOOLEAN;
        oldHorSize, oldVerSize                 := 0.0;
      OVERRIDES
        init     := Init;
        rescreen := Rescreen;
        reshape  := Reshape;
      END;

TYPE
  MC = MultiClass.Filter OBJECT
       OVERRIDES
         succ    := Succ;
         pred    := Succ;
         replace := Replace;
       END;

PROCEDURE Replace (m: MC; ch: VBT.T; new: VBT.T) =
  BEGIN
    WITH holder = Filter.Child(m.vbt) DO
      <* ASSERT ch = Filter.Child(holder) *>
      EVAL Filter.Replace(holder, new);
    END
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  BEGIN
    WITH holder = Filter.Child(m.vbt) DO
      IF ch = NIL THEN
        RETURN Filter.Child(holder)
      ELSE
        <* ASSERT ch = Filter.Child(holder) *>
        RETURN NIL
      END
    END
  END Succ;

PROCEDURE Init (t: T; ch: VBT.T): T =
  VAR holder := NEW(Filter.T).init(ch);
  BEGIN
    EVAL Filter.T.init(t, holder);
    MultiClass.Be(t, NEW(MC));
    MultiClass.BeChild(t, ch);
    RETURN t;
  END Init;

PROCEDURE Reshape (t: T; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    IF t.auto THEN AutoReshape(t, cd) END;
    Filter.T.reshape(t, cd)
  END Reshape;

TYPE
  ScaledScreenType =
    VBT.ScreenType OBJECT
      unscaledRes   : ARRAY Axis.T OF REAL;
      hscale, vscale                         := 1.0;
      stParent      : VBT.ScreenType;
    METHODS
      scale       (hscale, vscale: REAL) := ScaleScreenType;
      (* changes the res, scales all ScaledFonts in Palette *)
    OVERRIDES
      opApply                            := ScaleOpApply;
      cursorApply                        := ScaleCursorApply;
      pixmapApply                        := ScalePixmapApply;
      fontApply                          := ScaleFontApply;
    END;

PROCEDURE ScaleOpApply (             st: ScaledScreenType;
                        <* UNUSED *> cl: Palette.OpClosure;
                                     op: PaintOp.T          ):
  ScrnPaintOp.T =
  BEGIN
    RETURN Palette.ResolveOp(st.stParent, op);
  END ScaleOpApply;

PROCEDURE ScaleCursorApply (             st: ScaledScreenType;
                            <* UNUSED *> cl: Palette.CursorClosure;
                                         cs: Cursor.T               ):
  ScrnCursor.T =
  BEGIN
    RETURN Palette.ResolveCursor(st.stParent, cs);
  END ScaleCursorApply;

PROCEDURE ScalePixmapApply (             st: ScaledScreenType;
                            <* UNUSED *> cl: Palette.PixmapClosure;
                                         pm: Pixmap.T               ):
  ScrnPixmap.T =
  BEGIN
    RETURN Palette.ResolvePixmap(st.stParent, pm);
  END ScalePixmapApply;

PROCEDURE ScaleFontApply (st  : ScaledScreenType;
                          cl  : Palette.FontClosure;
                          font: Font.T                 ): ScrnFont.T =
  BEGIN
    IF cl =  NIL THEN
      (* builtin *)
      RETURN Palette.ResolveFont(st.stParent, font);
    ELSE
      RETURN VBT.ScreenType.fontApply(st, cl, font);
    END;
  END ScaleFontApply;

PROCEDURE ScaleScreenType (st: ScaledScreenType; hscale, vscale: REAL) =
  BEGIN
    st.res[Axis.T.Hor] := st.unscaledRes[Axis.T.Hor] * hscale;
    st.hscale := hscale;
    st.res[Axis.T.Ver] := st.unscaledRes[Axis.T.Ver] * vscale;
    st.vscale := vscale;
    FOR i := 0 TO LAST(st.fonts^) DO
      TYPECASE st.fonts[i] OF
      | NULL =>
      | ScaledFont (sf) => sf.scaleTo(MIN(st.hscale, st.vscale));
      ELSE
      END;
    END;
  END ScaleScreenType;

TYPE
  FontOracle =
    ScrnFont.Oracle OBJECT
      st: ScaledScreenType;
    METHODS
      lookupScaled (name: TEXT; size: REAL; initialScale: REAL := 1.0):
                    ScrnFont.T := LookupScaled;
    OVERRIDES
      match   := Match;
      list    := List;
      lookup  := Lookup;
      builtIn := BuiltIn;
    END;

PROCEDURE LookupScaled (orc         : FontOracle;
                        name        : TEXT;
                        size        : REAL;
                        initialScale: REAL        ): ScrnFont.T =
  VAR sf := NEW(ScaledFont);
  BEGIN
    sf.orc := orc;
    sf.name := DeSize(name);
    sf.size := size;
    sf.scale := initialScale;
    TRY
      sf.matches := orc.list(sf.name, 1000);
    EXCEPT TrestleComm.Failure => sf.matches := NIL END;
    RETURN BestMatch(orc, sf);
  END LookupScaled;

TYPE
  ScaledFont = ScrnFont.T OBJECT
                 orc    : FontOracle;
                 name   : TEXT;
                 size   : REAL;
                 scale  : REAL;
                 matches: REF ARRAY OF TEXT := NIL;
                 current: ScrnFont.T;
               METHODS
                 scaleTo (scale: REAL) := ScaleFont;
               END;

PROCEDURE ScaleFont (sf: ScaledFont; scale: REAL) =
  BEGIN
    sf.scale := scale;
    EVAL BestMatch(sf.orc, sf);
  END ScaleFont;

CONST Inf = 999999999.9;

PROCEDURE BestMatch (orc: FontOracle; sf: ScaledFont): ScrnFont.T =
  VAR
    matches       := sf.matches;
    closest: TEXT;
    dist   : REAL := Inf;
    size          := sf.size;
    scale         := sf.scale;
  BEGIN
    IF matches = NIL OR NUMBER(matches^) = 0 THEN
      sf.current := NIL;
    ELSE
      dist := Inf;
      FOR i := 0 TO LAST(matches^) DO
        WITH d = ABS(PointSize(matches[i]) - scale * size) DO
          IF d < dist THEN closest := matches[i]; dist := d; END;
        END;
      END;
      TRY
        sf.current := orc.st.stParent.font.lookup(closest);
      EXCEPT
        ScrnFont.Failure, TrestleComm.Failure => sf.current := NIL
      END;
    END;
    IF sf.current = NIL THEN
      sf.current := orc.st.stParent.fonts[Font.BuiltIn.fnt]
    END;
    sf.id := sf.current.id;
    sf.metrics := sf.current.metrics;
    RETURN sf;
  END BestMatch;

(* Assumes name is an X style font name, pointsize is the integer OR
   REAL!!!  after 8 "-"s *)
PROCEDURE PointSize (name: TEXT): REAL =
  VAR
    rd                 := TextRd.New(name);
    int      : INTEGER := 0;
    ch       : CHAR;
    res, frac: REAL;
  BEGIN
    TRY
      FOR i := 1 TO 8 DO REPEAT UNTIL Rd.GetChar(rd) = '-'; END;
      ch := Rd.GetChar(rd);
    EXCEPT
      Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN Inf
    END;

    TRY
      WHILE ORD(ch) >= ORD('0') AND ORD(ch) <= ORD('9') DO
        int := 10 * int + ORD(ch) - ORD('0');
        ch := Rd.GetChar(rd);
      END;
    EXCEPT
      Rd.EndOfFile, Rd.Failure, Thread.Alerted =>
    END;

    (* slightly inaccurate conversion to floating pt *)
    IF ch = '.' THEN
      res := FLOAT(int);
      TRY
        ch := Rd.GetChar(rd);
        frac := 0.1;
        WHILE ORD(ch) >= ORD('0') AND ORD(ch) <= ORD('9') DO
          res := res + FLOAT(ORD(ch) - ORD('0')) * frac;
          frac := frac / 10.0;
          ch := Rd.GetChar(rd);
        END;
      EXCEPT
        Rd.EndOfFile, Rd.Failure, Thread.Alerted =>
      END;
    ELSE
      (* integer measurements are in 1/10 points *)
      res := FLOAT(int) / 10.0;
    END;
    IF res = 0.0 THEN RETURN Inf ELSE RETURN res END;
  END PointSize;

PROCEDURE DeSize (name: TEXT): TEXT =
  VAR
    rd       := TextRd.New(name);
    wr       := TextWr.New();
    ch: CHAR;
  <* FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    TRY
      (* copy up to pixelsize *)
      FOR i := 1 TO 7 DO
        ch := Rd.GetChar(rd);
        WHILE ch # '-' DO Wr.PutChar(wr, ch); ch := Rd.GetChar(rd); END;
        Wr.PutChar(wr, ch);
      END;
    EXCEPT
      Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN name;
    END;

    TRY
      (* skip pixelsize, pointsize, hres, vres *)
      FOR i := 1 TO 4 DO
        ch := Rd.GetChar(rd);
        WHILE ch # '-' DO ch := Rd.GetChar(rd); END;
      END;
    EXCEPT
      Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN name
    END;
    Wr.PutText(wr, "*-*-*-*-");

    TRY
      (* copy spacing *)
      ch := Rd.GetChar(rd);
      WHILE ch # '-' DO Wr.PutChar(wr, ch); ch := Rd.GetChar(rd); END;
      Wr.PutChar(wr, ch);
    EXCEPT
      Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN name;
    END;

    TRY
      (* skip average width *)
      ch := Rd.GetChar(rd);
      WHILE ch # '-' DO ch := Rd.GetChar(rd); END;
    EXCEPT
      Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN name
    END;
    Wr.PutText(wr, "*-");

    TRY
      (* copy registry *)
      ch := Rd.GetChar(rd);
      WHILE ch # '-' DO Wr.PutChar(wr, ch); ch := Rd.GetChar(rd); END;
      Wr.PutChar(wr, ch);
    EXCEPT
      Rd.EndOfFile, Rd.Failure, Thread.Alerted => RETURN name;
    END;

    LOOP
      TRY
        (* copy charset *)
        ch := Rd.GetChar(rd);
        Wr.PutChar(wr, ch);
      EXCEPT
      | Rd.EndOfFile => EXIT;
      | Rd.Failure, Thread.Alerted => RETURN name;
      END;
    END;
    RETURN TextWr.ToText(wr);
  END DeSize;

PROCEDURE BuiltIn(orc: FontOracle; f: Font.Predefined): ScrnFont.T =
  BEGIN
    RETURN orc.st.stParent.bits.font.builtIn(f);
  END BuiltIn;

PROCEDURE List (orc: FontOracle; pat: TEXT; maxResults: INTEGER):
  REF ARRAY OF TEXT RAISES { TrestleComm.Failure} =
  BEGIN
    (* !!!!TEMPORARY (until msm fixes JoinFont oracle !!!!! *)
    IF ISTYPE (orc.st.stParent, JoinScreen.T) THEN RETURN NIL END;
    (* !!!!TEMPORARY (until msm fixes JoinFont oracle !!!!! *)
    RETURN orc.st.stParent.bits.font.list(pat, maxResults)
  END List;

PROCEDURE Lookup (orc: FontOracle; name: TEXT; <*UNUSED*>useXft : BOOLEAN := TRUE): ScrnFont.T
  RAISES {ScrnFont.Failure, TrestleComm.Failure} =
  VAR size := PointSize(name);
  BEGIN
    (* !!!!TEMPORARY (until msm fixes JoinFont oracle !!!!! *)
    IF ISTYPE(orc.st.stParent, JoinScreen.T) THEN
      RETURN Palette.ResolveFont(orc.st.stParent, Font.BuiltIn)
    END;
    (* !!!!TEMPORARY (until msm fixes JoinFont oracle !!!!! *)
    IF size = Inf THEN
      RETURN orc.st.stParent.bits.font.lookup(name)
    ELSE
      RETURN
        orc.lookupScaled(name, size, MIN(orc.st.hscale, orc.st.vscale))
    END;
  END Lookup;

PROCEDURE Match (             orc      : FontOracle;
                              family   : TEXT;
                 <* UNUSED *> pointSize: INTEGER      := 120;
                 slant     : ScrnFont.Slant := ScrnFont.Slant.Roman;
                 maxResults: CARDINAL       := 1;
                 weightName: TEXT           := ScrnFont.AnyMatch;
                 version   : TEXT           := "";
                 foundry   : TEXT           := ScrnFont.AnyMatch;
                 width     : TEXT           := ScrnFont.AnyMatch;
                 <* UNUSED *> pixelsize: INTEGER := ScrnFont.AnyValue;
                 <* UNUSED *> hres, vres: INTEGER := ScrnFont.ScreenTypeResolution;
                 spacing: ScrnFont.Spacing := ScrnFont.Spacing.Any;
                 <* UNUSED *> averageWidth: INTEGER := ScrnFont.AnyValue;
                 charsetRegistry: TEXT := "ISO8859";
                 charsetEncoding: TEXT := "1"        ): REF ARRAY OF TEXT
  RAISES {TrestleComm.Failure} =
  BEGIN
    (* !!!!TEMPORARY (until msm fixes JoinFont oracle !!!!! *)
    IF ISTYPE(orc.st.stParent, JoinScreen.T) THEN RETURN NIL END;
    (* !!!!TEMPORARY (until msm fixes JoinFont oracle !!!!! *)
    RETURN orc.st.stParent.bits.font.match(
             family, ScrnFont.AnyValue, slant, maxResults, weightName,
             version, foundry, width, ScrnFont.AnyValue,
             ScrnFont.ScreenTypeResolution, ScrnFont.ScreenTypeResolution,
             spacing, ScrnFont.AnyValue, charsetRegistry, charsetEncoding);
  END Match;

PROCEDURE InitST (stNew: ScaledScreenType;  st, stBits: VBT.ScreenType;
                  hscale, vscale   : REAL            ) =
  BEGIN
    stNew.stParent := st;
    stNew.depth := st.depth;
    stNew.color := st.color;
    stNew.res := st.res;
    stNew.bg := st.bg;
    stNew.fg := st.fg;
    stNew.bits := stBits;
    stNew.op := st.op;
    stNew.cursor := st.cursor;
    stNew.font := NEW(FontOracle, st := stNew);
    stNew.pixmap := st.pixmap;
    stNew.cmap := st.cmap;
    stNew.unscaledRes := st.res;
    Palette.Init(stNew);
    stNew.scale(hscale, vscale);
  END InitST;

PROCEDURE NewST (st: VBT.ScreenType; hscale, vscale: REAL):
  VBT.ScreenType =
  VAR stBits, stNew: VBT.ScreenType;
  BEGIN
    stBits := NEW(ScaledScreenType);
    InitST(stBits, st.bits, stBits, hscale, vscale);
    stNew := NEW(ScaledScreenType);
    InitST(stNew, st, stBits, hscale, vscale);
    RETURN stNew;
  END NewST;

PROCEDURE Rescreen (t: T; READONLY cd: VBT.RescreenRec) =
  BEGIN
    t.oldHorSize := 0.0;
    t.oldVerSize := 0.0;
    Scale1(t, cd.st);
  END Rescreen;

PROCEDURE Scale1 (t: T; st: VBT.ScreenType) =
  BEGIN
    IF st = NIL OR (t.hscale > 0.9 AND t.hscale < 1.1 AND t.vscale > 0.9
         AND t.vscale < 1.1) THEN
      t.stNew := st
    ELSE
      t.stNew := NewST(st, t.hscale, t.vscale)
    END;
    IF t.ch # NIL THEN VBTClass.Rescreen(t.ch, t.stNew); END;
  END Scale1;

PROCEDURE Get (t: T; VAR hscale, vscale: REAL) =
  BEGIN
    hscale := t.hscale;
    vscale := t.vscale;
  END Get;

PROCEDURE Scale (t: T; hscale, vscale: REAL) =
  BEGIN
    t.auto := FALSE;
    IF hscale # t.hscale OR vscale # t.vscale THEN
      ChangeScale(t, hscale, vscale);
    END;
  END Scale;

PROCEDURE ChangeScale (t: T; hscale, vscale: REAL) =
  BEGIN
    t.hscale := hscale;
    t.vscale := vscale;
    TYPECASE t.stNew OF
    | NULL => IF t.st # NIL THEN Scale1(t, t.st) END;
    | ScaledScreenType (sst) => sst.scale(hscale, vscale);
    ELSE
      Scale1(t, t.st);
    END;
    VBT.NewShape(t);
    VBT.Mark(t);
  END ChangeScale;

PROCEDURE AutoScale (t: T; keepAspectRatio := FALSE) =
  BEGIN
    t.auto := TRUE;
    t.keepAspectRatio := keepAspectRatio;
    VBT.Mark(t);
  END AutoScale;

PROCEDURE AutoReshape (t: T; READONLY cd: VBT.ReshapeRec) =
  VAR
    ch            := MultiFilter.Child(t);
    dom           := cd.new;
    sx, sy : REAL;
    horSize       := FLOAT(Rect.HorSize(dom));
    verSize       := FLOAT(Rect.VerSize(dom));
  BEGIN
    IF ch # NIL AND NOT Rect.IsEmpty(dom) THEN
      IF t.oldHorSize = 0.0 AND t.oldVerSize = 0.0 THEN
        VAR sz := VBTClass.GetShapes(ch, TRUE);
        BEGIN
          IF sz[Axis.T.Hor].pref = 0 OR sz[Axis.T.Ver].pref = 0 THEN
            sx := 1.0;
            sy := 1.0;
          ELSE
            sx := horSize / FLOAT(sz[Axis.T.Hor].pref);
            sy := verSize / FLOAT(sz[Axis.T.Ver].pref);
          END;
        END;
      ELSE
        sx := horSize / t.oldHorSize;
        sy := verSize / t.oldVerSize;
      END;
      IF t.keepAspectRatio THEN sx := MIN(sx, sy); sy := sx; END;
      IF sx < 0.95 OR sx > 1.05 OR sy < 0.95 OR sy > 1.05 THEN
        t.oldHorSize := horSize;
        t.oldVerSize := verSize;
        ChangeScale(t, t.hscale * sx, t.vscale * sy);
      END;
    END;
  END AutoReshape;

BEGIN
END ScaleFilter.
