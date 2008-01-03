(* Copyright 1994 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu May 25 12:21:21 PDT 1995 by mcjones *)
(*      modified on Thu Mar  9 11:05:37 PST 1995 by birrell *)

(* Post-process output of ocr.ps. *)

MODULE OCR_PS;

IMPORT BBox, BBoxSeq, FloatMode, Lex, Rd, Text, TextSeq, Thread;

<*FATAL FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted *>

<*FATAL Fatal*>

EXCEPTION Fatal;

(* Character encoding.  Each element of the QE directive produced by
   ocr.ps is either an index in the StandardGlyph array, or is
   "NonstandardGlyph" (indicating the corresponding entry in the font's
   encoding specifies some nonstandard glyph). *)

TYPE GlyphIndex = CARDINAL;
CONST
  NonstandardGlyph: GlyphIndex = 9999;
  UnknownChar = '#'; (* substitute for nonstandard glyph *)

(* The first 256 entries in StandardGlyphs correspond to ISOLatin1;
   the next 28 entries correspond to characters not in ISOLatin1, but
   defined in the standard /Times-Roman font. *)
CONST
  LastISOLatin1 = 255;
  SpecialGlyphs = ARRAY [LastISOLatin1+1 .. LastISOLatin1+28] OF TEXT {
    "''",    (* quotedblright *)
    "S\237", (* Scaron *)
    "+",     (* dagger *)
    "<",     (* guilsinglleft *)
    "Z\237", (* Zcaron *)
    "#",     (* daggerdbl *)
    "L/",    (* Lslash *)
    "...",   (* ellipsis *)
    ">",     (* guilsinglright *)
    "oe",    (* oe *)
    "fi",    (* fi *)
    ".",     (* bullet *)
    "o/oo",  (* perthousand *)
    "''",    (* quotedblbase *)
    "--",    (* endash *)
    "---",   (* emdash *)
    "^TM",   (* trademark *)
    "f",     (* florin *)
    "l/",    (* lslash *)
    "s\237", (* scaron *)
    "Y\250", (* Ydieresis *)
    "fl",    (* fl *)
    "/",     (* fraction *)
    "``",    (* quotedblleft *)
    "'",     (* quotesinglbase *)
    "'",     (* quotesingle *)
    "z\237", (* zcaron *)
    "OE"     (* OE *)
  };

(* The next 256 entries correspond to the self-named glyphs used in
   Type 3 fonts from dvips: "\000", ..., "\377":  *)
CONST
  FirstDvips = LAST(SpecialGlyphs)+1;
  LastDvips = FirstDvips+256-1;

(* The next 512 entries correspond to glyph names used in Microsoft
   TrueType fonts: "G00", ..., "Gff" and "G00", ..., "GFF", which
   in both cases correspond to ISOLatin1 with some extensions. *)
CONST
  FirstTT1 = LastDvips+1;
  LastTT1 = FirstTT1+256-1;
  FirstTT2 = LastTT1+1;
  LastTT2 = FirstTT2+256-1;
  FirstOldDvips = LastTT2+1;
  LastOldDvips = FirstOldDvips+128-1; (* note only 128 *)
  TTSpecialGlyphs = ARRAY [FirstTT1+130 .. FirstTT1+159] OF TEXT {
    "'",     (* quotesinglbase *)
    "f",     (* florin *)
    "''",    (* quotdblbase *)
    "...",   (* ellipsis *)
    "+",     (* dagger *)
    "#",     (* daggerdbl *)
    "\223",  (* circumflex *)
    "o/oo",  (* perthousand *)
    "S\237", (* Scaron *)
    "<",     (* guilsinglleft *)
    "OE",    (* OE *)
    "#",     (* <undefined> *)
    "#",     (* <undefined> *)
    "#",     (* <undefined> *)
    "#",     (* <undefined> *)
    "`",     (* ISOLatin1: quoteleft *)
    "'",     (* ISOLatin1: quoteright *)
    "``",    (* quotedblleft *)
    "''",    (* quotedblright *)
    ".",     (* bullet *)
    "--",    (* endash *)
    "---",   (* emdash *)
    "~",     (* ISOLatin1: tilde *)
    "^TM",   (* trademark *)
    "s\237", (* scaron *)
    ">",     (* guilsinglright *)
    "oe",    (* oe *)
    "#",     (* <undefined> *)
    "#",     (* <undefined> *)
    "Y\250"  (* Ydieresis" *)
    };

  DvipsGlyphs = ARRAY [FirstDvips .. FirstDvips+127] OF TEXT {
  (* 00x *)
    "\\Gamma", "\\Delta", "\\Theta", "\\Lambda",
    "\\Xi", "\\Pi", "\\Sigma", "\\Upsilon",
  (* 01x *)
    "\\Phi", "\\Psi", "\\Omega", "ff", "fi", "fl", "ffi", "ffl",
  (* 02x *)
    "i",     (* \imath *)
    "j",     (* \jmath *)
    "`",
    "'",
    "\237",  (* caron *)
    "\226",  (* breve *)
    "\257",  (* macron *)
    "\232",  (* ring *)
  (* 03x *)
    "\270",  (* cedilla *)
    "\337",  (* germandbls *)
    "ae",
    "oe",
    "\370",  (* oslash *)
    "AE",
    "OE",
    "\330",  (* Oslash *)
  (* 04x *)
    "/" (* bar for Polish suppressed-L ??? *), "!", "''", "#",
    "$", "%", "&", "'",
  (* 05x *)
    "(", ")", "*", "+", ",", "\255" (* hyphen *), ".", "/",
  (* 06x *)
    "0", "1", "2", "3", "4", "5", "6", "7",
  (* 07x *)
    "8", "9", ":", ";",
    "!" (* exclamdown *), "=", "?" (* questiondown *), "?",
  (* 10x *)
    "@", "A", "B", "C", "D", "E", "F", "G",
  (* 11x *)
    "H", "I", "J", "K", "L", "M", "N", "O",
  (* 12x *)
    "P", "Q", "R", "S", "T", "U", "V", "W",
  (* 13x *)
    "X", "Y", "Z", "[",
    "``", "]", "\223" (* circumflex *), "\227" (* dotaccent *),
  (* 14x *)
    "`", "a", "b", "c", "d", "e", "f", "g",
  (* 15x *)
    "h", "i", "j", "k", "l", "m", "n", "o",
  (* 16x *)
    "p", "q", "r", "s", "t", "u", "v", "w",
  (* 17x *)
    "x", "y", "z",
    "--",    (* en dash *)
    "---",   (* em dash *)
    "\235",  (* hungarumlaut *)
    "~",
    "\250"   (* dieresis *)
    };

(* There are gaps in the set of printable ISOLatin1 characters: *)
CONST ISOLatin1Gaps = SET OF [0..255] {8_0..8_37, 8_177..8_217, 8_231, 8_234};

TYPE
  Metrics = REF RECORD
    blx, bly, trx, try: REAL; (* font matrix in character coordinates *)
    char: ARRAY [0..255] OF RECORD x, y: REAL END (* widths (ditto) *)
  END;
  Encoding = REF ARRAY CHAR OF GlyphIndex;
  Font = REF RECORD
    x, y: REAL; (* (1000,0) in font's character coordinate system *)
    xp, yp: REAL; (* (0,1000) in font's character coordinate system *)
    e: INTEGER; (* index in "t.encoding" *)
    m: INTEGER; (* index in "t.metrics" *)
    bx, by, tx, ty: REAL (* height of font bbox in reporting coordinates *)
  END;

REVEAL T = Public BRANDED OBJECT
    rd: Rd.T;
    scale: REAL; (* conversion from reporting coordinates to pixels *)
    metrics: REF ARRAY OF Metrics;
    encoding: REF ARRAY OF Encoding;
    font: REF ARRAY OF Font;
    words: TextSeq.T;
    bBoxes: BBoxSeq.T;

    (* Data for current word prefix: *)
    buf: ARRAY [0..1000] OF CHAR;
    lbuf := 0; (* SUBARRAY(buf, 0, lbuf) is in use *)
    f: INTEGER; (* font number *)
    x0, y0, x1, y1: REAL; (* initial, final currentpoint *)
  METHODS
    output() RAISES {Rd.EndOfFile} := Output;
    parseChar(pattern: CHAR) RAISES {Rd.EndOfFile} := ParseChar;
    parseEncoding() RAISES {Rd.EndOfFile} := ParseEncoding;
    parseFont() RAISES {Rd.EndOfFile} := ParseFont;
    parseMetrics() RAISES {Rd.EndOfFile} := ParseMetrics;
    parsePair(VAR (*OUT*) x, y: REAL) RAISES {Rd.EndOfFile} := ParsePair;
    parseString() RAISES {Rd.EndOfFile} := ParseString
  OVERRIDES
    init := Init;
    nextPage := NextPage
  END;

PROCEDURE Init(t: T; rd: Rd.T; <*UNUSED*>resolution: CARDINAL): T =
  BEGIN
    t.rd := rd;
    t.scale := 1.0;
    t.metrics := NEW(REF ARRAY OF Metrics, 100);
    t.encoding := NEW(REF ARRAY OF Encoding, 100);
    t.font := NEW(REF ARRAY OF Font, 100);
    RETURN t
  END Init;

PROCEDURE NextPage(t: T; words: TextSeq.T; bBoxes: BBoxSeq.T): BOOLEAN =
  VAR ch: CHAR;
  BEGIN
    t.words := words;
    t.bBoxes := bBoxes;
    TRY
      LOOP
        REPEAT (*SKIP*) UNTIL Rd.GetChar(t.rd) = 'Q';
        ch := Rd.GetChar(t.rd);
        CASE ch OF
        | 'M' => t.parseMetrics()
        | 'E' => t.parseEncoding()
        | 'F' => t.parseFont()
        | 'S' => t.parseString()
        | 'C' => (* copypage *) RAISE Fatal
        | 'P' => (* showpage *)
                 IF t.lbuf > 0 THEN t.output() END; RETURN TRUE
        | 'Z' => (*SKIP*) (* erasepage *)
        ELSE (* look for next 'Q' *)
        END;
        Lex.Skip(t.rd, Lex.Blanks)
      END;
    EXCEPT Rd.EndOfFile =>
    END;
    RETURN FALSE
  END NextPage;

PROCEDURE ParseChar(t: T; pattern: CHAR) RAISES {Rd.EndOfFile} =
  VAR ch := Rd.GetChar(t.rd);
  BEGIN
    IF ch # pattern THEN RAISE Fatal END
  END ParseChar;

PROCEDURE ParseEncoding(t: T) =
(* Parse QE directive. *)
  VAR
    e := Lex.Int(t.rd);
    n := Lex.Int(t.rd);
    new: REF ARRAY OF Encoding;
    tooSparse := FALSE;
  BEGIN
    IF e < 0 THEN RAISE Fatal END;
    IF n > 256 THEN RAISE Fatal END;
    WITH size = NUMBER(t.encoding^) DO
      IF size <= e THEN
        new := NEW(REF ARRAY OF Encoding, 2 * MAX(e, size));
        SUBARRAY(new^, 0, size) := t.encoding^;
        t.encoding := new
      END
    END;
    IF t.encoding[e] = NIL THEN
      t.encoding[e] := NEW(Encoding)
    END;
    WITH enc = t.encoding[e] DO
      FOR i := 0 TO 255 DO
        IF i < n THEN
          enc^[VAL(i, CHAR)] := Lex.Int(t.rd)
        ELSE
          enc^[VAL(i, CHAR)] := NonstandardGlyph
        END
      END;
      (* Some applications build the encoding vector incrementally.
         If this one doesn't have at least the lower-case letters,
         we augment it with ISOLatin1. *)
      FOR c := 'a' TO 'z' DO
        IF enc^[c] = NonstandardGlyph THEN tooSparse := TRUE END;
      END;
      IF tooSparse THEN
        FOR i := 0 TO 255 DO
          WITH glyph = enc^[VAL(i, CHAR)] DO
            IF glyph = NonstandardGlyph THEN
              glyph := i
            END
          END
        END
      END
    END
  END ParseEncoding;

CONST
  GuessAscend =   0.9;
  GuessDescend = -0.3;

PROCEDURE ParseFont(t: T) RAISES {Rd.EndOfFile} =
(* Parse QF directive. *)
  VAR n := Lex.Int(t.rd); new: REF ARRAY OF Font; xmax: REAL;
  BEGIN
    IF n < 0 THEN RAISE Fatal END;
    WITH size = NUMBER(t.font^) DO
      IF size <= n THEN
        new := NEW(REF ARRAY OF Font, 2 * MAX(n, size));
        SUBARRAY(new^, 0, size) := t.font^;
        t.font := new
      END
    END;
    IF t.font[n] = NIL THEN
      t.font[n] := NEW(Font);
    END;
    WITH f = t.font[n] DO
      t.parsePair(f.x, f.y);
      t.parsePair(f.xp, f.yp);
      f.e := Lex.Int(t.rd);
      IF t.encoding[f.e] = NIL THEN RAISE Fatal END;
      f.m := Lex.Int(t.rd);
      WITH mt = t.metrics[f.m] DO
        IF mt = NIL THEN RAISE Fatal END;
        (* Transform height of font bounding box to reporting coordinates: *)
        f.bx := f.xp * mt.bly / 1000.0;
        f.by := f.yp * mt.bly / 1000.0;
        f.tx := f.xp * mt.try / 1000.0;
        f.ty := f.yp * mt.try / 1000.0;

        (* In some fonts produced by dvips, the FontBBox is incorrectly
           defined as [0 0 1 1].  We check for this, and apply the same
           heuristic used for an undefined FontBBox in "ParseMetrics".  *)
        IF f.by-f.ty < 1.1 THEN
          xmax := 0.0;
          FOR i := 0 TO 255 DO
            IF mt.char[i].x > xmax THEN xmax := mt.char[i].x END
          END;
          WITH bly = GuessDescend * xmax, try = GuessAscend * xmax DO
            f.bx := f.xp * bly / 1000.0;
            f.by := f.yp * bly / 1000.0;
            f.tx := f.xp * try / 1000.0;
            f.ty := f.yp * try / 1000.0
          END
        END;
      
      END
    END
  END ParseFont;

PROCEDURE ParseMetrics(t: T) RAISES {Rd.EndOfFile} =
(* Parse QM directive. *)
  VAR m := Lex.Int(t.rd); new: REF ARRAY OF Metrics;
  BEGIN
    IF m < 0 THEN RAISE Fatal END;
    WITH size = NUMBER(t.metrics^) DO
      IF size <= m THEN
        new := NEW(REF ARRAY OF Metrics, 2 * MAX(m, size));
        SUBARRAY(new^, 0, size) := t.metrics^;
        t.metrics := new
      END
    END;
    IF t.metrics[m] = NIL THEN
      t.metrics[m] := NEW(Metrics)
    END;
    WITH mt = t.metrics[m] DO
      t.parsePair(mt.blx, mt.bly);
      t.parsePair(mt.trx, mt.try);
      FOR i := 0 TO 255 DO
        t.parsePair(mt.char[i].x, mt.char[i].y)
      END;
      IF mt.blx = 0.0 AND mt.bly = 0.0 AND mt.trx = 0.0 AND mt.try = 0.0 THEN
        (* "FontBBox" was not specified; take a guess. *)
        (* *** <* ASSERT FALSE *> *)
        FOR i := 0 TO 255 DO
          IF mt.char[i].x > mt.trx THEN mt.trx := mt.char[i].x END
        END;
        mt.bly := GuessDescend * mt.trx;
        mt.try := GuessAscend * mt.trx
      END;
    END
  END ParseMetrics;

PROCEDURE ParsePair(t: T; VAR (*OUT*) x, y: REAL) =
(* Set "x" and "y" to the next pair of "fixed-point" numbers. *)
  BEGIN
    x := FLOAT(Lex.Int(t.rd)) / 100.0;
    y := FLOAT(Lex.Int(t.rd)) / 100.0
  END ParsePair;

PROCEDURE SameDirection(x0, y0, x1, y1: REAL): BOOLEAN =
(* Return TRUE iff the vectors (x0, y0) and (x1, y1) have the same angle. *)
  BEGIN
    RETURN y0 = 0.0 AND y1 = 0.0 AND x0*x1 > 0.0
        OR x0 = 0.0 AND x1 = 0.0 AND y0*y1 > 0.0
        OR x0 * y1 = x1 * y0
  END SameDirection;

PROCEDURE Output(t: T) =
  VAR x0, y0, x1, y1, x2, y2, x3, y3: REAL;
  BEGIN
    t.words.addhi(Text.FromChars(SUBARRAY(t.buf, 0, t.lbuf)));

    WITH f = t.font[t.f] DO
      (* Compute the corners of the parallelogram with width
         "(t.x0,t.y0)" to "(t.x1,t.y1)" and height "(f.bx,f.by)" to
         "(f.tx,f.ty)". Then compute the bottom left corner and the
         top right corner of the bounding box (rectangle with sides
         parallel to the coordinate system) of this rectangle. *)
      x0 := t.x0 + f.bx; y0 := t.y0 + f.by;
      x1 := t.x1 + f.bx; y1 := t.y1 + f.by;
      x2 := t.x0 + f.tx; y2 := t.y0 + f.ty;
      x3 := t.x1 + f.tx; y3 := t.y1 + f.ty
    END;
    t.bBoxes.addhi(BBox.T{
      blx := FLOOR(t.scale * MIN(MIN(MIN(x0, x1), x2), x3)),
      bly := FLOOR(t.scale * MAX(MAX(MAX(y0, y1), y2), y3)),
      trx := FLOOR(t.scale * MAX(MAX(MAX(x0, x1), x2), x3)),
      try := FLOOR(t.scale * MIN(MIN(MIN(y0, y1), y2), y3))
      });
    t.lbuf := 0
  END Output;

PROCEDURE ParseString(t: T) RAISES {Rd.EndOfFile} =
(* Parse QS directive. *)
  CONST k = 0.3; (* fraction of average character width to signal word break *)
  VAR
    buf: ARRAY [0..1000] OF CHAR;
    n, l: INTEGER;
    glyph: GlyphIndex;
    x0, y0, x1, y1, xsp, ysp: REAL;
  PROCEDURE SetBuf() =
    BEGIN
      SUBARRAY(t.buf, 0, l) := SUBARRAY(buf, 0, l);
      t.lbuf := l;
      t.f := n;
      t.x0 := x0; t.y0 := y0; t.x1 := x1; t.y1 := y1
    END SetBuf;
  BEGIN
    n := Lex.Int(t.rd); (* index in "t.font" *)
    IF t.font[n] = NIL THEN RAISE Fatal END;
    t.parsePair(x0, y0); (* initial currentpoint *)
    WITH j = Lex.Int(t.rd) (* length of string *) DO
      t.parseChar(' ');

      l := 0;
      WITH f = t.font[n], enc = t.encoding[f.e] DO
        FOR i := 0 TO j - 1 DO
          WITH out = buf[l], in = Rd.GetChar(t.rd) DO
            glyph := enc[in];
            (* If "glyph=0", then "in" mapped to the glyph ".notdef".  This is
               usually a mistake, but we check for several known cases: *)
            IF glyph = 0 THEN
              (* If any element of the current encoding is in the range
                 used by Microsoft TrueType, assume this character is, too. *)
              VAR k := FIRST(enc^); tt := FALSE; BEGIN
                LOOP
                  IF FirstTT1 <= enc[k] AND enc[k] <= LastTT2 THEN
                    tt := TRUE
                  END;
                  IF tt OR k = LAST(enc^) THEN EXIT END;
                  INC(k)
                END;
                IF tt THEN glyph := FirstTT1 + ORD(in)
                ELSIF in = '\r' THEN (* Adobe Illustrator does this... *)
                ELSIF in = '\t' THEN (* MacDraw Pro does this... *)
                ELSIF in = '\032' THEN (* MS Word on Mac does this... *)
                ELSE (* RAISE Fatal ... and lots of other stuff! *)
                END
              END
            END;
            IF glyph = 0 THEN
              (*SKIP*)
            ELSIF glyph <= LastISOLatin1 THEN
              out := VAL(glyph, CHAR);
              IF glyph IN ISOLatin1Gaps THEN out := UnknownChar END;
              INC(l)
            ELSIF glyph <= LAST(SpecialGlyphs) THEN
              WITH str = SpecialGlyphs[glyph], lstr = Text.Length(str) DO
                Text.SetChars(SUBARRAY(buf, l, lstr), str); INC(l, lstr)
              END
            ELSIF glyph <= LastDvips THEN
              IF glyph <= LAST(DvipsGlyphs) THEN
                WITH str = DvipsGlyphs[glyph], lstr = Text.Length(str) DO
                  Text.SetChars(SUBARRAY(buf, l, lstr), str); INC(l, lstr)
                END
              ELSE
                (* E.g., U encoding or Cork encoding *)
                out := UnknownChar; INC(l)
              END
            ELSIF glyph <= LastTT2 THEN
              IF FirstTT2 <= glyph THEN DEC(glyph, FirstTT2-FirstTT1) END;
              IF glyph < FirstTT1+32 THEN out := UnknownChar; INC(l)
              ELSIF glyph < FIRST(TTSpecialGlyphs) OR
                    LAST(TTSpecialGlyphs) < glyph THEN
                out := VAL(glyph - FirstTT1, CHAR); INC(l)
              ELSE
                WITH str = TTSpecialGlyphs[glyph], lstr = Text.Length(str) DO
                  Text.SetChars(SUBARRAY(buf, l, lstr), str); INC(l, lstr)
                END
              END
            ELSIF glyph <= LastOldDvips THEN
              WITH str = DvipsGlyphs[glyph-FirstOldDvips+FirstDvips],
                   lstr = Text.Length(str) DO
                Text.SetChars(SUBARRAY(buf, l, lstr), str); INC(l, lstr)
              END
            ELSIF glyph = NonstandardGlyph THEN (* not in StandardGlyphs *)
              out := UnknownChar; INC(l)
            ELSE RAISE Fatal
            END;
            (* Substitute minus for hyphen. *)
            IF buf[l-1] = '\255' THEN buf[l-1] := '-' END;
          END
        END
      END
    END;

    t.parsePair(x1, y1); (* final currentpoint *)
    IF l # 0 THEN (* l = 0 e.g., when Adobe Illustrator outputs "\r" *)
      IF t.lbuf = 0 THEN SetBuf()
      ELSE
        (* If the distance between this string and the previous one is
           less than "k" times the minimum of the average character
           widths in the two strings, and the two strings are in the
           same direction, then append this string to the previous
           one.  Otherwise, output the previous string and then save
           the current one.

           Sometimes this string overlaps the previous string, e.g.,
           when TeX is overprinting an accent over another character.
           So we make a special case for this (but only handle the
           left-to-right orientation). *)

        (* Set "(xsp,ysp)" to the reporting space coordinates of the
           minimum of the average width of the characters in this string and
           the previous one. *)
        xsp := MIN((t.x1-t.x0) / FLOAT(t.lbuf), (x1-x0) / FLOAT(l));
        ysp := MIN((t.y1-t.y0) / FLOAT(t.lbuf), (y1-y0) / FLOAT(l));

        WITH dx = (x0-t.x1), dy = (y0-t.y1),
             maxx = k * xsp, maxy = k * ysp DO
          IF (dx*dx + dy*dy < maxx*maxx + maxy*maxy
              OR t.y1=y0 AND t.x0 <= t.x1 AND t.x0 <= x0 AND x0 <= t.x1)
             AND SameDirection(t.x1-t.x0, t.y1-t.y0, x1-x0, y1-y0) THEN
            IF t.lbuf+l >= NUMBER(t.buf) THEN RAISE Fatal END;
            SUBARRAY(t.buf, t.lbuf, l) := SUBARRAY(buf, 0, l);
            INC(t.lbuf, l);
            t.x1 := x1; t.y1 := y1;
            (* *** Merge font bounding boxes? *)
          ELSE
            t.output();
            SetBuf()
          END
        END
      END
    END
  END ParseString;

BEGIN
END OCR_PS.
