(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Dec 21 09:56:22 PST 1994 by heydon                   *)

MODULE PklFonts EXPORTS Main;

IMPORT JunoValue, PSFont, PklFontsBundle;
IMPORT Fmt, Rd, Wr, TextRd, Lex, Rsrc, Pickle, Stdio, FloatMode;
IMPORT   TextRefTbl, TextIntSeqTbl, IntSeq;
FROM Thread IMPORT Alerted;

<* FATAL Rd.Failure, Wr.Failure, Alerted, Pickle.Error *>

CONST
  NumFonts = 13;
  NumPtSizes = 6;

TYPE Encoding = TextIntSeqTbl.T;
(* An encoding is a binary relation on names and integers. If "e: Encoding",
   and "e" maps the name "n" to the sequence of integers "i1, i2, ..., iN",
   then the relation contains the pairs "(n, i1)", "(n, i2)", ..., "(n, iN)".
   Usually, there is only one integer in each sequence, but for some fonts,
   the same name may be related to multiple codes. *)

VAR (* READONLY *)
  path: Rsrc.Path;
  isoEncoding, symEncoding, defaultEncoding: Encoding;

EXCEPTION BadMetricFile;

PROCEDURE ReadMetrics(
  nm: TEXT;
  encoding: Encoding;
  VAR (*OUT*) capHeight: JunoValue.Real)
  : PSFont.Metric RAISES {Rd.Failure, BadMetricFile} =
(* Returns a new font metric structure corresponding to the PostScript font
   named "nm", and sets "capHeight" to the height of the font's capital
   letters (measured in points) for a 1-point font. The metric data is read
   from the locally-bundled AFM file named "nm" with the extension ".afm"
   using the character encoding described by the relation "encoding". Raises
   "Rd.Failure" if the file could not be opened, or "BadMetricFile" if the AFM
   file could not be parsed succesfully.

   The only entries in the font metric file processed by this procedure are
   those named "FontBBox", "CapHeight", and "StartCharMetrics". These tokens
   are assumed to start on their own lines. The "FontBBox" entry has the form:

|    FontBBox <west> <south> <east> <north>

   where each of the 4 argument is an integer in thousandths of points.
   Hence, the actual bounding box is computed from these integers for a
   1-point font by dividing them by 1,000. The "CapHeight" entry has the form:

|    CapHeight <height>

   where "height" is the height of a capital letter in thousandths of points.
   The "StartCharMetrics" entry has the form:

|    StartCharMetrics <num>
|    <character-metric-1>
|    ...
|    <character-metric-num>

   where "num" is the number of character metric lines immediately following
   the "StartCharMetrics" line, and the format of the character metric lines
   is described in the "ReadCharMetrics" procedure below. *)
  <* FATAL Rd.EndOfFile, Rsrc.NotFound *>
  CONST
    AFMExt = ".afm";
  VAR
    rd: Rd.T := Rsrc.Open(nm & AFMExt, path);
    res := NEW(PSFont.Metric);
    tRd := NEW(TextRd.T);
    readBBox, readCapHeight, readCharMetrics := FALSE;
  BEGIN
    WHILE NOT Rd.EOF(rd) DO
      EVAL tRd.init(Rd.GetLine(rd));
      IF MatchToken(tRd, "FontBBox") THEN
        WITH bb = res.bbox DO
          bb.west := ScanScaled(tRd); bb.south := ScanScaled(tRd);
          bb.east := ScanScaled(tRd); bb.north := ScanScaled(tRd)
        END;
        readBBox := TRUE
      ELSIF MatchToken(tRd, "CapHeight") THEN
        capHeight := ScanScaled(tRd);
        readCapHeight := TRUE
      ELSIF MatchToken(tRd, "StartCharMetrics") THEN
        ReadCharMetrics(rd, encoding, ScanInt(tRd),
          res.mapped, res.width, res.charBB);
        readCharMetrics := TRUE
      END
    END;
    IF NOT readCapHeight THEN
      WITH bigT = ORD('T') DO
        IF res.mapped[bigT] AND res.charBB[bigT] # NIL THEN
          capHeight := res.charBB[bigT].north;
          readCapHeight := TRUE
        END
      END
    END;
    IF NOT (readBBox AND readCapHeight AND readCharMetrics) THEN
      RAISE BadMetricFile
    END;
    RETURN res
  END ReadMetrics;

PROCEDURE ReadCharMetrics(
    rd: Rd.T;
    encoding: Encoding;
    num: INTEGER;
    VAR (*OUT*) mapped: PSFont.CharMapped;
    VAR (*OUT*) width: PSFont.CharWidth;
    VAR (*OUT*) charBB: PSFont.CharBBox)
    RAISES {Rd.Failure, BadMetricFile} =
(* Reads "num" character metric lines from "rd", setting "mapped[i]" to TRUE
   for each code "i" related to a named character by the character encoding
   "encoding", and setting "width[i]" and "charBB[i]" to the character width
   and bounding box for each such character. After reading "num" such lines,
   the next line must be "EndCharMetrics", or else "BadMetricFile" is raised.

   Each character metric line must have the form:

|    <metrics>  ::= <metric> 
|                 | <metric> <metrics>
|    <metric>   ::= "N" <charName> ";"
|                 | "WX" <width> ";"
|                 | "B" <bbox> ";"
|                 | <any-number-of-non-semicolon-tokens> ";"
|    <bbox>     ::= <west> <south> <east> <north>

   This line asserts that the character with name "charName" has width "width"
   and bounding-box "bbox". The character name is a string, and the character
   width and bounding-box values are integers. If the character name is "nm",
   then "nm" must be in the domain of the relation denoted by "encoding", or
   "BadMetricFile" is raised. For each pair "(nm, code)" in "encoding",
   "mapped[code]" is set to TRUE, and "width[code]" and "charBB[code]" are set
   to the width and bounding box of the character. The character width and
   bounding-box values are expressed in thouandths of points, so they must be
   divided by 1,000 to represent the metrics of the character rendered at
   1-point. *)
  VAR
    tRd := NEW(TextRd.T);
    name: TEXT;				 (* character name *)
    w: JunoValue.Real;			 (* scaled width *)
    sawW, sawBBox: BOOLEAN;		 (* width, bbox specified? *)
    bbox: REF PSFont.BBox;		 (* scaled character bounding box *)
  BEGIN
    TRY
      WHILE num > 0 DO
        (* initialize line *)
        EVAL tRd.init(Rd.GetLine(rd));
        name := NIL; sawW := FALSE; sawBBox := FALSE;

        (* read character info *)
        LOOP
          Lex.Skip(tRd);
          IF Rd.EOF(tRd) THEN EXIT END;
          IF MatchToken(tRd, "C") THEN
            EVAL Lex.Int(tRd);		 (* discard character code *)
          ELSIF MatchToken(tRd, "N") THEN
            Lex.Skip(tRd);
            name := Lex.Scan(tRd);
          ELSIF MatchToken(tRd, "WX") THEN
            sawW := TRUE;
            w := ScanScaled(tRd)
          ELSIF MatchToken(tRd, "B") THEN
            sawBBox := TRUE;
            bbox := NEW(REF PSFont.BBox);
            bbox.west  := ScanScaled(tRd);
            bbox.south := ScanScaled(tRd);
            bbox.east  := ScanScaled(tRd);
            bbox.north := ScanScaled(tRd);
            IF bbox.east <= bbox.west AND
               bbox.north <= bbox.south THEN
              bbox := NIL
            END
          ELSE
            VAR c: CHAR; BEGIN
              TRY REPEAT c := Rd.GetChar(tRd) UNTIL c = ';' EXCEPT
                Rd.EndOfFile => RAISE BadMetricFile
              END;
              Rd.UnGetChar(tRd)
            END
          END;
          Lex.Skip(tRd); Lex.Match(tRd, ";");
        END;

        (* save width and bbox info for this character *)
        VAR seq: IntSeq.T; BEGIN
          IF encoding.get(name, seq) THEN
            IF sawW AND sawBBox THEN
              VAR code: INTEGER; BEGIN
          	FOR i := 0 TO seq.size() - 1 DO
          	  code := seq.get(i);
          	  mapped[code] := TRUE;
          	  width[code] := w;
          	  charBB[code] := bbox
          	END
              END
            ELSE
              RAISE BadMetricFile
            END
          ELSE
            Wr.PutText(Stdio.stderr, "    Unknown character: ");
            Wr.PutText(Stdio.stderr, name);
            Wr.PutChar(Stdio.stderr, '\n');
            Wr.Flush(Stdio.stderr)
          END
        END;
        DEC(num)
      END; (* WHILE *)

      (* check that "EndCharMetrics" is next line *)
      EVAL tRd.init(Rd.GetLine(rd));
      IF NOT MatchToken(tRd, "EndCharMetrics") THEN RAISE BadMetricFile END
    EXCEPT
      Lex.Error, FloatMode.Trap, Rd.EndOfFile => RAISE BadMetricFile
    END;
  END ReadCharMetrics;

PROCEDURE MatchToken(rd: Rd.T; token: TEXT): BOOLEAN =
(* If the prefix of "rd" contains "token" concatenated with a non-trivial
   amount of whitespace, or if it contains "token" followed immediately by
   end-of-file, then skip past the token and any whitespace and return "TRUE".
   Otherwise, return "FALSE" and leave "rd" unchanged. This routine requires
   that "rd" is seekable. *)
  VAR start := Rd.Index(rd); BEGIN
    TRY
      Lex.Match(rd, token);
      VAR end := Rd.Index(rd); BEGIN
        Lex.Skip(rd, Lex.Blanks);
        IF Rd.Index(rd) = end AND NOT Rd.EOF(rd) THEN RAISE Lex.Error END
      END
    EXCEPT Lex.Error =>
      Rd.Seek(rd, start);
      RETURN FALSE
    END;
    RETURN TRUE
  END MatchToken;

PROCEDURE ScanScaled(rd: Rd.T): REAL RAISES {BadMetricFile} =
(* Read an integer from "rd", first skipping whitespace on "rd", and return
   the value of the integer divided by 1,000. *)
  VAR val := ScanInt(rd); BEGIN
    RETURN FLOAT(val, REAL) / 1000.0
  END ScanScaled;

PROCEDURE ScanInt(rd: Rd.T): INTEGER RAISES {BadMetricFile} =
(* Read and return an integer from "rd", first skipping whitespace on "rd".
   Raises "BadMetricFile" if an integer could not be read. *)
  VAR res: INTEGER; BEGIN
    TRY res := Lex.Int(rd) EXCEPT
      Lex.Error, FloatMode.Trap => RAISE BadMetricFile
    END;
    RETURN res
  END ScanInt;

VAR
  fontTbl := NEW(TextRefTbl.Default).init(sizeHint := NumPtSizes * NumFonts);
  metricTbl := NEW(TextRefTbl.Default).init(sizeHint := NumFonts);
  fontData := NEW(PSFont.Data, fontTbl := fontTbl, metricTbl := metricTbl);

TYPE
  Size = [0..5];
  CapHeights = ARRAY Size OF CARDINAL;

CONST
  YScale = 72.0 / 104.2; (* points / pixel *)
  (* This is the vertical scale factor for converting from screen-dependent
     units (pixels) to screen-independent units (points) on a display with a
     vertical resolution of 102.4 pixels / inch (as reported by the xdpyinfo(1)
     command). This value must correspond to the display on which the X font
     sizes that appear in the "CapHeights" arrays below were measured. *)

CONST
  TimesHeights     = CapHeights{7, 10, 11, 13, 17, 23};
  HelveticaHeights = CapHeights{8, 11, 12, 14, 19, 25};
  CourierHeights1  = CapHeights{6,  9, 10, 11, 14, 19};
  CourierHeights2  = CapHeights{6,  9, 10, 11, 15, 20};
  SymbolHeights    = TimesHeights;

(* These values are the heights of a capital letter in pixels at each of the 6
   different X font sizes (8, 10, 12, 14, 18, 24). Their sizes in the
   screen-independent units of points are therefore the pixel size times
   the constant "YScale". *)

PROCEDURE AddFont(ps, x: TEXT; READONLY ch: CapHeights;
  encoding: Encoding := NIL) =
(* Binds the name "ps" to the metric data for the font with PostScript name
   "ps" (e.g., "Times-Roman") in the global table "metricTbl" using the
   character encoding "encoding" (or "defaultEncoding" if "encoding" is NIL);
   for each of the font's sizes "i" (in the range "[0..5]"), binds the name
   "ps & i" (e.g., "Times-Roman2") to the name of the corresponding X font in
   the global table "fontTbl". The name of the corresponding X font is formed
   from "x" and the font's point size: "x" must be the "-"-separated string
   consisting of the X font family, weight, and slant for the PostScript font
   "ps" (e.g., "times-bold-i"). *)
  CONST
    Sizes = ARRAY OF INTEGER{8, 10, 12, 14, 18, 24};
    Prefix = "-*-"; Middle = "-normal-*-*-"; Suffix = "0-*-*-*-*-*-*";
  VAR psCapHeight: JunoValue.Real; BEGIN
    Wr.PutText(Stdio.stderr, "  " & ps & "\n"); Wr.Flush(Stdio.stderr);
    IF encoding = NIL THEN encoding := defaultEncoding END;
    <* FATAL Rd.Failure, BadMetricFile *> BEGIN
      EVAL metricTbl.put(ps, ReadMetrics(ps, encoding, psCapHeight))
    END;
    FOR i := FIRST(Sizes) TO LAST(Sizes) DO
      WITH sz = Fmt.Int(i), ptSz = Fmt.Int(Sizes[i]) DO
        VAR xInfo := NEW(PSFont.XInfo); BEGIN
          (* The size for the corresponding PostScript font is computed by
             first converting the height of the screen font's capital letters
             from pixels to points, and then dividing by the height of capital
             letters in the corresponding 1-point PostScript font. *)
          xInfo.name := Prefix & x & Middle & ptSz & Suffix;
          xInfo.ptSize := (FLOAT(ch[i], JunoValue.Real)*YScale) / psCapHeight;
          EVAL fontTbl.put(ps & sz, xInfo)
        END
      END
    END
  END AddFont;

PROCEDURE ReadEncoding(filename: TEXT): Encoding =
(* Create and return a new character encoding as described in the file
   "filename". In this file, any blank line or line beginning with a "#" is
   ignored. All other lines should be of the form:

|    <name> <whitespace> <octal-code>

   If the <octal-code> is a non-negative number, this has the effect of
   relating <name> to the integer with octal value <octal-code> in the
   resulting encoding. *)
  <* FATAL Rd.EndOfFile, Rsrc.NotFound, FloatMode.Trap, Lex.Error *>
  VAR
    res := NEW(TextIntSeqTbl.Default).init(sizeHint := 210);
    rd: Rd.T := Rsrc.Open(filename, path);
    tRd := NEW(TextRd.T);
    name: TEXT;
    value: INTEGER;
    seq: IntSeq.T;
  BEGIN
    WHILE NOT Rd.EOF(rd) DO
      EVAL tRd.init(Rd.GetLine(rd));
      IF NOT Rd.EOF(tRd) AND Rd.GetChar(tRd) # '#' THEN
        Rd.UnGetChar(tRd);
        name := Lex.Scan(tRd);
        Lex.Skip(tRd);
        value := Lex.Int(tRd, defaultBase := 8);
        IF NOT res.get(name, seq) THEN
          seq := NEW(IntSeq.T).init(sizeHint := 1);
          EVAL res.put(name, seq)
        END;
        IF value >= 0 THEN seq.addhi(value) END
      END
    END;
    RETURN res
  END ReadEncoding;

BEGIN
  (* initialize resource path *)
  path := Rsrc.BuildPath("$PklFontsPATH", PklFontsBundle.Get());

  (* read encodings *)
  isoEncoding := ReadEncoding("ISOLatin1Encoding");
  symEncoding := ReadEncoding("SymbolEncoding");
  defaultEncoding := isoEncoding;

  (* fill in "fontTbl" and "metricTbl" *)
  Wr.PutText(Stdio.stderr, "Building font metrics...\n");
  AddFont("Times-Roman",           "times-medium-r",     TimesHeights);
  AddFont("Times-Bold",            "times-bold-r",       TimesHeights);
  AddFont("Times-Italic",          "times-medium-i",     TimesHeights);
  AddFont("Times-BoldItalic",      "times-bold-i",       TimesHeights);
  AddFont("Helvetica",             "helvetica-medium-r", HelveticaHeights);
  AddFont("Helvetica-Bold",        "helvetica-bold-r",   HelveticaHeights);
  AddFont("Helvetica-Oblique",     "helvetica-medium-o", HelveticaHeights);
  AddFont("Helvetica-BoldOblique", "helvetica-bold-o",   HelveticaHeights);
  AddFont("Courier",               "courier-medium-r",   CourierHeights1);
  AddFont("Courier-Bold",          "courier-bold-r",     CourierHeights2);
  AddFont("Courier-Oblique",       "courier-medium-o",   CourierHeights1);
  AddFont("Courier-BoldOblique",   "courier-bold-o",     CourierHeights2);
  AddFont("Symbol",                "symbol-medium-r",    SymbolHeights,
    encoding := symEncoding);

  (* write out the pickle *)
  Wr.PutText(Stdio.stderr, "Pickling font metrics...\n");
  Wr.Flush(Stdio.stderr);
  Pickle.Write(Stdio.stdout, fontData);
  Wr.Close(Stdio.stdout);
  Wr.PutText(Stdio.stderr, "Done.\n");
END PklFonts.
