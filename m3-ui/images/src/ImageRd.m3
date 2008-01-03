(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: interface for accessing images stored in readers *)

(* Last modified on Mon Mar 27 15:10:37 PST 1995 by birrell   *)

UNSAFE MODULE ImageRd; (* for loophole in pixel copying inner loop *)

IMPORT Bundle, Fmt, Images, ImagesBundle, IntIntTbl, Math, PaintOp,
       PaintPrivate, Point, Rd, Rect, ScreenType, ScrnColorMap, ScrnPixmap,
       Thread, Time, TrestleComm, VBT, Word, Wr;


(* *)
(* Types *)
(* *)

REVEAL T = Public BRANDED OBJECT
    (* Locking is somewhat complex.  This is to allow decompression and
       painting to be interrupted by Alerts or by a call of the init method,
       and to allow access to the image's domain without being blocked by
       a decompression or painting operation.  Additonally, the global rdMu
       allows a single reader to be used in multiple images *)
    (* Locking order: t.mu < rdMu < { t.domMu, t.incMu } *)
    mu: MUTEX := NIL;               (* for everything except incarnation *)
    incMu: MUTEX := NIL;            (* for incarnation *)
    domMu: MUTEX := NIL;            (* for domainValid and cachedDomain *)
    rd: Rd.T;
    start, length: CARDINAL;
    op: PaintOp.T;
    incarnation := 0;               (* incremented by .init *)
    domainValid := FALSE;           (* whether cached domain is valid *)
    cachedDomain: Rect.T;
    rawValid: BOOLEAN;              (* whether a non-NIL .raw has pixels for
                                       current .st and .gamma *)
    raw: ScrnPixmap.Raw := NIL;     (* cached screen-dependent pixels *)
    st: VBT.ScreenType;             (* screentype for which "raw" is valid *)
    pixelToRGB: Images.RGBMap := NIL; (* RGB map for .raw if .st=NIL; this
                                       gets set by each image format subroutine
                                       either by explicit code, or by calling
                                       .isBitmap or GetGrayToPixel *)
    isBW, isGray, isGrayRamp: BOOLEAN; (* flags about .pixelToRGB *)
    format: Format;                 (* result of header parsing *)
    cooked: ScrnPixmap.T := NIL;    (* cached copy of raw loaded into st *)
    gamma: REAL;                    (* gamma correction used in "raw" *)
    data: REF ARRAY OF CHAR := NIL; (* serially reusable input buffer *)
  METHODS
    isBitmap() := IsBitmap;         (* update t.op and set t.pixelToRGB *)
  OVERRIDES
    init := Init;
    domain := Domain;
    paint := Paint;
    render := Render;
    contents := GetContents;
    close := Close;
    toEPSF := ToEPSF
  END;


(* *)
(* Color maps and screen pixels.  All access to the VBT.ScreenType is here. *)
(* *)

TYPE SrcToPixel = REF ARRAY OF ScrnColorMap.Pixel;
  (* Array indexed by gray-scale value, yielding screen pixel *)

TYPE RGBToPixel = IntIntTbl.Default;
  (* Table keyed by RGB value, yielding screen pixel *)

VAR
  cachedRGBMu := NEW(Thread.Mutex);
  cachedRGBtoPixel: RGBToPixel := NIL;
  cachedRGBtoPixelSt: VBT.ScreenType;
  cachedRGBtoPixelGamma: REAL;

PROCEDURE GetRGBToPixel(t: T): RGBToPixel =
  (* Returns a RGB-to-pixel table for t.st and t.gamma, using cached one if
     possible. *)
  VAR rgbToPixel: RGBToPixel;
  BEGIN
    LOCK cachedRGBMu DO
      IF t.st = NIL THEN
        rgbToPixel := NEW(RGBToPixel).init();
      ELSIF cachedRGBtoPixel = NIL OR cachedRGBtoPixelSt # t.st OR
                                  cachedRGBtoPixelGamma # t.gamma THEN
        rgbToPixel := NEW(RGBToPixel).init();
        cachedRGBtoPixel := rgbToPixel;
        cachedRGBtoPixelSt := t.st;
        cachedRGBtoPixelGamma := t.gamma;
      ELSE
        rgbToPixel := cachedRGBtoPixel;
      END;
    END;
    RETURN rgbToPixel
  END GetRGBToPixel;

PROCEDURE ConvertRGBToPixel(t: T; rgb: Images.RGB): INTEGER
                            RAISES { Images.Error } =
  (* Converts an RGB into a pixel for t.st *)
  VAR
    realRGB := ScrnColorMap.RGB{r := FLOAT(rgb.r) / 255.0,
                                g := FLOAT(rgb.g) / 255.0,
                                b := FLOAT(rgb.b) / 255.0};
    cmap: ScrnColorMap.T;
  BEGIN
    <*ASSERT t.st # NIL*> (* That case should be implemented by caller *)
    TRY
      IF t.gamma # 1.0 THEN
        realRGB.r := FLOAT(Math.pow(FLOAT(realRGB.r, LONGREAL),
                                    FLOAT(1.0/t.gamma, LONGREAL)));
        realRGB.g := FLOAT(Math.pow(FLOAT(realRGB.g, LONGREAL),
                                    FLOAT(1.0/t.gamma, LONGREAL)));
        realRGB.b := FLOAT(Math.pow(FLOAT(realRGB.b, LONGREAL),
                                    FLOAT(1.0/t.gamma, LONGREAL)));
      END;
      cmap := t.st.cmap.standard();
      IF cmap = NIL THEN
        IF Images.GrayFromRGB(rgb) < 128 THEN
          RETURN t.st.fg
        ELSE
          RETURN t.st.bg
        END;
      ELSE
        TRY
          RETURN cmap.fromRGB(realRGB, ScrnColorMap.Mode.Accurate);
        EXCEPT ScrnColorMap.Failure =>
          TRY
            RETURN cmap.fromRGB(realRGB, ScrnColorMap.Mode.Normal);
          EXCEPT ScrnColorMap.Failure =>
            TRY
              realRGB.r := FLOAT(Images.GrayFromRGB(rgb)) / 255.0;
              realRGB.g := realRGB.r;
              realRGB.b := realRGB.r;
              RETURN cmap.fromRGB(realRGB, ScrnColorMap.Mode.Normal);
            EXCEPT ScrnColorMap.Failure =>
              IF realRGB.r < 0.5 THEN RETURN t.st.fg ELSE RETURN t.st.bg END;
            END;
          END;
        END;
      END;
    EXCEPT TrestleComm.Failure =>
      RAISE Images.Error("Can't contact display server")
    END;
  END ConvertRGBToPixel;

PROCEDURE LookupRGBToPixel(t: T;
                           rgb: Images.RGB;
                           rgbToPixel: RGBToPixel;
                           nextValue: INTEGER): ScrnPixmap.Pixel
                           RAISES { Images.Error } =
    (* Looks up "rgb" in "rgbToPixel"; if found, return it; else
       get screen pixel (for t.st#NIL) or use "nextValue" (for t.st=NIL). *)
  VAR
    rgbInt := rgb.r*65536 + rgb.g*256 + rgb.b;
    pixel: ScrnPixmap.Pixel;
  BEGIN
    IF NOT rgbToPixel.get(rgbInt, pixel) THEN
      IF t.st = NIL THEN
        pixel := nextValue;
      ELSE
        pixel := ConvertRGBToPixel(t, rgb);
      END;
      EVAL rgbToPixel.put(rgbInt, pixel);
    END;
    RETURN pixel;
  END LookupRGBToPixel;

VAR
  cachedGrayMu := NEW(Thread.Mutex);
  cachedGrayToPixel: SrcToPixel := NIL;
  cachedGrayToPixelSt: VBT.ScreenType;
  cachedGrayToPixelGamma: REAL;

PROCEDURE GetGrayToPixel(t: T; maxVal: INTEGER): SrcToPixel
                         RAISES{ Images.Error } =
    (* Returns a table mapping grayscale pixels in [0..maxVal] into pixels on
       t.st. *)
  VAR
    values: SrcToPixel;
    rgbToPixel: RGBToPixel;
    gray: Images.Channel;
  BEGIN
    <*ASSERT t.st # NIL*> (* That case should be implemented by caller *)
    LOCK cachedGrayMu DO
      IF cachedGrayToPixelSt = t.st AND cachedGrayToPixel # NIL AND
                                 LAST(cachedGrayToPixel^) = maxVal AND
                                 cachedGrayToPixelGamma = t.gamma THEN
        values := cachedGrayToPixel;
      ELSE
        values := NEW(SrcToPixel, maxVal+1);
        rgbToPixel := GetRGBToPixel(t);
        FOR i := 0 TO maxVal DO
          gray := ROUND((FLOAT(i) / FLOAT(maxVal)) * 255.0);
          values[i] := LookupRGBToPixel(t,
                                        Images.RGB{r := gray,
                                                   g := gray,
                                                   b := gray},
                                        rgbToPixel,
                                        0);
        END;
        cachedGrayToPixel := values;
        cachedGrayToPixelSt := t.st;
        cachedGrayToPixelGamma := t.gamma;
      END;
    END; (* LOCK*)
    RETURN values
  END GetGrayToPixel;

PROCEDURE ExpandRaw (res: ScrnPixmap.Raw; READONLY bnds: Rect.T): ScrnPixmap.Raw RAISES {} =
  (* expand the array of words if necessary and reset the bounds rectangle,
     but don't change the depth. This avoids a NEW if the array is already big
     enough, thereby avoiding thrashing the GC. *)
  CONST WordSize = BITSIZE (ScrnPixmap.PixWord);
  VAR
    pixPerWord  := WordSize DIV res.depth;
    westRounded := bnds.west - (bnds.west MOD pixPerWord);
    pixPerRow   := bnds.east - westRounded;
    wordsPerRow := (pixPerRow + pixPerWord - 1) DIV pixPerWord;
    nWords      := wordsPerRow * Rect.VerSize (bnds);
  BEGIN
    IF (res.pixels = NIL) OR (NUMBER (res.pixels^) < nWords) THEN
      res.pixels := NEW(REF ARRAY OF ScrnPixmap.PixWord, nWords);
    END;
    res.bounds      := bnds;
    res.offset      := 0;
    res.wordsPerRow := wordsPerRow;
    res.westRounded := westRounded;
    IF pixPerWord = 1
      THEN res.bitsPerPixel := WordSize;
      ELSE res.bitsPerPixel := res.depth;
    END;
    RETURN res;
  END ExpandRaw;

PROCEDURE NewRaw(t: T; bitmap: BOOLEAN;
                 maxVal, width, height: INTEGER): ScrnPixmap.Raw =
  (* Returns a raw pixmap of appropriate size and depth for displaying on
     t.st, or if t.st=NIL then for pixels with maximum value "maxVal" *)
  VAR
    bounds := Rect.FromSize(width, height);
    depth: INTEGER;
  BEGIN
    IF t.st = NIL THEN
      IF maxVal <= 1 THEN
        depth := 1;
      ELSIF maxVal <= 3 THEN
        depth := 2;
      ELSIF maxVal <= 15 THEN
        depth := 4;
      ELSIF maxVal <= 255 THEN
        depth := 8;
      ELSIF maxVal <= 65535 THEN
        depth := 16;
      ELSE
        depth := 32;
      END;
    ELSIF bitmap THEN
      depth := 1;
    ELSE
      depth := t.st.depth;
    END;
    IF t.raw # NIL AND t.raw.depth = depth AND
                       t.raw.bounds.west = bounds.west AND
                       t.raw.bounds.east = bounds.east AND
                       t.raw.bounds.north = bounds.north AND
                       t.raw.bounds.south = bounds.south THEN
      (* Avoid the cost of allocating the memory *)
      RETURN t.raw
    ELSIF (t.raw # NIL) AND (t.raw.depth = depth) THEN
      RETURN ExpandRaw(t.raw, bounds);
    ELSE
      RETURN ScrnPixmap.NewRaw(depth, bounds)
    END;
  END NewRaw;


(* *)
(* Bit fiddling for bitmaps *)
(* *)

VAR reversedBits, plainBits, pbmToHost, lgmToHost: ARRAY CHAR OF CHAR;
  (* reverses bit order: m.s.first  to l.s. first, or vice versa *)

PROCEDURE InitReversedBits() =
  VAR b: Word.T;
  BEGIN
    FOR i := 0 TO 255 DO
      plainBits[VAL(i,CHAR)] := VAL(i, CHAR);
      b := 0;
      FOR j := 0 TO 7 DO
        b := Word.Insert(b, Word.Extract(i, j, 1), 7-j, 1)
      END;
      reversedBits[VAL(i,CHAR)] := VAL(b, CHAR);
    END;
  END InitReversedBits;


(* *)
(* Subroutines for parsing PNM files *)
(* *)

PROCEDURE ScanInt(rd: Rd.T): INTEGER
                  RAISES{Thread.Alerted, Rd.Failure, Images.Error} =
  (* Return value of next integer in rd; skip any
     whitespace or comments before the first digit. *)
  (* Stolen from Image.m3 by Steve Glassman *)
  CONST
    Digits = SET OF CHAR{'0'.. '9'};
    Spaces  = SET OF CHAR{' ', '\t', '\n', '\r'};
    Comment = '#';
  VAR
    res: INTEGER;
    ch : CHAR;
  BEGIN
    TRY
      ch := Rd.GetChar(rd);
      WHILE (ch = Comment) OR (ch IN Spaces) DO
        IF ch = Comment THEN EVAL Rd.GetLine(rd) END;
        ch := Rd.GetChar(rd)
      END;
    EXCEPT Rd.EndOfFile =>
      RAISE Images.Error("Syntax error in PNM file header")
    END;
    IF ch IN Digits THEN
      res := ORD(ch) - ORD('0')
    ELSE
      RAISE Images.Error("Syntax error in PNM file header")
    END;
    TRY
      LOOP
        ch := Rd.GetChar(rd);
        IF ch IN Digits THEN
          res := 10 * res + ORD(ch) - ORD('0')
        ELSE
          EXIT
        END
      END;
    EXCEPT
    | Rd.EndOfFile =>
    END;
    Rd.UnGetChar(rd);
    RETURN res
  END ScanInt;

PROCEDURE GetScanLine(rd: Rd.T; scanLine: REF ARRAY OF CHAR; maxVal: INTEGER;
                      binary: BOOLEAN)
                      RAISES { Thread.Alerted, Rd.Failure, Images.Error} =
  (* Get a scan line by calling ScanInt or Rd.GetSub, depending on "binary". *)
  BEGIN
    IF binary THEN
      EVAL Rd.GetSub(rd, scanLine^);
    ELSE
      FOR h := 0 TO LAST(scanLine^) DO
        scanLine[h] := VAL((ScanInt(rd)*255) DIV maxVal, CHAR);
      END;
    END;
  END GetScanLine;


(* *)
(* PNM and LGM reading routines *)
(* *)

VAR rdMu := NEW(MUTEX);
  (* LL: rdMu > t *)
  (* Global lock on using readers, so one reader can be in multipl T's *)

PROCEDURE GetHeader(t: T;
                    VAR bodyStart, width, height: INTEGER;
                    VAR binary, hasMap: BOOLEAN): Format
                    RAISES{ Rd.Failure, Thread.Alerted, Images.Error } =
  (* LL = t *)
  (* Read a PNM or LGM header, leaving reader at end of header *)
  (* Sets t.format, t.cachedDomain and t.domainValid; ignores t.st *)
  (* Assigns final reader position to "bodyStart" *)
  VAR class, mode: CHAR; rd := t.rd; format: Format;
  BEGIN
    LOCK rdMu DO
      TRY
        IF Rd.Index(rd) # t.start THEN Rd.Seek(rd, t.start) END;
        class := Rd.GetChar(rd);
        mode := Rd.GetChar(rd);
        width := ScanInt(rd);
        height := ScanInt(rd);
        IF class = 'P' THEN
          CASE mode OF
          | '1' => binary := FALSE; format := Format.PBM;
          | '2' => binary := FALSE; format := Format.PGM;
          | '3' => binary := FALSE; format := Format.PPM;
          | '4' => binary := TRUE; format := Format.PBM;
          | '5' => binary := TRUE; format := Format.PGM;
          | '6' => binary := TRUE; format := Format.PPM;
          ELSE
              RAISE Images.Error("Unknown image file format")
          END (*   ELSIF *)
        ELSIF class = 'L' AND (mode = 'G' OR mode = 'M') THEN
          binary := TRUE; hasMap := (mode='M'); format := Format.LGM;
        ELSE
          RAISE Images.Error("Unknown image file format")
        END;
        bodyStart := Rd.Index(rd);
      EXCEPT Rd.EndOfFile =>
        RAISE Images.Error("End-of-file in image file header");
      END;
    END;
    t.format := format;
    LOCK t.domMu DO
      t.cachedDomain := Rect.FromSize(width, height);
      t.domainValid := TRUE;
    END;
    RETURN format
  END GetHeader;

PROCEDURE pbm(t: T; bodyStart, width, height: INTEGER;
              binary: BOOLEAN): ScrnPixmap.Raw
              RAISES {Thread.Alerted, Rd.Failure, Images.Error} =
  (* LL = t *)
  VAR rd := t.rd;
  VAR raw := NewRaw(t, TRUE, 1, width, height);
  VAR word: Word.T;
  VAR scanLine: REF ARRAY OF CHAR;
  BEGIN
    LOCK rdMu DO
      TRY
        IF Rd.Index(rd) # bodyStart THEN Rd.Seek(rd, bodyStart) END;
        IF binary THEN
          WHILE Rd.GetChar(rd) # '\n' DO END;
          scanLine := NEW(REF ARRAY OF CHAR, (width+7) DIV 8);
        END;
        FOR v := 0 TO height - 1 DO
          IF binary THEN
            GetScanLine(rd, scanLine, 255, binary);
            IF (raw.bitsPerPixel = 1) AND (raw.westRounded = 0) AND
              (raw.pixelOrder = PaintPrivate.HostByteOrder) THEN
              (* Optimized case *)
              WITH
                rowStart = (v - raw.bounds.north) * raw.wordsPerRow+raw.offset,
                row = LOOPHOLE(ADR(raw.pixels[rowStart]),
                               UNTRACED REF ARRAY [0..999999] OF CHAR) DO
                FOR h := 0 TO (width+7) DIV 8 - 1 DO
                  row[h] := pbmToHost[scanLine[h]];
                END;
              END
            ELSE
              FOR h := 0 TO width - 1 DO
                IF h MOD 8 = 0 THEN word := ORD(scanLine[h DIV 8]) END;
                raw.set(Point.T{h, v}, ORD(Word.And(word, 128) # 0));
                word := Word.LeftShift(word, 1);
              END;
            END;
          ELSE
            FOR h := 0 TO width-1 DO
              LOOP
                WITH c = Rd.GetChar(rd) DO
                  IF c = '0' THEN
                    raw.set(Point.T{h, v}, 0);
                    EXIT
                  ELSIF c = '1' THEN
                    raw.set(Point.T{h, v}, 1);
                    EXIT
                  ELSIF c = '#' THEN
                    EVAL Rd.GetLine(rd);
                  END;
                END;
              END;
            END;
          END;
        END;
      EXCEPT Rd.EndOfFile =>
        RAISE Images.Error("Unexpected end-of-file in PBM file")
      END;
    END;
    t.isBitmap();
    RETURN raw
  END pbm;

PROCEDURE pgm(t: T; bodyStart, width, height: INTEGER;
              binary: BOOLEAN): ScrnPixmap.Raw
             RAISES {Thread.Alerted, Rd.Failure, Images.Error} =
  (* LL = t *)
  VAR
    rd := t.rd;
    maxVal: INTEGER;
    raw: ScrnPixmap.Raw;
    pixelToRGB: Images.RGBMap;
    srcToPixel: SrcToPixel;
    nextValue: INTEGER := 0;
    scanLine := NEW(REF ARRAY OF CHAR, width);
    isBW := TRUE;
  CONST
    NoValue = -1;
  PROCEDURE MapGray(pos: INTEGER): ScrnPixmap.Pixel =
    VAR
      pixel: ScrnPixmap.Pixel;
      g := ORD(scanLine[pos]);
    BEGIN
      pixel := srcToPixel[g];
      IF pixel = NoValue AND t.st = NIL THEN
        pixel := nextValue; INC(nextValue);
        srcToPixel[g] := pixel;
        WITH gray = ROUND((FLOAT(g) / FLOAT(maxVal)) * 255.0) DO
          pixelToRGB[pixel] := Images.RGB{r := gray, g := gray, b := gray};
          IF gray # 0 AND gray # 255 THEN isBW := FALSE END;
        END;
      END;
      RETURN pixel;
    END MapGray;
  BEGIN
    LOCK rdMu DO
      IF Rd.Index(rd) # bodyStart THEN Rd.Seek(rd, bodyStart) END;
      maxVal := ScanInt(rd);
      raw := NewRaw(t, FALSE, maxVal, width, height);
      pixelToRGB := NEW(Images.RGBMap, maxVal+1);
      IF t.st = NIL THEN
        (* Keep track carefully, to minimize resulting Images.RGBMap *)
        srcToPixel := NEW(SrcToPixel, maxVal+1);
        FOR i := 0 TO LAST(srcToPixel^) DO srcToPixel[i] := NoValue END;
      ELSE
        (* Just use a gray ramp *)
        srcToPixel := GetGrayToPixel(t, maxVal);
      END;
      TRY
        IF binary THEN
          maxVal := MIN(maxVal, 255);
          WHILE Rd.GetChar(rd) # '\n' DO END;
        END;
        FOR v := 0 TO height - 1 DO
          GetScanLine(rd, scanLine, maxVal, binary);
          IF (raw.bitsPerPixel = 8) AND (raw.westRounded = 0) AND
            (raw.pixelOrder = PaintPrivate.HostByteOrder) THEN
            (* Optimized case *)
            WITH
              rowStart = (v - raw.bounds.north) * raw.wordsPerRow + raw.offset,
              row = LOOPHOLE(ADR(raw.pixels[rowStart]),
                             UNTRACED REF ARRAY [0..999999] OF CHAR)
            DO
              FOR h := 0 TO width-1 DO row[h] := VAL(MapGray(h), CHAR) END;
            END
          ELSE
            FOR h := 0 TO width - 1 DO raw.set(Point.T{h,v}, MapGray(h)) END;
          END;
        END;
      EXCEPT Rd.EndOfFile =>
        RAISE Images.Error("Unexpected end-of-file in PGM file")
      END;
    END;
    IF t.st = NIL THEN
      t.pixelToRGB := NEW(Images.RGBMap, nextValue);
      t.pixelToRGB^ := SUBARRAY(pixelToRGB^, 0, nextValue);
      t.isBW := isBW;
      t.isGray := TRUE;
      t.isGrayRamp := FALSE;
    END;
    RETURN raw
  END pgm;

PROCEDURE ppm(t: T; bodyStart, width, height: INTEGER;
              binary: BOOLEAN): ScrnPixmap.Raw
             RAISES {Thread.Alerted, Rd.Failure, Images.Error} =
  (* LL = t *)
  VAR
    rd := t.rd;
    maxVal: INTEGER;
    rgbToPixel: RGBToPixel;
    nextValue: INTEGER := 0;
    raw := NewRaw(t, FALSE, width * height - 1, width, height);
    scanLine := NEW(REF ARRAY OF CHAR, width*3);
    pixelToRGB := NEW(Images.RGBMap, 256);
    isBW, isGray := TRUE;
  PROCEDURE MapRGB(pos: INTEGER): ScrnPixmap.Pixel RAISES { Images.Error } =
    VAR
      pixel: ScrnPixmap.Pixel;
      rgb := Images.RGB{ ORD(scanLine[pos]),
                         ORD(scanLine[pos+1]),
                         ORD(scanLine[pos+2]) };
    BEGIN
      IF maxVal # 255 THEN
        rgb.r := ROUND((FLOAT(rgb.r) / FLOAT(maxVal)) * 255.0);
        rgb.g := ROUND((FLOAT(rgb.g) / FLOAT(maxVal)) * 255.0);
        rgb.b := ROUND((FLOAT(rgb.b) / FLOAT(maxVal)) * 255.0);
      END;
      pixel := LookupRGBToPixel(t, rgb, rgbToPixel, nextValue);
      IF pixel = nextValue AND t.st = NIL THEN
        INC(nextValue);
        IF pixel > LAST(pixelToRGB^) THEN
          WITH new = NEW(Images.RGBMap, NUMBER(pixelToRGB^)*2) DO
            SUBARRAY(new^, 0, NUMBER(pixelToRGB^)) := pixelToRGB^;
            pixelToRGB := new;
          END;
        END;
        pixelToRGB[pixel] := rgb;
        IF isBW OR isGray THEN
          IF rgb.r # rgb.g OR rgb.g # rgb.b THEN
            isBW := FALSE;
            isGray := FALSE;
          ELSIF rgb.r # 0 AND rgb.r # 255 THEN
            isBW := FALSE;
          END;
        END;
      END;
      RETURN pixel;
    END MapRGB;
  BEGIN
    LOCK rdMu DO
      IF Rd.Index(rd) # bodyStart THEN Rd.Seek(rd, bodyStart) END;
      maxVal := ScanInt(rd);
      rgbToPixel := GetRGBToPixel(t);
      TRY
        IF binary THEN
          maxVal := MIN(maxVal, 255);
          WHILE Rd.GetChar(rd) # '\n' DO END;
        END;
        FOR v := 0 TO height - 1 DO
          GetScanLine(rd, scanLine, maxVal, binary);
          IF (raw.bitsPerPixel = 8) AND (raw.westRounded = 0) AND
             (raw.pixelOrder = PaintPrivate.HostByteOrder) THEN
            (* Optimized case *)
            WITH
              rowStart = (v - raw.bounds.north) * raw.wordsPerRow + raw.offset,
              row = LOOPHOLE(ADR(raw.pixels[rowStart]),
                             UNTRACED REF ARRAY [0..999999] OF CHAR)
            DO
              FOR h := 0 TO width-1 DO
                row[h] := VAL(MapRGB(h*3), CHAR);
              END;
            END
          ELSE
            FOR h := 0 TO width - 1 DO
              raw.set(Point.T{h,v}, MapRGB(h*3));
            END;
          END;
        END;
      EXCEPT
        Rd.EndOfFile => RAISE Images.Error("Unexpected end-of-file in PPM")
      END;
    END;
    IF t.st = NIL THEN
      t.pixelToRGB := NEW(Images.RGBMap, nextValue);
      t.pixelToRGB^ := SUBARRAY(pixelToRGB^, 0, nextValue);
      t.isBW := isBW;
      t.isGray := isGray;
      t.isGrayRamp := FALSE;
    END;
    RETURN raw
  END ppm;

PROCEDURE ComputePacking(maxVal: INTEGER): INTEGER =
  VAR pixelsPerByte: INTEGER;
  BEGIN
    IF maxVal <= 1 THEN
      pixelsPerByte := 8;
    ELSIF maxVal <= 2 THEN
      pixelsPerByte := 5;
    ELSIF maxVal <= 3 THEN
      pixelsPerByte := 4;
    ELSIF maxVal <= 5 THEN
      pixelsPerByte := 3;
    ELSIF maxVal <= 15 THEN
      pixelsPerByte := 2;
    ELSE
      pixelsPerByte := 1;
    END;
    RETURN pixelsPerByte
  END ComputePacking;

PROCEDURE ReadLGMData(t: T;
                      bodyStart: INTEGER;
                      hasMap: BOOLEAN;
                      VAR scale, maxVal, dataLength: INTEGER;
                      VAR srcRGBMap: Images.RGBMap): REF ARRAY OF CHAR
                      RAISES { Thread.Alerted, Rd.Failure, Images.Error } =
  (* LL = t *)
  VAR
    data: REF ARRAY OF CHAR;
  BEGIN
    LOCK rdMu DO
      TRY
        IF Rd.Index(t.rd) # bodyStart THEN Rd.Seek(t.rd, bodyStart) END;
        scale := ScanInt(t.rd);
        maxVal := MIN(ScanInt(t.rd), ORD(LAST(CHAR)));
        WHILE Rd.GetChar(t.rd) # '\n' DO END;
        IF hasMap THEN
          srcRGBMap := NEW(Images.RGBMap, maxVal+1);
          WITH mapData = NEW(REF ARRAY OF CHAR, 3 * NUMBER(srcRGBMap^)) DO
            EVAL Rd.GetSub(t.rd, mapData^);
            FOR i := 0 TO LAST(srcRGBMap^) DO
              srcRGBMap[i] := Images.RGB{r := ORD(mapData[i*3]),
                                         g := ORD(mapData[i*3+1]),
                                         b := ORD(mapData[i*3+2])};
            END;
          END;
        ELSE
          srcRGBMap := NIL;
        END;
        dataLength := t.length - (Rd.Index(t.rd) - t.start);
        IF t.data # NIL AND NUMBER(t.data^) >= dataLength THEN
          data := t.data;
        ELSE
          data := NEW(REF ARRAY OF CHAR, dataLength);
          t.data := data;
        END;
        EVAL Rd.GetSub(t.rd, SUBARRAY(data^, 0, dataLength));      
      EXCEPT
      | Rd.EndOfFile => RAISE Images.Error("Unexpected end-of-file in LGM file")
      END;
    END;
    RETURN data;
  END ReadLGMData;

PROCEDURE lgm(t: T; bodyStart, width, height: INTEGER;
              hasMap: BOOLEAN): ScrnPixmap.Raw
              RAISES {Thread.Alerted, Rd.Failure, Images.Error} =
  (* LL = t *)
  CONST
    SimpleRunMax = 14;                 (* longest run encoded in first byte *)
    FirstNonLiteral = 160;             (* first byte value used for encoding *)
    RunZBase = FirstNonLiteral;        (* runs of zeroes *)
    RunZEscape = RunZBase+SimpleRunMax;(* second byte is run length *)
    RunPBase = RunZEscape+1;           (* runs of matches to prev. scan-line *)
    RunPEscape = RunPBase+SimpleRunMax;(* second byte is run length *)
    LiteralEscape = RunPEscape+1;      (* second byte is literal byte value *)
    (* EOF = LiteralEscape+1; *)
  TYPE
    RowIndex = [0..99999];
    Row = UNTRACED REF ARRAY RowIndex OF CHAR;
    Char4 = ARRAY [0..3] OF CHAR;
  VAR
    myIncarnation: INTEGER;
    scale, maxVal, dataLength: INTEGER;
    srcRGBMap: Images.RGBMap;
    data := ReadLGMData(t, bodyStart, hasMap, scale, maxVal,
                        dataLength, srcRGBMap);
    bitmap := (NOT hasMap) AND (maxVal=1);
    srcToPixel: SrcToPixel;
    pixelsPerGroup := ComputePacking(maxVal);
    adjustedWidth := (width DIV pixelsPerGroup) * pixelsPerGroup;
    raw: ScrnPixmap.Raw;
    optimize: BOOLEAN;
    charMap: ARRAY CHAR OF Char4;
    dataPos := 0;
    rowStart: INTEGER;
    zeroPixel: ScrnColorMap.Pixel;
    zeroRow: REF ARRAY OF CHAR;
    factors: ARRAY [0..8] OF INTEGER; (* for dividing up packed pixels *)
  BEGIN
    LOCK t.incMu DO myIncarnation := t.incarnation END;
    raw := NewRaw(t, bitmap, maxVal, width, height);
    optimize := (bitmap OR (raw.bitsPerPixel = 8)) AND
                (raw.westRounded = 0) AND
                (raw.pixelOrder = PaintPrivate.HostByteOrder);
    CASE pixelsPerGroup OF
      | 1, 2, 3, 4 =>
      | 8 => IF NOT bitmap THEN optimize := FALSE END;
    ELSE
      optimize := FALSE;
    END;
    IF width*height = 0 THEN
      (* avoid some special cases later on *)
      width := 0; height := 0; optimize := FALSE;
    END;
    IF bitmap THEN
      srcToPixel := NEW(SrcToPixel, 2);
      srcToPixel[0] := 0;
      srcToPixel[1] := 1;
    ELSIF hasMap THEN
      srcToPixel := NEW(SrcToPixel, NUMBER(srcRGBMap^));
      IF t.st = NIL THEN
        t.isBW := TRUE; t.isGray := TRUE; t.isGrayRamp := FALSE;
        FOR i := 0 TO LAST(srcToPixel^) DO
          srcToPixel[i] := i;
          IF t.isBW OR t.isGray THEN
            WITH rgb = srcRGBMap[i] DO
              IF rgb.r # rgb.g OR rgb.g # rgb.b THEN
                t.isBW := FALSE;
                t.isGray := FALSE;
              ELSIF rgb.r # 0 AND rgb.r # 255 THEN
                t.isBW := FALSE;
              END;
            END;
          END;
        END;
        t.pixelToRGB := srcRGBMap;
      ELSE
        WITH rgbToPixel = GetRGBToPixel(t) DO
          FOR i := 0 TO LAST(srcToPixel^) DO
            srcToPixel[i] := LookupRGBToPixel(t, srcRGBMap[i], rgbToPixel, 0);
          END;
        END;
      END;
    ELSE
      (* not a bitmap and doesn't have a map: simple gray ramp *)
      IF t.st = NIL THEN
        srcToPixel := NEW(SrcToPixel, maxVal+1);
        t.pixelToRGB := NEW(Images.RGBMap, maxVal+1);
        FOR i := 0 TO LAST(srcToPixel^) DO
          srcToPixel[i] := i;
          WITH gray = ROUND((FLOAT(maxVal-i) / FLOAT(maxVal)) * 255.0) DO
            t.pixelToRGB[i] := Images.RGB{ gray, gray, gray };
          END;
        END;
        t.isBW := FALSE;
        t.isGray := TRUE;
        t.isGrayRamp := TRUE;
      ELSE
        WITH srcToPixelInverse = GetGrayToPixel(t, maxVal) DO
          srcToPixel := NEW(SrcToPixel, NUMBER(srcToPixelInverse^));
          FOR i := 0 TO LAST(srcToPixelInverse^) DO
            srcToPixel[i] := srcToPixelInverse[maxVal-i];
          END;
        END;
      END;
    END;
    IF NUMBER(srcToPixel^) = 0 THEN
      zeroPixel := 0; (* there aren't any pixels *)
    ELSE
      zeroPixel := srcToPixel[0];
    END;
    factors[pixelsPerGroup] := 1;
    FOR i := pixelsPerGroup-1 TO 0 BY -1 DO
      factors[i] := factors[i+1] * (maxVal+1);
    END;
    IF optimize THEN
      (* Compute tables to speed up the inner loop *)
      FOR i := 0 TO factors[0]-1 DO
        FOR j := 1 TO MIN(pixelsPerGroup, 4) DO
          charMap[VAL(i,CHAR)][j-1] :=
              VAL(srcToPixel[(i DIV factors[j]) MOD (maxVal+1)], CHAR);
        END;
      END;
      zeroRow := NEW(REF ARRAY OF CHAR, adjustedWidth);
      FOR i := 0 TO LAST(zeroRow^) DO
        zeroRow[i] := VAL(srcToPixel[0], CHAR);
      END;
    END;
      rowStart := raw.offset;
      FOR v := 0 TO height - 1 DO
        IF v MOD 200 = 199 THEN
          Thread.AlertPause(0.002D0); (* allow other processing *)
          LOCK t.incMu DO
            IF t.incarnation > myIncarnation OR Thread.TestAlert() THEN
              RAISE Thread.Alerted;
            END;
          END;
        END;
        IF v > 0 THEN
          (* Initialize row to previous one, quickly *)
          SUBARRAY(raw.pixels^, rowStart, raw.wordsPerRow) :=
              SUBARRAY(raw.pixels^, rowStart-raw.wordsPerRow, raw.wordsPerRow);
        END;
        VAR
          this := LOOPHOLE(ADR(raw.pixels[rowStart]), Row);
          h: RowIndex := 0;
          c: CHAR;
          d: INTEGER;
        BEGIN
          IF optimize THEN
            CASE pixelsPerGroup OF
            | 1 =>
                WHILE h < adjustedWidth DO
                  c := data[dataPos]; INC(dataPos);
                  IF ORD(c) >= RunZBase AND ORD(c) <= RunZEscape THEN
                    (* Run of groups of "Zero" *)
                    IF ORD(c) = RunZEscape THEN
                      d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                    ELSE
                      d := ORD(c) - RunZBase;
                    END;
                    SUBARRAY(this^, h, d+1) :=
                        SUBARRAY(zeroRow^, 0, d+1);
                    INC(h, d+1);
                  ELSIF ORD(c) >= RunPBase AND ORD(c) <= RunPEscape THEN
                    (* Run of groups equal to previous row - already done *)
                    IF ORD(c) = RunPEscape THEN
                      d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                    ELSE
                      d := ORD(c) - RunPBase;
                    END;
                    INC(h, d+1);
                  ELSE (* Literal, perhaps escaped *)
                    IF ORD(c) = LiteralEscape THEN
                      c := data[dataPos]; INC(dataPos);
                    END;
                    (* Pixel group *)
                    this[h] := charMap[c][0];
                    INC(h);
                  END;
                END;
            | 2 =>
                WHILE h < adjustedWidth DO
                  c := data[dataPos]; INC(dataPos);
                  IF ORD(c) >= RunZBase AND ORD(c) <= RunZEscape THEN
                    (* Run of groups of "Zero" *)
                    IF ORD(c) = RunZEscape THEN
                      d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                    ELSE
                      d := ORD(c) - RunZBase;
                    END;
                    SUBARRAY(this^, h, (d+1)*pixelsPerGroup) :=
                        SUBARRAY(zeroRow^, 0, (d+1)*pixelsPerGroup);
                    INC(h, (d+1)*pixelsPerGroup);
                  ELSIF ORD(c) >= RunPBase AND ORD(c) <= RunPEscape THEN
                    (* Run of groups equal to previous row - already done *)
                    IF ORD(c) = RunPEscape THEN
                      d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                    ELSE
                      d := ORD(c) - RunPBase;
                    END;
                    INC(h, (d+1)*pixelsPerGroup);
                  ELSE (* Literal, perhaps escaped *)
                    IF ORD(c) = LiteralEscape THEN
                      c := data[dataPos]; INC(dataPos);
                    END;
                    (* Pixel group *)
                    this[h] := charMap[c][0];
                    this[h+1] := charMap[c][1];
                    INC(h, 2);
                  END;
                END;
            | 3 =>
                WHILE h < adjustedWidth DO
                  c := data[dataPos]; INC(dataPos);
                  IF ORD(c) >= RunZBase AND ORD(c) <= RunZEscape THEN
                    (* Run of groups of "Zero" *)
                    IF ORD(c) = RunZEscape THEN
                      d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                    ELSE
                      d := ORD(c) - RunZBase;
                    END;
                    SUBARRAY(this^, h, (d+1)*pixelsPerGroup) :=
                        SUBARRAY(zeroRow^, 0, (d+1)*pixelsPerGroup);
                    INC(h, (d+1)*pixelsPerGroup);
                  ELSIF ORD(c) >= RunPBase AND ORD(c) <= RunPEscape THEN
                    (* Run of groups equal to previous row - already done *)
                    IF ORD(c) = RunPEscape THEN
                      d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                    ELSE
                      d := ORD(c) - RunPBase;
                    END;
                    INC(h, (d+1)*pixelsPerGroup);
                  ELSE (* Literal, perhaps escaped *)
                    IF ORD(c) = LiteralEscape THEN
                      c := data[dataPos]; INC(dataPos);
                    END;
                    (* Pixel group *)
                    this[h] := charMap[c][0];
                    this[h+1] := charMap[c][1];
                    this[h+2] := charMap[c][2];
                    INC(h, 3);
                  END;
                END;
            | 4 =>
                WHILE h < adjustedWidth DO
                  c := data[dataPos]; INC(dataPos);
                  IF ORD(c) >= RunZBase AND ORD(c) <= RunZEscape THEN
                    (* Run of groups of "Zero" *)
                    IF ORD(c) = RunZEscape THEN
                      d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                    ELSE
                      d := ORD(c) - RunZBase;
                    END;
                    SUBARRAY(this^, h, (d+1)*pixelsPerGroup) :=
                        SUBARRAY(zeroRow^, 0, (d+1)*pixelsPerGroup);
                    INC(h, (d+1)*pixelsPerGroup);
                  ELSIF ORD(c) >= RunPBase AND ORD(c) <= RunPEscape THEN
                    (* Run of groups equal to previous row - already done *)
                    IF ORD(c) = RunPEscape THEN
                      d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                    ELSE
                      d := ORD(c) - RunPBase;
                    END;
                    INC(h, (d+1)*pixelsPerGroup);
                  ELSE (* Literal, perhaps escaped *)
                    IF ORD(c) = LiteralEscape THEN
                      c := data[dataPos]; INC(dataPos);
                    END;
                    LOOPHOLE(ADR(this[h]), UNTRACED REF Char4)^ := charMap[c];
                    (* Well, that was a bit marginal. An alternative is the
                       following, though it's a fair bit slower and this really
                       is the inner loop ...
                       this[h] := charMap[c][0];
                       this[h+1] := charMap[c][1];
                       this[h+2] := charMap[c][2];
                       this[h+3] := charMap[c][3];
                       *)
                    INC(h, 4);
                  END;
                END;
            | 8 =>
                WHILE h < adjustedWidth DO
                  c := data[dataPos]; INC(dataPos);
                  IF ORD(c) >= RunZBase AND ORD(c) <= RunZEscape THEN
                    (* Run of groups of "Zero" *)
                    IF ORD(c) = RunZEscape THEN
                      d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                    ELSE
                      d := ORD(c) - RunZBase;
                    END;
                    SUBARRAY(this^, h DIV 8, d+1) :=
                        SUBARRAY(zeroRow^, 0, d+1);
                    INC(h, (d+1)*8);
                 ELSIF ORD(c) >= RunPBase AND ORD(c) <= RunPEscape THEN
                    (* Run of groups equal to previous row - already done *)
                    IF ORD(c) = RunPEscape THEN
                      d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                    ELSE
                      d := ORD(c) - RunPBase;
                    END;
                    INC(h, (d+1)*8);
                  ELSE (* Literal, perhaps escaped *)
                    IF ORD(c) = LiteralEscape THEN
                      c := data[dataPos]; INC(dataPos);
                    END;
                    (* Pixel group *)
                    this[h DIV 8] := lgmToHost[c];
                    INC(h, 8);
                  END;
                END;
            ELSE
                <* ASSERT FALSE *>
            END;
          ELSE (* not optimized *)
            WHILE h < adjustedWidth DO
              c := data[dataPos]; INC(dataPos);
              IF ORD(c) >= RunZBase AND ORD(c) <= RunZEscape THEN
                (* Run of groups of "Zero" *)
                IF ORD(c) = RunZEscape THEN
                  d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                ELSE
                  d := ORD(c) - RunZBase;
                END;
                FOR i := 1 TO (d+1) * pixelsPerGroup DO
                  raw.set(Point.T{h,v}, zeroPixel); INC(h);
                END;
              ELSIF ORD(c) >= RunPBase AND ORD(c) <= RunPEscape THEN
                (* Run of groups equal to previous row - already done *)
                IF ORD(c) = RunPEscape THEN
                  d := ORD(data[dataPos])+SimpleRunMax; INC(dataPos);
                ELSE
                  d := ORD(c) - RunPBase;
                END;
                INC(h, (d+1) * pixelsPerGroup);
              ELSE (* Literal, perhaps escaped *)
                IF ORD(c) = LiteralEscape THEN
                  c := data[dataPos]; INC(dataPos);
                END;
                (* Pixel group, first in l.s. *)
                FOR j := 1 TO pixelsPerGroup DO
                  raw.set(Point.T{h,v}, 
                          srcToPixel[(ORD(c) DIV factors[j]) MOD (maxVal+1)]);
                  INC(h);
                END;
              END (* IF ORD(c) *);
            END (* WHILE h *);
          END (* IF optimized *);
        END (* BEGIN *);
        INC(rowStart, raw.wordsPerRow);
      END (* FOR v *);
    IF bitmap THEN t.isBitmap() END;
    RETURN raw
  END lgm;


(* *)
(* Read-ahead *)
(* *)

TYPE PreComputeClosure = Thread.Closure OBJECT
    t: T;
    st: VBT.ScreenType;
  OVERRIDES
    apply := PreCompute;
  END;

PROCEDURE PreCompute(self: PreComputeClosure): REFANY =
  VAR t := self.t;
  BEGIN
    Thread.Pause(0.050D0); (* Allow foreground work to go first *)
    LOCK t.mu DO
      IF NOT t.rawValid THEN
        (* Don't do it if someone else has computed one by now *)
        TRY GetRaw(t, self.st)
        EXCEPT Thread.Alerted, Images.Error =>
        END;
      END;
    END;
    RETURN NIL
  END PreCompute;


(* *)
(* Methods and top-level entries *)
(* *)

PROCEDURE Init(t: T; rd: Rd.T; start, length: CARDINAL;
               op: PaintOp.T := PaintOp.Copy;
               st: VBT.ScreenType := NIL;
               gamma: REAL := 1.0): T =
    (* LL < t *)
  BEGIN
    IF t.mu = NIL THEN
      t.mu := NEW(MUTEX);
      t.incMu := NEW(MUTEX);
      t.domMu := NEW(MUTEX);
    END;
    LOCK t.incMu DO INC(t.incarnation) END;
    LOCK t.domMu DO t.domainValid := FALSE END;
    LOCK t.mu DO
      t.rd := rd;
      t.start := start;
      t.length := length;
      t.op := op;
      t.rawValid := FALSE;
      t.gamma := gamma;
      IF t.rd # NIL AND st # NIL THEN
        EVAL Thread.Fork(NEW(PreComputeClosure, t := t, st := st));
      END;
    END;
    RETURN t
  END Init;

PROCEDURE IsBitmap(t: T) =
  BEGIN
    IF t.op = PaintOp.Copy THEN
      t.op := PaintOp.Pair(PaintOp.FromRGB(1.0, 1.0, 1.0),
                           PaintOp.FromRGB(0.0, 0.0, 0.0));
    END;
    IF t.st = NIL THEN
      t.pixelToRGB := NEW(Images.RGBMap, 2);
      t.pixelToRGB^ := ARRAY OF Images.RGB { Images.RGB{ 255, 255, 255 },
                                         Images.RGB{ 0, 0, 0 } };
      t.isBW := TRUE;
      t.isGray := TRUE;
      t.isGrayRamp := TRUE;
    END;
  END IsBitmap;

PROCEDURE GetRaw(t: T; st: VBT.ScreenType) RAISES { Thread.Alerted,
                                                    Images.Error } =
    (* LL = t *)
  VAR
    binary, hasMap: BOOLEAN;
    format: Format;
    bodyStart, width, height: INTEGER;
  BEGIN
    IF (NOT t.rawValid) OR t.st # st OR t.raw = NIL THEN
      (* Cache isn't enough; need to do some actual work *)
      t.rawValid := FALSE;
      TRY
        IF t.cooked # NIL THEN t.cooked.free() END;
      EXCEPT
      | TrestleComm.Failure =>
      END;
      t.cooked := NIL;
      IF t.rd = NIL THEN RAISE Images.Error("No image") END;
      t.st := st;
      TRY
        format := GetHeader(t, bodyStart, width, height, binary, hasMap);
        CASE format OF
        | Format.PBM =>
            t.raw := pbm(t, bodyStart, width, height, binary);
        | Format.PGM => 
            t.raw := pgm(t, bodyStart, width, height, binary);
        | Format.PPM => 
            t.raw := ppm(t, bodyStart, width, height, binary);
        | Format.LGM =>
            t.raw := lgm(t, bodyStart, width, height, hasMap)
        ELSE
            RAISE Images.Error("Unsupported file format")
        END;
      EXCEPT
        | Rd.Failure =>
            RAISE Images.Error("Failure while reading image file")
      END;
      t.rawValid := TRUE; (* except on error *)
    END;
  END GetRaw;

PROCEDURE Domain(t: T; <*UNUSED*>v: VBT.Leaf): Rect.T =
    (* LL < t *)
    (* Assumes domain is independent of v and its screenType *)
  VAR
    binary, hasMap: BOOLEAN; bodyStart, width, height: INTEGER;
  BEGIN
    LOCK t.domMu DO IF t.domainValid THEN RETURN t.cachedDomain END END;
    TRY
      LOCK t.mu DO
        IF t.rd = NIL THEN RETURN Rect.Empty END;
        EVAL GetHeader(t, bodyStart, width, height, binary, hasMap);
        LOCK t.domMu DO RETURN t.cachedDomain END;
      END;
    EXCEPT
    | Rd.Failure, Thread.Alerted, Images.Error =>
        RETURN Rect.Empty;
    END;
  END Domain;

PROCEDURE Paint(t: T; v: VBT.Leaf;
                READONLY clip: Rect.T := Rect.Full;
                READONLY delta: Point.T;
                paintChunk := 0)
               RAISES { Thread.Alerted, Images.Error } =
    (* LL <= VBT.mu, LL < t *)
  VAR
    st := VBT.ScreenTypeOf(v);
    sub: ScrnPixmap.Raw;
    cooked: ScrnPixmap.T;
    myIncarnation: INTEGER;
    paintSub: Rect.T;
    linesPerPaint: INTEGER;
  BEGIN
    LOCK t.incMu DO myIncarnation := t.incarnation END;
    LOCK t.mu DO
      IF t.rd = NIL OR Rect.HorSize(clip) = 0 OR Rect.VerSize(clip) = 0 THEN
        (* Paint nothing *)
      ELSE
        TRY
          GetRaw(t, st);
          IF Rect.HorSize(t.raw.bounds)*Rect.VerSize(t.raw.bounds) < 1000 THEN
            (* Small enough to keep loaded in the server *)
            IF t.cooked # NIL THEN
              (* Must be ok; leave it alone *)
            ELSE
              t.cooked := st.pixmap.load(t.raw);
            END;
            VBT.PaintScrnPixmap(v, clip, t.op, t.cooked, delta);
          ELSE
            paintSub := clip;
            IF paintChunk <= 0 THEN
              linesPerPaint := Rect.VerSize(paintSub);
            ELSE
              linesPerPaint := 1 + paintChunk DIV Rect.HorSize(clip);
            END;
            FOR top := paintSub.north TO paintSub.south-1 BY linesPerPaint DO
              paintSub.north := top;
              paintSub.south := MIN(top+linesPerPaint, clip.south);
              sub := t.raw.sub(Rect.Sub(paintSub, delta));
              sub.bounds.west := sub.westRounded; (* Trestle bug *)
              cooked := st.pixmap.load(sub);
              TRY
                IF paintSub.south < clip.south THEN
                  Thread.AlertPause(0.010D0); (* allow other processing *)
                  LOCK t.incMu DO
                    IF t.incarnation > myIncarnation OR Thread.TestAlert() THEN
                      RAISE Thread.Alerted;
                    END;
                  END;
                END;
                VBT.PaintScrnPixmap(v, paintSub, t.op, cooked, delta);
                VBT.Sync(v, FALSE); (* flush to process boundary *)
              FINALLY
                cooked.free();
              END;
            END;
          END;
        EXCEPT
          TrestleComm.Failure => (* We can't paint. Do nothing *)
        END;
      END; (*LOCK*)
    END;
  END Paint;

PROCEDURE Render(t: T; v: VBT.Leaf): ScrnPixmap.Raw
                RAISES{ Thread.Alerted, Images.Error } =
  VAR st: VBT.ScreenType;
  BEGIN
    IF v = NIL THEN st := NIL ELSE st := VBT.ScreenTypeOf(v) END;
    LOCK t.mu DO
      GetRaw(t, st);
      RETURN t.raw;
    END;
  END Render;

PROCEDURE GetContents(t: T): Images.Contents
                      RAISES{ Thread.Alerted, Images.Error } =
  BEGIN
    LOCK t.mu DO
      GetRaw(t, NIL);
      RETURN NEW(Contents, raw := t.raw,
                           width := Rect.HorSize(t.raw.bounds),
                           height := Rect.VerSize(t.raw.bounds),
                           map := t.pixelToRGB,
                           isBW := t.isBW,
                           isGray := t.isGray,
                           isGrayRamp := t.isGrayRamp,
                           format := t.format);
    END;
  END GetContents;

PROCEDURE Close(t: T) =
  BEGIN
    LOCK t.mu DO
      t.rd := NIL;
      t.rawValid := FALSE;
      t.raw := NIL;
      TRY
        IF t.cooked # NIL THEN t.cooked.free(); END;
      EXCEPT
      | TrestleComm.Failure =>
      END;
      t.st := NIL;
    END; (*LOCK*)
  END Close;

PROCEDURE ToEPSF(t: T; wr: Wr.T; binary: BOOLEAN)
                 RAISES { Wr.Failure, Thread.Alerted, Images.Error } =
  VAR
    psLib := Bundle.Get(ImagesBundle.Get(), "sendImage.ps");
    pnmBinary, hasMap: BOOLEAN;
    bodyStart, width, height: INTEGER;
    format: Format;
    scanLine: REF ARRAY OF CHAR;
  CONST dpi = 300.0;
  CONST hexDigits = ARRAY OF CHAR {
                                '0', '1', '2', '3', '4', '5', '6', '7',
                                '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};
  VAR ms, ls: ARRAY CHAR OF CHAR;
  CONST
    LineLen = 64; (* characters per line of output; must divide PSBufLen*2 *)
    PSBufLen = 1024; (* shared with sendImage.ps *)
    LinesInBuf = (PSBufLen*2) DIV LineLen; (* lines of hex *)
    BufLen = (LineLen+1) * LinesInBuf;
  VAR
    buffer: ARRAY [0..BufLen-1] OF CHAR;
    charsInBuf := 0;
    charsInLine := 0;
    lastTime := Time.Now();
  PROCEDURE PollAlert() RAISES { Thread.Alerted } =
    BEGIN
      IF Time.Now() > lastTime + 0.100D0 THEN
        Thread.Pause(0.005D0); (* allow user interactions *)
        IF Thread.TestAlert() THEN RAISE Thread.Alerted END;
        lastTime := Time.Now();
      END;
    END PollAlert;
  PROCEDURE PutData(VAR data: ARRAY OF CHAR)
                    RAISES { Wr.Failure, Thread.Alerted } =
    (* Write out data converted to lines of hex *)
    BEGIN
      FOR i := 0 TO LAST(data) DO
        IF binary THEN
          IF charsInBuf = PSBufLen THEN
            Wr.PutString(wr, SUBARRAY(buffer, 0, PSBufLen));
            charsInBuf := 0;
            PollAlert();
          END;
          buffer[charsInBuf] := data[i];
          INC(charsInBuf);
        ELSE
          IF charsInLine = LineLen THEN
            buffer[charsInBuf+LineLen] := '\n';
            INC(charsInBuf, LineLen+1);
            charsInLine := 0;
            IF charsInBuf = BufLen THEN
              Wr.PutString(wr, buffer); charsInBuf := 0;
              PollAlert();
            END;
          END;
          VAR c := data[i];
          BEGIN
            buffer[charsInBuf+charsInLine] := ms[c];
            buffer[charsInBuf+charsInLine+1] := ls[c];
          END;
          INC(charsInLine, 2);
        END;
      END;
    END PutData;
  PROCEDURE FlushData() RAISES { Wr.Failure, Thread.Alerted } =
    (* Pad to buffer boundary and flush *)
    BEGIN
      IF binary THEN
        FOR i := charsInBuf TO PSBufLen-1 DO buffer[i] := VAL(0, CHAR) END;
        Wr.PutString(wr, SUBARRAY(buffer, 0, PSBufLen));
      ELSE
        FOR i := charsInBuf+charsInLine TO BufLen-1 DO
          IF charsInLine = LineLen THEN
            buffer[i] := '\n';
            charsInLine := 0;
          ELSE
            buffer[i] := '0';
            INC(charsInLine);
          END;
        END;
        Wr.PutString(wr, buffer);
      END;
    END FlushData;
  BEGIN
    <* ASSERT psLib # NIL *>
    <* ASSERT (PSBufLen*2) MOD LineLen = 0 *>
    FOR i := FIRST(CHAR) TO LAST(CHAR) DO
      (* Construct hex table *)
      ms[i] := hexDigits[ORD(i) DIV 16];
      ls[i] := hexDigits[ORD(i) MOD 16];
    END;
    TRY
      LOCK t.mu DO
        format := GetHeader(t, bodyStart, width, height, pnmBinary, hasMap);
        Wr.PutText(wr, "%!PS-Adobe-3.0 EPSF-3.0\n");
        Wr.PutText(wr, "%%BoundingBox: 0 0 " &
                       Fmt.Int(ROUND(FLOAT(width) * 72.0 / dpi)) & " " &
                       Fmt.Int(ROUND(FLOAT(height) * 72.0 / dpi)) & "\n");
        Wr.PutText(wr, "%%EndComments\n");
        Wr.PutText(wr, psLib);
        Wr.PutText(wr, Fmt.Real(dpi) & " " &
                       Fmt.Int(width) & " " &
                       Fmt.Int(height) & " ");
        IF binary THEN
          Wr.PutText(wr, "true ");
        ELSE
          Wr.PutText(wr, "false ");
        END;
        CASE format OF
          | Format.LGM =>
              VAR
                scale, maxVal, dataLength: INTEGER;
                srcRGBMap: Images.RGBMap;
                lgmData := ReadLGMData(t, bodyStart, hasMap, scale, maxVal,
                                       dataLength, srcRGBMap);
              BEGIN
                IF hasMap THEN
                  RAISE Images.Error(
                              "Printing mapped LGM files not yet implemented");
                END;
                Wr.PutText(wr, Fmt.Int(maxVal) & " lgm\n");
                PutData(SUBARRAY(lgmData^, 0, dataLength));
                FlushData();
              END;
          | Format.PBM =>
              Wr.PutText(wr, "raw\n");
              LOCK rdMu DO
                IF Rd.Index(t.rd) # bodyStart THEN Rd.Seek(t.rd, bodyStart) END;
                IF pnmBinary THEN WHILE Rd.GetChar(t.rd) # '\n' DO END END;
                scanLine := NEW(REF ARRAY OF CHAR, (width+7) DIV 8);
                FOR v := 0 TO height-1 DO
                  IF pnmBinary THEN
                    GetScanLine(t.rd, scanLine, 255, pnmBinary);
                  ELSE
                    FOR w := 0 TO LAST(scanLine^) DO
                      VAR
                        byte: Word.T := 0;
                        b := w * 8;
                        limit := MIN(width, b+8);
                      BEGIN
                        WHILE b < limit DO
                          WITH c = Rd.GetChar(t.rd) DO
                            IF c = '0' THEN
                              byte := Word.Shift(byte, 1);
                              INC(b);
                            ELSIF c = '1' THEN
                              byte := Word.Or(Word.Shift(byte, 1), 1);
                              INC(b);
                            ELSIF c = '#' THEN
                              EVAL Rd.GetLine(t.rd);
                            END;
                          END;
                        END;
                         scanLine[w] := VAL(byte, CHAR);
                     END;
                    END;
                  END;
                  (* Invert the bits, to PS standard *)
                  FOR i := 0 TO LAST(scanLine^) DO
                    scanLine[i] := VAL(255-ORD(scanLine[i]), CHAR);
                  END;
                  PutData(scanLine^);
                END;
                FlushData();
              END;
        ELSE
            RAISE Images.Error(
                          "Printing this format of image not yet implemented");
        END;
        Wr.PutChar(wr, '\n'); (* blank line for flushing to *)
        Wr.PutText(wr, "showpage\n");
        (* Don't write %%EOF; for imbedded EPSF, confuses CAP print driver *)
        (* For stand-alone EPSF, caller writes %%EOF before closing *)
      END;
    EXCEPT
      | Rd.EndOfFile =>
          RAISE Images.Error("Unexpected end-of-file in image file");
      | Rd.Failure =>
          RAISE Images.Error("Failure while reading image file");
    END;
  END ToEPSF;

PROCEDURE Copy(rd: Rd.T; wr: Wr.T)
              RAISES { Rd.Failure, Wr.Failure, Thread.Alerted, Images.Error } =
  (* LL = 0 *)
  VAR
    t: T;
    bodyStart, width, height, maxVal, bytes: INTEGER;
    binary, hasMap: BOOLEAN;
    format: Format;
    data := NEW(REF ARRAY OF CHAR, 8192*3);
    done := 0;
  BEGIN
    LOCK rdMu DO
      TRY
        (* Skip initial white space *)
        WHILE TRUE DO
          WITH c = Rd.GetChar(rd) DO
            IF c # ' ' AND c # '\n' THEN Rd.UnGetChar(rd); EXIT END;
          END;
        END;
      EXCEPT Rd.EndOfFile => (* Ignore, it will be dealt with later *)
      END;
      t := NEW(T).init(rd, Rd.Index(rd), LAST(INTEGER));
    END;
    format := GetHeader(t, bodyStart, width, height, binary, hasMap);
    LOCK rdMu DO
      IF Rd.Index(rd) # bodyStart THEN Rd.Seek(rd, bodyStart) END;
      IF format = Format.LGM THEN
        RAISE Images.Error("Copying LGM not implemented");
      END;
      Wr.PutChar(wr, 'P');
      CASE format OF
      | Format.PBM => Wr.PutChar(wr, '4'); bytes := ((width+7) DIV 8) * height;
      | Format.PGM => Wr.PutChar(wr, '5'); bytes := width * height;
      | Format.PPM => Wr.PutChar(wr, '6'); bytes := width * height * 3;
      ELSE
        <* ASSERT FALSE *>
      END;
      Wr.PutText(wr, Fmt.Int(width)); Wr.PutChar(wr, '\n');
      Wr.PutText(wr, Fmt.Int(height)); Wr.PutChar(wr, '\n');
      IF format = Format.PGM OR format = Format.PPM THEN
        maxVal := ScanInt(rd);
        Wr.PutText(wr, Fmt.Int(maxVal)); Wr.PutChar(wr, '\n');
      END;
      IF binary THEN
        TRY WHILE Rd.GetChar(rd) # '\n' DO END; EXCEPT Rd.EndOfFile => END;
        WHILE TRUE DO
          WITH thisTime = MIN(bytes - done, NUMBER(data^)) DO
            IF thisTime <= 0 THEN EXIT END;
            IF Rd.GetSub(rd, SUBARRAY(data^, 0, thisTime)) # thisTime THEN
              RAISE Images.Error("Unexpected end of file while copying PNM");
            END;
            Wr.PutString(wr, SUBARRAY(data^, 0, thisTime));
            INC(done, thisTime);
          END;
        END;
      ELSE
        RAISE Images.Error("Copying non-raw PNM not yet implemented");
      END;
    END;
  END Copy;

BEGIN

  InitReversedBits();
  IF PaintPrivate.HostByteOrder = PaintPrivate.ByteOrder.MSBFirst THEN
    pbmToHost := plainBits;
    lgmToHost := plainBits;
  ELSE
    pbmToHost := reversedBits;
    lgmToHost := reversedBits;
  END;

END ImageRd.
