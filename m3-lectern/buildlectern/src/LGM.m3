(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* Filter for converting pbm files to Lectern's own lgm format *)

(* Last modified on Mon Mar 27 15:07:09 PST 1995 by birrell   *)
(*      modified on Tue Nov 15 18:20:08 PST 1994 by wobber    *)
(*      modified on Thu Oct 20 17:00:14 PDT 1994 by mcjones   *)

UNSAFE MODULE LGM;

IMPORT Fmt, ImageRd, Images, Math, OSError, OSUtils,
       PaintPrivate, Rd, Rect, RegularFile, ScrnPixmap, Stdio,
       Thread, Word, Wr;

IMPORT Cscale, RTMisc;

(* *)
(* Types *)
(* *)

REVEAL T = ImageRd.T BRANDED OBJECT
    file: RegularFile.T;
    start, length: INTEGER;
    dupRd: Rd.T;
    vanilla: Images.Contents;
  END;


(* *)
(* Bit fiddling *)
(* *)

VAR reversedBits, plainBits, rawToLGM: ARRAY CHAR OF CHAR;
  (* reverses bit order: m.s. first to l.s. first, or vice versa *)

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

VAR bitsIn: ARRAY [0..255] OF INTEGER; (* count of 1 bits in each byte *)

PROCEDURE InitBitsIn() =
    VAR done := 1;
  BEGIN
    bitsIn[0] := 0;
    WHILE done < 256 DO
      FOR i := 0 TO done-1 DO bitsIn[done+i] := 1 + bitsIn[i] END;
      done := done * 2;
    END;
  END InitBitsIn;


(* *)
(* Lazy Image Contents: avoids buffering entire RGB images in memory *)
(* *)

TYPE LazyContents = Images.Contents OBJECT
  (* NOTE: this is an improper sub-class of Images.Contents: the .getLine
     method must be called sequentially, starting at 0, and the .map field
     is valid only when indexed by pixels from the most recently read line.
     But it uses a lot less memory! *)
    prevLine: INTEGER;
    chars: REF ARRAY OF CHAR;
  METHODS
    getRawLine();
      (* Just gets the next scan line into "chars" as a sequence of RGB
         triples; provided by sub-class. Note: .map is not valid after a
         call of this method.  Faster than .getLine *)
  OVERRIDES
    getLine := GetLazyLine;
      (* calls getRawLine then creates temporary pixel map *)
  END;

PROCEDURE GetLazyLine(lc: LazyContents;
                      v: INTEGER;
                      VAR line: ARRAY OF INTEGER) =
  BEGIN
    <* ASSERT v = lc.prevLine+1 *>
    lc.getRawLine();
    FOR i := 0 TO lc.width-1 DO
      lc.map[i] := Images.RGB{r := ORD(lc.chars[i*3]),
                              g := ORD(lc.chars[i*3+1]),
                              b := ORD(lc.chars[i*3+2])};
      line[i] := i;
    END;
  END GetLazyLine;


(* *)
(* Lazy PPM reading *)
(* *)

TYPE LazyPPM = LazyContents OBJECT
  (* Read a PPM file without buffering the entire image in memory.
     See comments at definition of LazyContents. *)
    rd: Rd.T;
  METHODS
    init(rd: Rd.T): LazyPPM RAISES { Images.Error } := InitLazyPPM;
  OVERRIDES
    getRawLine := GetLazyPPMRawLine;
  END;

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

PROCEDURE InitLazyPPM(lc: LazyPPM; rd: Rd.T): LazyPPM
                      RAISES { Images.Error } =
  VAR
    maxVal: INTEGER;
  BEGIN
    TRY
      IF Rd.GetChar(rd) # 'P' OR Rd.GetChar(rd) # '6' THEN
        RAISE Images.Error("Not a ppmraw file");
      END;
      lc.rd := rd;
      lc.width := ScanInt(rd);
      lc.height := ScanInt(rd);
      maxVal := ScanInt(rd);
      IF maxVal # 255 THEN RAISE Images.Error("Unsuitable PPM file") END;
      lc.map := NEW(Images.RGBMap, lc.width);
      lc.isBW := FALSE;
      lc.isGray := FALSE;
      lc.isGrayRamp := FALSE;
      lc.prevLine := -1;
      lc.chars := NEW(REF ARRAY OF CHAR, 3*lc.width);
      WHILE Rd.GetChar(rd) # '\n' DO END;
    EXCEPT Rd.EndOfFile, Rd.Failure, Thread.Alerted =>
      RAISE Images.Error("Syntax error in ppmraw file");
    END;
    RETURN lc;
  END InitLazyPPM;

PROCEDURE GetLazyPPMRawLine(lc: LazyPPM) =
  BEGIN
    TRY
      WITH n = Rd.GetSub(lc.rd, lc.chars^) DO
        <* ASSERT n = NUMBER(lc.chars^) *>
      END;
    EXCEPT
      Rd.Failure, Thread.Alerted => <*ASSERT FALSE*>
    END;
    INC(lc.prevLine);
  END GetLazyPPMRawLine;


(* *)
(* RGB Scaling *)
(* *)

TYPE ScaledRGB = LazyContents OBJECT
  (* Sub-class of LazyContents that scales an RGB image *)
    scale: INTEGER;
    unscaled: Images.Contents;
    line: REF ARRAY OF INTEGER; (* for accumulating counts during scaling *)
    unscaledLine: REF ARRAY OF INTEGER; (* for non-lazy unscaled sources *)
    factor: INTEGER;
      (* divide accumulated total by this before indexing toDestPixel *)
    toDestPixel: REF ARRAY OF CHAR;
      (* maps total DIV factor gamma adjusted to [0..255] *)
  METHODS
    init(unscaled: Images.Contents; scale: INTEGER; gamma: REAL): ScaledRGB
         RAISES { Images.Error } := InitScaledRGB;
  OVERRIDES
    getRawLine := GetScaledRGBRawLine;
  END;

PROCEDURE InitScaledRGB(lc: ScaledRGB;
                        unscaled: Images.Contents;
                        scale: INTEGER;
                        gamma: REAL): ScaledRGB =
  VAR
    scaledMax: INTEGER;  (* the maximum value that will be accumulated *)
    gray, lastGray: REAL;
  BEGIN
    lc.width := unscaled.width DIV scale;
    lc.height := unscaled.height DIV scale;
    lc.map := NEW(Images.RGBMap, lc.width);
    lc.prevLine := -1;
    lc.chars := NEW(REF ARRAY OF CHAR, 3*lc.width);
    lc.unscaled := unscaled;
    lc.scale := scale;
    lc.line := NEW(REF ARRAY OF INTEGER, 3*lc.width);
    lc.unscaledLine := NEW(REF ARRAY OF INTEGER, unscaled.width);
    scaledMax := scale * scale * 255; (* maximum accumulation *)
    IF scaledMax > 36 * 255 THEN
      (* keep .toDestPixel array down to a reasonable length *)
      lc.factor := scale*scale;
    ELSE
      lc.factor := 1;
    END;
    lc.toDestPixel := NEW(REF ARRAY OF CHAR, (scaledMax DIV lc.factor)+1);
    lastGray := FLOAT(LAST(lc.toDestPixel^));
    FOR i := 0 TO LAST(lc.toDestPixel^) DO
      gray := FLOAT(i) / lastGray; (* gray level of accumulation *)
      IF gamma # 1.0 THEN
        gray  := FLOAT(Math.pow(FLOAT(gray, LONGREAL),
                                FLOAT(1.0/gamma, LONGREAL)));
      END;
      (* convert to [0..255] with white = 255 *)
      lc.toDestPixel[i] := VAL(ROUND(gray * 255.0), CHAR);
    END;
    lc.isBW := FALSE;
    lc.isGray := FALSE;
    lc.isGrayRamp := FALSE;
    RETURN lc
  END InitScaledRGB;

PROCEDURE GetScaledRGBRawLine(lc: ScaledRGB) =
  VAR
    total0, total1, total2, scale, scale3, scaledH: INTEGER;
    unscaledChars: REF ARRAY OF CHAR;
    line: REF ARRAY OF INTEGER;
    toDestPixel: REF ARRAY OF CHAR;
    scaledChars: REF ARRAY OF CHAR;
    unscaledLine: REF ARRAY OF INTEGER;
    unscaledLC: LazyContents;
    rawRGB: Images.RGB;
    map: Images.RGBMap;
  BEGIN
    INC(lc.prevLine);
    scale := lc.scale;
    scale3 := (scale-1)*3;
    TYPECASE lc.unscaled OF
      | LazyContents(ulc) =>
          unscaledLC := ulc;
          unscaledChars := ulc.chars;
    ELSE
      unscaledLC := NIL;
      unscaledChars := NIL;
    END;
    line := lc.line;
    toDestPixel := lc.toDestPixel;
    scaledChars := lc.chars;
    unscaledLine := lc.unscaledLine;
    IF  scale = 1 AND unscaledLC # NIL THEN
      unscaledLC.getRawLine();
      lc.chars^ := unscaledChars^;
    ELSE
      RTMisc.Zero(ADR(line[0]), NUMBER(line^)*BYTESIZE(INTEGER));
      FOR v := 0 TO scale-1 DO
        IF unscaledLC = NIL THEN
          lc.unscaled.getLine(lc.prevLine*scale, unscaledLine^);
          map := lc.unscaled.map;
          FOR h := 0 TO lc.width-1 DO
            scaledH := h * scale;
            total0 := 0; total1 := 0; total2 := 0;
            FOR p := 0 TO scale-1 DO
              rawRGB := map[unscaledLine[scaledH+p]];
              INC(total0, ORD(rawRGB.r));
              INC(total1, ORD(rawRGB.g));
              INC(total2, ORD(rawRGB.b));
            END;
            INC(line[h*3], total0);
            INC(line[h*3+1], total1);
            INC(line[h*3+2], total2);
          END;
        ELSE
          unscaledLC.getRawLine();
          Cscale.ScaleRGB(ADR(line[0]), 3*lc.width, ADR(unscaledChars[0]),
                             scale);
(*
          FOR h := 0 TO (lc.width-1)*3 BY 3 DO
            scaledH := h * scale;
            total0 := 0; total1 := 0; total2 := 0;
            FOR p := 0 TO scale3 BY 3 DO
              INC(total0, ORD(unscaledChars[scaledH+p]));
              INC(total1, ORD(unscaledChars[scaledH+1+p]));
              INC(total2, ORD(unscaledChars[scaledH+2+p]));
            END;
            INC(line[h], total0);
            INC(line[h+1], total1);
            INC(line[h+2], total2);
          END;
*)
        END;
      END;
      IF lc.factor = 1 THEN
        Cscale.FastMapToChar(ADR(line[0]), 3*lc.width, ADR(toDestPixel[0]),
                             ADR(scaledChars[0]));
(*
        FOR h := 0 TO (3*lc.width)-1 DO
          scaledChars[h] := toDestPixel[line[h]];
        END;
*)
      ELSE
        FOR h := 0 TO (3*lc.width)-1 DO
          scaledChars[h] := toDestPixel[line[h] DIV lc.factor];
        END;
      END;
    END;
  END GetScaledRGBRawLine;


(* *)
(* RGB Dithering: Floyd-Steinberg error diffusion dither *)
(* *)

TYPE DitherRGB = INTEGER;
  (* Represents RGB packed 3 to a word by multiplying up by DitherPacking.
     Also used for RGB deltas, which might be in [-128..255+128].  The
     offset DitherRGBBias is used to make RGB deltas positive during
     some arithmetic operations.  *)

CONST DitherPacking = 512;
  (* Packed RGB = r + g*DitherPacking + b*DitherPacking*DitherPacking *)

CONST DitherRGBBias = 128 +
                      128*DitherPacking +
                      128*DitherPacking*DitherPacking;

CONST DitherFraction = 16;
  (* Amount by which we will divide accumulated error. This is the total
     of the error weights used in accumulating propagated error.  Assumed
     to be a power of two, to allow MOD by masking. *)

CONST DitherFractionMask = (DitherPacking-DitherFraction) +
                           (DitherPacking-DitherFraction)*DitherPacking +
                           (DitherPacking-DitherFraction)*DitherPacking*
                                                          DitherPacking;
  (* Mask to avoid propagating across fields when dividing by DitherFraction *)

CONST DitherRound = (DitherFraction +
                     DitherFraction*DitherPacking +
                     DitherFraction*DitherPacking*DitherPacking) DIV 2;
  (* To ensure unbiased rounding when dividing by DitherFraction *)

TYPE DitheredPixel = BITS 8 FOR [0..255];
  (* A dithered pixel value, as stored in the resulting .pixels *)

CONST NullDitheredPixel = LAST(DitheredPixel);
  (* An unused value for DitheredPixel, used in .posterizedToDithered *)

TYPE DitheredPixels = ARRAY OF DitheredPixel;
  (* Used for storing the resulting dithered pixels *)

TYPE DitheredRGB = Images.Contents OBJECT
    undithered: Images.Contents;
    unditheredLine: REF ARRAY OF INTEGER;
      (* Buffer for line from "undithered" if it's not a LazyPPMContents *)
    lc: LazyContents;
    levels: INTEGER;
    diffuse: BOOLEAN;
    posterize: ARRAY [0 .. LAST(Images.Channel)+128+128] OF INTEGER;
      (* indexed by a biased RGB delta: index=128 for RGB=0.  Value
         is in [0..levels) *)
    posterizedToDitheredPixel: ARRAY [0..255] OF DitheredPixel;
      (* Indexed by posterized RGB in [0..levels^3), returns pixel or Null *)
    posterizedToDitherRGB: ARRAY [0..255] OF DitherRGB;
      (* Indexed by posterized RGB, returns DitherRGB *)
    nextValue: DitheredPixel;
      (* Next unused value of DitheredPixel *)
    prevLineError: REF ARRAY OF DitherRGB;
      (* packed RGB deltas from previous line *)
    pixels: REF DitheredPixels;
      (* the dithered image *)
  METHODS
    init(undithered: Images.Contents;
         levels: INTEGER;
         diffuse: BOOLEAN): DitheredRGB := InitDitheredRGB;
      (* Initializes this Contents to deliver a version of "undithered"
         whose pixels are restricted to having the given number of
         levels in each channel.  Implemented by a simple error diffusion
         algorithm if "diffuse", and by posterizing otherwise. *)
    doLine(v: INTEGER; VAR line: DitheredPixels) := DoDitheredLine;
      (* Assembles one dithered line into "line"; private. *)
  OVERRIDES
    getLine := GetDitheredLine;
  END;

PROCEDURE InitDitheredRGB(dc: DitheredRGB;
                          undithered: Images.Contents;
                          levels: INTEGER;
                          diffuse: BOOLEAN): DitheredRGB =
  VAR
    distance := 255.0 / FLOAT(levels-1); (* distance between result rgb's *)
  BEGIN
    dc.undithered := undithered;
    TYPECASE undithered OF
    | LazyContents(lc) =>
        dc.lc := lc;
    ELSE
        dc.lc := NIL;
        dc.unditheredLine := NEW(REF ARRAY OF INTEGER, undithered.width);
    END;
    dc.levels := levels;
    dc.diffuse := diffuse;
    dc.width := undithered.width;
    dc.height := undithered.height;
    dc.isBW := TRUE;
    dc.isGray := TRUE;
    dc.isGrayRamp := TRUE;
    dc.map := NEW(Images.RGBMap, 0);
    FOR i := -128 TO LAST(Images.Channel)+128 DO
      dc.posterize[i+128] := MAX(0, MIN(levels-1, ROUND(FLOAT(i)/distance)));
    END;
    FOR i := FIRST(DitheredPixel) TO LAST(DitheredPixel) DO
      dc.posterizedToDitheredPixel[i] := NullDitheredPixel;
    END;
    dc.nextValue := 0;
    dc.pixels := NEW(REF DitheredPixels, dc.width*dc.height);
    dc.prevLineError := NEW(REF ARRAY OF DitherRGB, dc.width+1);
    FOR i := 0 TO LAST(dc.prevLineError^) DO dc.prevLineError[i] := 0 END;
    FOR v := 0 TO dc.height-1 DO
      dc.doLine(v, SUBARRAY(dc.pixels^, v*dc.width, dc.width));
    END;
    (* Reduce the map array to the correct size *)
    VAR oldMap := dc.map;
    BEGIN
      dc.map := NEW(Images.RGBMap, dc.nextValue);
      dc.map^ := SUBARRAY(oldMap^, 0, dc.nextValue);
    END;
    IF NUMBER(dc.map^) < 2 THEN dc.isGrayRamp := FALSE END;
    RETURN dc;
  END InitDitheredRGB;

PROCEDURE GetDitheredLine(dc: DitheredRGB;
                          v: INTEGER;
                          VAR line: ARRAY OF INTEGER) =
  VAR
    offset := v * dc.width;
  BEGIN
    FOR i := 0 TO dc.width-1 DO line[i] := dc.pixels[i+offset] END;
  END GetDitheredLine;

PROCEDURE DoDitheredLine(dc: DitheredRGB;
                         v: INTEGER;
                         VAR line: DitheredPixels) =
  VAR
    prevLinePrev, prevLineCurr, prevLineNext: DitherRGB;
    newError: DitherRGB := 0;
    levels := dc.levels;
    diffuse := dc.diffuse;
    chars: REF ARRAY OF CHAR;
    from, to, by: INTEGER;
  BEGIN
    IF dc.lc # NIL THEN
      dc.lc.getRawLine(); (* faster than .getLine *)
      chars := dc.lc.chars;
    ELSE
      dc.undithered.getLine(v, dc.unditheredLine^);
      chars := NIL;
    END;
    IF v MOD 2 = 0 THEN
      from := 0; to := dc.width-1; by := 1;
    ELSE
      from := dc.width-1; to := 0; by := -1;
    END;
    prevLineCurr := 0;
    IF dc.width # 0 THEN prevLineNext := dc.prevLineError[from] END;
    FOR i := from TO to BY by DO
      VAR
        rawRGB: Images.RGB;
        correctedRGB: DitherRGB;
        posterizedRGB: INTEGER;
        newPixel: DitheredPixel;
      BEGIN
        (* shuffle copies of error values from previous line *)
        prevLinePrev := prevLineCurr;
        prevLineCurr := prevLineNext;
        IF i = to THEN
          prevLineNext := 0;
        ELSE
          prevLineNext := dc.prevLineError[i+by];
        END;
        (* compute ideal RGB for current pixel, folding in accumulated error *)
        IF chars # NIL THEN
          correctedRGB := ORD(chars[i*3]) +
                          ORD(chars[i*3+1]) * DitherPacking +
                          ORD(chars[i*3+2]) * DitherPacking*DitherPacking +
                          prevLinePrev * 3 +
                          prevLineCurr * 5 +
                          prevLineNext * 1 +
                          newError * 7 +
                          DitherRGBBias;
        ELSE
          rawRGB := dc.undithered.map[dc.unditheredLine[i]];
          correctedRGB := rawRGB.r +
                          rawRGB.g * DitherPacking +
                          rawRGB.b * DitherPacking*DitherPacking +
                          prevLinePrev * 3 +
                          prevLineCurr * 5 +
                          prevLineNext * 1 +
                          newError * 7 +
                          DitherRGBBias;
        END;
        (* compute closest available RGB, and pixel value *)
        posterizedRGB := dc.posterize[correctedRGB MOD DitherPacking] +
          dc.posterize[(correctedRGB DIV DitherPacking) MOD DitherPacking] *
                                                               levels +
          dc.posterize[correctedRGB DIV (DitherPacking*DitherPacking)] *
                                                               levels * levels;
        newPixel := dc.posterizedToDitheredPixel[posterizedRGB];
        IF newPixel = NullDitheredPixel THEN
          (* First pixel with this posterizedRGB value.  Set up the
             .map, .posterizedToDitherRGB and .posterizedToDitheredPixel
             arrays *)
          newPixel := dc.nextValue; INC(dc.nextValue);
          IF LAST(dc.map^) < newPixel THEN
            (* Extend the map, generously.  At end, caller will truncate it *)
            VAR oldMap := dc.map;
            BEGIN
              dc.map := NEW(Images.RGBMap, NUMBER(oldMap^)*2+1);
              SUBARRAY(dc.map^, 0, NUMBER(oldMap^)) := oldMap^;
            END;
          END;
          WITH
            distance = 255.0 / FLOAT(levels-1),
            r = posterizedRGB MOD levels,
            g = (posterizedRGB DIV levels) MOD levels,
            b = posterizedRGB DIV (levels*levels),
            rgb = Images.RGB{r := ROUND(FLOAT(r)*distance),
                             g := ROUND(FLOAT(g)*distance),
                             b := ROUND(FLOAT(b)*distance)} DO
            dc.map[newPixel] := rgb;
            dc.posterizedToDitherRGB[posterizedRGB] :=
                              rgb.r +
                              rgb.g*DitherPacking +
                              rgb.b*DitherPacking*DitherPacking;
            IF dc.isBW OR dc.isGray THEN
              IF rgb.r # rgb.g OR rgb.g # rgb.b THEN
                dc.isBW := FALSE;
                dc.isGray := FALSE;
                dc.isGrayRamp := FALSE;
              ELSIF rgb.r # 0 AND rgb.r # 255 THEN
                dc.isBW := FALSE;
                dc.isGrayRamp := FALSE;
              END;
            END;
          END;
          dc.posterizedToDitheredPixel[posterizedRGB] := newPixel;
        END;
        line[i] := newPixel;
        (* compute and record error in pixel's actual RGB *)
        IF diffuse THEN
          newError := Word.And(correctedRGB -
                               dc.posterizedToDitherRGB[posterizedRGB] +
                               DitherRound,
                               DitherFractionMask) DIV DitherFraction -
                      DitherRGBBias DIV DitherFraction;
        END;
        (* Note: "newError" is required for next loop iteration *)
        (* Note: we have prev line's error in "prevLineCurr", so we can
                 overwrite that cell to record this line's error. *)
        dc.prevLineError[i] := newError;
      END;
    END;
  END DoDitheredLine;


(* *)
(* B&W and Gray Scaling *)
(* *)

TYPE ScaledGray = Images.Contents OBJECT
    (* An Images.Contents object that scales a given Images.Contents object. *)
    unscaled: Images.RawContents;
    scale: INTEGER;
    toGray: Images.GrayMap;
    factor: INTEGER;
    toDestPixel: REF ARRAY OF INTEGER;
  METHODS
    init(unscaled: Images.RawContents;
         scale: INTEGER;
         gamma: REAL;
         levels: INTEGER): ScaledGray := InitScaledGray;
      (* Initializes this Contents to deliver a version of "unscaled" that
         is scaled by "scale", converted to grayscale, and adjusted by "gamma"
         (gamma < 1.0 moves pixel values toward white).  The resulting
         contents are gray or B&W with at most "levels" levels. *)
  OVERRIDES
    getLine := GetScaledLine;
  END;

PROCEDURE InitScaledGray(sc: ScaledGray;
                             unscaled: Images.RawContents;
                             scale: INTEGER;
                             gamma: REAL;
                             levels: INTEGER): ScaledGray =
  (* Sets up the various fields of "sc" so that GetScaledLine will work.
     The plan is to do as much as possible here, to speed up GetScaledLine.
     In particular, sc.toDestPixel is set up as an array indexed by
     accumulated value, yielding a pixel "p" such that sc.map[p] is the
     appropriate RGB value.  The "accumulated value" used by GetScaledLine
     is the total brightness, or the total number of "1" bits, depending
     on unscaled.raw.bitsPerPixel; divided by sc.factor to keep the size of
     sc.toDestPixel reasonable.  Sets up sc.toGray to yield the brightness
     of pixels in unscaled.raw. *)
  VAR
    raw := unscaled.raw;
    scaledMax: INTEGER;  (* the maximum value that will be accumulated *)
    gray, lastGray, zeroBit, oneBit: REAL;
  BEGIN
    <* ASSERT raw.westRounded = 0 AND
              raw.pixelOrder = PaintPrivate.HostByteOrder *>
    sc.width := Rect.HorSize(raw.bounds) DIV scale;
    sc.height := Rect.VerSize(raw.bounds) DIV scale;
    sc.unscaled := unscaled;
    sc.scale := scale;
    sc.toGray := Images.GrayMapFromRGBMap(unscaled.map);
    IF unscaled.isBW AND scale = 1 THEN levels := 2 END;
    IF unscaled.raw.bitsPerPixel = 1 THEN
      scaledMax := scale * scale;
      zeroBit := FLOAT(sc.toGray[0]) / 255.0;
      oneBit := FLOAT(sc.toGray[1]) / 255.0;
    ELSE
      scaledMax := scale * scale * 255;
    END;
    sc.factor := (scaledMax+1+255) DIV 256; (* brings index into [0..255] *)
    sc.toDestPixel := NEW(REF ARRAY OF INTEGER, (scaledMax DIV sc.factor)+1);
    lastGray := FLOAT(LAST(sc.toDestPixel^));
    FOR i := 0 TO LAST(sc.toDestPixel^) DO
      gray := FLOAT(i) / lastGray; (* gray level of accumulation *)
      IF unscaled.raw.bitsPerPixel = 1 THEN
        gray := oneBit * gray + zeroBit * (1.0-gray);
      END;
      IF gamma # 1.0 THEN
        gray := FLOAT(Math.pow(FLOAT(gray, LONGREAL),
                               FLOAT(1.0/gamma, LONGREAL)));
      END;
      (* convert to [0..levels) with white=0 *)
      sc.toDestPixel[i] := ROUND((1.0-gray) * FLOAT(levels-1));
    END;
    sc.map := NEW(Images.RGBMap, levels);
    FOR i := 0 TO LAST(sc.map^) DO
      WITH gray255 = 255 - ROUND((FLOAT(i) / FLOAT(levels-1)) * 255.0) DO
        sc.map[i] := Images.RGB{ r := gray255, g := gray255, b := gray255 };
      END;
    END;
    sc.isBW := NUMBER(sc.map^) = 2;
    sc.isGray := TRUE;
    sc.isGrayRamp := TRUE;
    RETURN sc
  END InitScaledGray;

PROCEDURE GetScaledLine(sc: ScaledGray;
                        v: INTEGER;
                        VAR line: ARRAY OF INTEGER) =
  (* "getLine" method for ScaledGray. *)
  VAR
    raw := sc.unscaled.raw;
    scale := sc.scale;
    factor := sc.factor;
    toDestPixel := sc.toDestPixel;
    scaledWidth := Rect.HorSize(raw.bounds) DIV scale;
    rowStart := (v*scale-raw.bounds.north)*raw.wordsPerRow + raw.offset;
  BEGIN
    RTMisc.Zero(ADR(line[0]), NUMBER(line)*BYTESIZE(Word.T));
    FOR vdelta := 0 TO sc.scale-1 DO
      VAR
        pixels := LOOPHOLE(ADR(raw.pixels[rowStart+vdelta*raw.wordsPerRow]),
                         UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord);
      BEGIN
        IF raw.bitsPerPixel = 1 THEN
          IF raw.pixelOrder = PaintPrivate.ByteOrder.LSBFirst THEN
            CASE scale OF
            | 2 => Cscale.By2(ADR(line[0]), ADR(pixels[0]), scaledWidth);
            | 3 => Cscale.By3(ADR(line[0]), ADR(pixels[0]), scaledWidth);
            | 4 => Cscale.By4(ADR(line[0]), ADR(pixels[0]), scaledWidth);
            | 48 => Cscale.By48(ADR(line[0]), ADR(pixels[0]), scaledWidth);
            ELSE Cscale.ByN(ADR(line[0]), ADR(pixels[0]), scaledWidth, scale);
            END;
          ELSE
            CASE sc.scale OF
            | 2 => Cscale.By2R(ADR(line[0]), ADR(pixels[0]), scaledWidth);
            | 4 => Cscale.By4R(ADR(line[0]), ADR(pixels[0]), scaledWidth);
            | 48 => Cscale.By48R(ADR(line[0]), ADR(pixels[0]), scaledWidth);
            ELSE Cscale.ByNR(ADR(line[0]), ADR(pixels[0]), scaledWidth, scale);
            END;
          END;
        ELSE
          IF raw.pixelOrder = PaintPrivate.ByteOrder.LSBFirst THEN
            SlowScale(sc, SUBARRAY(line, 0, scaledWidth), pixels);
          ELSE
            SlowScaleR(sc, SUBARRAY(line, 0, scaledWidth), pixels);
          END;
        END;
      END;
    END;
    IF factor = 1 THEN
      Cscale.FastMap(ADR(line[0]), scaledWidth, ADR(toDestPixel[0]));
    ELSE
      FOR p := 0 TO scaledWidth - 1 DO
        line[p] := toDestPixel[line[p] DIV factor];
      END;
    END;
  END GetScaledLine;

(*
PROCEDURE FastScaleBy2(
       VAR line: ARRAY OF INTEGER;
       pixels: UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord ) =
  CONST PixelsPerWord = BITSIZE(ScrnPixmap.PixWord) DIV 2;
  VAR
    source := pixels[0];
    sourceWord := 0;
    i := 0; j := 0;
  BEGIN
    WHILE (i < NUMBER(line)) DO
      IF source = 0 THEN
        INC(i, PixelsPerWord - j);
        INC(sourceWord);
        source := pixels[sourceWord];
        j := 0;
      ELSE
        INC(line[i], bitsIn[Word.And(source, 3)]);
        INC(i);
        INC(j);
        IF j = PixelsPerWord THEN
          INC(sourceWord);
          source := pixels[sourceWord];
          j := 0;
        ELSE
          source := Word.RightShift(source, 2);
        END;
      END;
    END;
  END FastScaleBy2;

PROCEDURE FastScaleBy3(
       VAR line: ARRAY OF INTEGER;
       pixels: UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord ) =
  CONST PixelsPerWord = BITSIZE(ScrnPixmap.PixWord) DIV 3;
  VAR
    i := 0;
    j := 0;
    grabBit := 1;    (* must change if BITSIZE(ScrnPixmap.PixWord) # 32 *)
    source := pixels[0];
    sourceWord := 0;
    temp: ScrnPixmap.PixWord;
  BEGIN
    WHILE i < NUMBER(line) DO
      IF j = PixelsPerWord THEN
        temp := source;
        INC(sourceWord);
        source := pixels[sourceWord];
        CASE grabBit OF
        | 0 =>
            INC(line[i], bitsIn[Word.And(source, 7)]);
            INC(i);
            grabBit := 1;
            j := 1;
            source := Word.RightShift(source, 3);
        | 1 =>
            INC(line[i], bitsIn[temp] + Word.And(source, 1));
            INC(i);
            source := Word.RightShift(source, 1);
            grabBit := 2;
            j := 0;
        ELSE  (*2*)
            INC(line[i], temp + bitsIn[Word.And(source, 3)]);
            INC(i);
            source := Word.RightShift(source, 2);
            grabBit := 0;
            j := 0;
        END;
      ELSIF source = 0 THEN
        INC(i, PixelsPerWord - j);
        j := PixelsPerWord;
      ELSE
        INC(line[i], bitsIn[Word.And(source, 7)]);
        INC(i);
        INC(j);
        source := Word.RightShift(source, 3);
      END;
    END;
  END FastScaleBy3;

PROCEDURE FastScaleBy4(
       VAR line: ARRAY OF INTEGER;
       pixels: UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord ) =
  CONST PixelsPerWord = BITSIZE(ScrnPixmap.PixWord) DIV 4;
  VAR
    source := pixels[0];
    sourceWord := 0;
    i := 0; j := 0;
  BEGIN
    WHILE (i < NUMBER(line)) DO
      IF source = 0 THEN
        INC(i, PixelsPerWord - j);
        INC(sourceWord);
        source := pixels[sourceWord];
        j := 0;
      ELSE
        INC(line[i], bitsIn[Word.And(source, 15)]);
        INC(i);
        INC(j);
        IF j = PixelsPerWord THEN
          INC(sourceWord);
          source := pixels[sourceWord];
          j := 0;
        ELSE
          source := Word.RightShift(source, 4);
        END;
      END;
    END;
  END FastScaleBy4;

PROCEDURE FastScaleBy48(
       VAR line: ARRAY OF INTEGER;
       pixels: UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord ) =
  CONST PixelsPerWord = BITSIZE(ScrnPixmap.PixWord) DIV 8;
  VAR
    k: INTEGER;
    j := PixelsPerWord;
    source := pixels[0];
    sourceWord := 0;
  BEGIN
    FOR i := 0 TO LAST(line) DO
      k := 6;
      WHILE k > 0 DO
        IF source = 0 THEN
          IF j <= k THEN
            DEC(k, j);
            j := PixelsPerWord;
            INC(sourceWord);
            source := pixels[sourceWord];
          ELSE
            DEC(j, k);
            EXIT;
          END;
        ELSE
          DEC(k); DEC(j);
          INC(line[i], bitsIn[Word.And(source, 255)]);
          IF j = 0 THEN
            INC(sourceWord);
            source := pixels[sourceWord];
            j := PixelsPerWord;
          ELSE
            source := Word.RightShift(source, 8);
          END;
        END;
      END;
    END;
  END FastScaleBy48;

PROCEDURE FastScaleByN(
       VAR line: ARRAY OF INTEGER;
       pixels: UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord;
       scale: INTEGER ) =
  VAR
    source := pixels[0];
    sourceWord := 0;
    j := 0;
  BEGIN
    FOR i := 0 TO LAST(line) DO
      FOR k := 0 TO scale-1 DO
        IF Word.And(source, 1) # 0 THEN INC(line[i]); END;
        source := Word.RightShift(source, 1);
        INC(j);
        IF j = BITSIZE(ScrnPixmap.PixWord) THEN
          INC(sourceWord);
          source := pixels[sourceWord];
          j := 0;
        END;
      END;
    END;
  END FastScaleByN;

PROCEDURE FastScaleBy2R(
       VAR line: ARRAY OF INTEGER;
       pixels: UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord ) =
  VAR
    source := pixels[0];
    sourceWord := 0;
    j := BITSIZE(ScrnPixmap.PixWord);
  BEGIN
    FOR i := 0 TO LAST(line) DO
      DEC(j, 2);
      INC(line[i], bitsIn[Word.Extract(source, j, 2)]);
      IF j = BITSIZE(ScrnPixmap.PixWord) THEN
        INC(sourceWord);
        source := pixels[sourceWord];
        j := BITSIZE(ScrnPixmap.PixWord);
      END;
    END;
  END FastScaleBy2R;

PROCEDURE FastScaleBy4R(
       VAR line: ARRAY OF INTEGER;
       pixels: UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord ) =
  VAR
    source := pixels[0];
    sourceWord := 0;
    j := BITSIZE(ScrnPixmap.PixWord);
  BEGIN
    FOR i := 0 TO LAST(line) DO
      DEC(j, 4);
      INC(line[i], bitsIn[Word.Extract(source, j, 4)]);
      IF j = BITSIZE(ScrnPixmap.PixWord) THEN
        INC(sourceWord);
        source := pixels[sourceWord];
        j := BITSIZE(ScrnPixmap.PixWord);
      END;
    END;
  END FastScaleBy4R;

PROCEDURE FastScaleByNR(
       VAR line: ARRAY OF INTEGER;
       pixels: UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord;
       scale: INTEGER ) =
  VAR
    source := pixels[0];
    sourceWord := 0;
    j := BITSIZE(ScrnPixmap.PixWord);
  BEGIN
    FOR i := 0 TO LAST(line) DO
      FOR hdelta := 0 TO scale-1 DO
        IF source >= 16_80000000 THEN INC(line[i]); END;
        source := Word.LeftShift(source, 1);
        DEC(j);
        IF j = 0 THEN
          INC(sourceWord);
          source := pixels[sourceWord];
          j := BITSIZE(ScrnPixmap.PixWord);
        END;
      END;
    END;
  END FastScaleByNR;
*)

PROCEDURE SlowScale(
       sc: ScaledGray; VAR line: ARRAY OF INTEGER;
       pixels: UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord ) =
  VAR
    scale := sc.scale;
    bitsPerPixel := sc.unscaled.raw.bitsPerPixel;
    sourceWord := 0;
    source := pixels[0];
    total, j: INTEGER;
    toGray := sc.toGray;
  BEGIN
    CheckPixelSize(bitsPerPixel);
    j := 0;
    FOR i := 0 TO LAST(line) DO
      total := 0;
      FOR k := 0 TO scale-1 DO
        INC(total, toGray[Word.Extract(source,
                                      j,
                                      bitsPerPixel)]);
        INC(j, bitsPerPixel);
        IF j = BITSIZE(ScrnPixmap.PixWord) THEN
          INC(sourceWord);
          source := pixels[sourceWord];
          j := 0;
        END;
      END;
      INC(line[i], total);
    END;   (* of i loop *)
  END SlowScale;

PROCEDURE SlowScaleR(
       sc: ScaledGray; VAR line: ARRAY OF INTEGER;
       pixels: UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord ) =
  VAR
    scale := sc.scale;
    bitsPerPixel := sc.unscaled.raw.bitsPerPixel;
    sourceWord := 0;
    source := pixels[0];
    total, j: INTEGER;
    toGray := sc.toGray;
  BEGIN
    CheckPixelSize(bitsPerPixel);
    j := BITSIZE(ScrnPixmap.PixWord);
    FOR i := 0 TO LAST(line) DO
      total := 0;
      FOR k := 0 TO scale-1 DO
        DEC(j, bitsPerPixel);
        INC(total, toGray[Word.Extract(source,
                                       j,
                                       bitsPerPixel)]);
        IF j = 0 THEN
          INC(sourceWord);
          source := pixels[sourceWord];
          j := BITSIZE(ScrnPixmap.PixWord);
        END;
      END;
      INC(line[i], total);
    END;
  END SlowScaleR;

EXCEPTION BadPixelSize;

PROCEDURE CheckPixelSize(n: INTEGER) =
    <* FATAL BadPixelSize *>
  BEGIN
    IF n > BITSIZE(ScrnPixmap.PixWord) OR
            (BITSIZE(ScrnPixmap.PixWord) MOD n) # 0 THEN
      RAISE BadPixelSize;
    END;
  END CheckPixelSize;


(* *)
(* Compression algorithm *)
(* *)

PROCEDURE ComputePacking(maxVal: INTEGER): INTEGER =
  (* Determines how many pixels in [0..maxVal] will fit in one byte. *)
  VAR pixelsPerGroup: INTEGER;
  BEGIN
    IF maxVal <= 1 THEN
      pixelsPerGroup := 8;
    ELSIF maxVal <= 2 THEN
      pixelsPerGroup := 5;
    ELSIF maxVal <= 3 THEN
      pixelsPerGroup := 4;
    ELSIF maxVal <= 5 THEN
      pixelsPerGroup := 3;
    ELSIF maxVal <= 15 THEN
      pixelsPerGroup := 2;
    ELSIF maxVal > 255 THEN
      <* ASSERT FALSE *> (* Can't pack into a byte *)
    ELSE
      pixelsPerGroup := 1;
    END;
    RETURN pixelsPerGroup
  END ComputePacking;

PROCEDURE Compress(contents: Images.Contents; scale: INTEGER;
                   wr: Wr.T; verbose: BOOLEAN)
                RAISES { Wr.Failure, Thread.Alerted } =
  (* Write a compressed form of "contents" on "wr". The parameter "scale"
     is recorded in the output file, but has no other effect.

     Uses a simple two-dimensional run-encoding scheme.  Output is byte
     oriented, to allow fast decompression.  The algorithm run-encodes
     pixel groups whose value is "Zero", and pixel groups that equal
     the corresponding groups of the previous scan-line.

     The resulting compression ratios are similar to GNU zip applied to
     typical 300 DPI B&W text image data, and better than LZW.  If the output
     image has pixel depth 1, the resulting compression ratios are roughly a
     factor of two worse than group 4 fax, but decompression can be much
     faster.  The output can profitable be re-compressed by a program such as
     GNU zip, gaining another factor of roughly 1.3, at the cost of slightly
     slower decompression.

     Output format is byte = P = FirstNonLiteral+n, where n means:
       n=[0..13]  => run of n+1 groups, value=Zero
       n=14       => next byte = Q => run of Q+15 groups, value=Zero
       n=[15..28] => run of n-14 groups equal to previous line
       n=29       => next byte = Q => run of Q+15 groups equal to previous line
       n=30       => next byte = Q => literal group, value=Q
       n=31       => end of file marker (there are two)
       n=other    => literal group, value=P

     "FirstNonLiteral" is chosen to maximize pixel groups whose value can
     be represented as a non-escaped literal, heuristically.  For scale=1,
     the maximum group value is 255, but the range [160..190] are somewhat
     unlikely values.  (The top 3 pixels in a group whose value is in
     [160..191] are BWB, corresponding to an isolated white pixel.)
     *)
  CONST
    SimpleRunMax = 14;                 (* longest run encoded in first byte *)
    EscapeRunMax = 14+256;             (* longest run encoded in second byte *)
    FirstNonLiteral = 160;             (* first byte value used for encoding *)
    RunZBase = FirstNonLiteral;        (* runs of zeroes *)
    RunZEscape = RunZBase+SimpleRunMax;(* second byte is run length *)
    RunPBase = RunZEscape+1;           (* runs of matches to prev. scan-line *)
    RunPEscape = RunPBase+SimpleRunMax;(* second byte is run length *)
    LiteralEscape = RunPEscape+1;      (* second byte is literal byte value *)
    EOF = LiteralEscape+1;             (* end-of-file marker *)
    LastNonLiteral = EOF;              (* last byte value used for encoding *)
    Zero = VAL(0, CHAR);               (* zero pixel group *)
  VAR
    unscaledBitmap: BOOLEAN;           (* for optimization/hack below *)
    raw: ScrnPixmap.Raw;               (* ditto *)
    maxVal: INTEGER;                   (* largest pixel from contents.getLine *)
    line: REF ARRAY OF INTEGER;        (* buffer for contents.getLine *)
    pixelsPerGroup: INTEGER;           (* pixels in packing group *)
    wGroups: INTEGER;                  (* groups per scan-line *)
    height := contents.height;         (* scan-lines in output *)
    this, prev: REF ARRAY OF CHAR;     (* buffers for compressed lines *)
    pr: UNTRACED REF ARRAY [0..999999] OF CHAR;
    th: UNTRACED REF ARRAY [0..999999] OF CHAR;
  CONST
    CharBuffLen = 1000;
  VAR
    charBuff := NEW(REF ARRAY OF CHAR, CharBuffLen);
    charBuffPos := 0;
  PROCEDURE FlushCharBuff() RAISES { Wr.Failure, Thread.Alerted } =
    BEGIN
      Wr.PutString(wr, SUBARRAY(charBuff^, 0, charBuffPos));
      charBuffPos := 0;
    END FlushCharBuff;
  PROCEDURE PutChar(c: [0..255]) RAISES { Wr.Failure, Thread.Alerted } =
    BEGIN
      IF charBuffPos = CharBuffLen THEN FlushCharBuff() END;
      charBuff[charBuffPos] := VAL(c, CHAR);
      INC(charBuffPos);
    END PutChar;
  (* statistics *)
  VAR
    runZCount, runZEscCount, runZBytes,
    runPCount, runPEscCount, runPBytes,
    literalCount, literalEscCount: INTEGER := 0;
  BEGIN
    IF verbose THEN
      Wr.PutText(Stdio.stderr, "  Compress ... ");
      Wr.Flush(Stdio.stderr);
    END;
    maxVal := LAST(contents.map^);
    TYPECASE contents OF
    | Images.RawContents(sc) =>
        raw := sc.raw;
        unscaledBitmap := maxVal = 1 AND contents.isBW;
    ELSE
      raw := NIL;
      unscaledBitmap := FALSE;
    END;
    line := NEW(REF ARRAY OF INTEGER, contents.width);
    pixelsPerGroup := ComputePacking(maxVal);
    wGroups := contents.width DIV pixelsPerGroup;
    IF wGroups * pixelsPerGroup = 0 THEN height := 0 END;
    this := NEW(REF ARRAY OF CHAR, wGroups);
    prev := NEW(REF ARRAY OF CHAR, wGroups);
    Wr.PutChar(wr, 'L');
    IF contents.isGrayRamp THEN
      Wr.PutChar(wr, 'G');
    ELSE
      Wr.PutChar(wr, 'M');
    END;
    Wr.PutText(wr, "\n" & Fmt.Int(wGroups * pixelsPerGroup) & "\n" &
                          Fmt.Int(height) & "\n" &
                          Fmt.Int(scale) & "\n" &
                          Fmt.Int(maxVal) & "\n");
    IF NOT contents.isGrayRamp THEN
      FOR i := 0 TO LAST(contents.map^) DO
        PutChar(contents.map[i].r);
        PutChar(contents.map[i].g);
        PutChar(contents.map[i].b);
      END;
    END;
    FOR v := 0 TO height-1 DO
      VAR
        h := 0;
        temp := prev;
      PROCEDURE PutZRun(run: INTEGER) RAISES { Wr.Failure, Thread.Alerted } =
        BEGIN
          INC(runZCount);
          INC(runZBytes, run);
          IF run <= SimpleRunMax THEN
            PutChar(RunZBase+run-1);
          ELSE
            PutChar(RunZEscape);
            PutChar(run-SimpleRunMax-1);
            INC(runZEscCount);
          END;
        END PutZRun;
      PROCEDURE PutPRun(run: INTEGER) RAISES { Wr.Failure, Thread.Alerted } =
        BEGIN
          INC(runPCount);
          INC(runPBytes, run);
          IF run <= SimpleRunMax THEN
            PutChar(RunPBase+run-1);
          ELSE
            PutChar(RunPEscape);
            PutChar(run-SimpleRunMax-1);
            INC(runPEscCount);
          END;
        END PutPRun;
      BEGIN
        prev := this; this := temp;
        (* Get the pixel groups for this scan line ... *)
        IF unscaledBitmap THEN
          WITH
            rowStart = (v - raw.bounds.north) * raw.wordsPerRow + raw.offset
          DO
            pr := th;
            th := LOOPHOLE(ADR(raw.pixels[rowStart]),
                           UNTRACED REF ARRAY [0..999999] OF CHAR);
          END;
        ELSE
          th := LOOPHOLE(ADR(this[0]), UNTRACED REF ARRAY [0..999999] OF CHAR);
          pr := LOOPHOLE(ADR(prev[0]), UNTRACED REF ARRAY [0..999999] OF CHAR);
          contents.getLine(v, line^);
          (* Pack into groups *)
          IF pixelsPerGroup = 4 THEN
            Cscale.FastPack(ADR(line[0]), ADR(this[0]), wGroups);
          ELSE
            VAR lineH := 0; BEGIN
              FOR packedH := 0 TO wGroups-1 DO
                VAR value := 0; BEGIN
                  FOR i := 0 TO pixelsPerGroup-1 DO
                    value := value * (maxVal+1) + line[lineH+i];
                  END;
                  INC(lineH, pixelsPerGroup);
                  this[packedH] := VAL(value, CHAR);
                END;
              END;
            END;
          END;
        END;
        (* Compress the groups ... *)
        WHILE h < wGroups DO
          VAR x := th[h]; BEGIN
            IF v > 0 AND x = pr[h] THEN
              VAR diff := Cscale.FinishPRun(
                     ADR(th[h]), ADR(pr[h]), MIN(wGroups-h,EscapeRunMax));
              BEGIN
                IF diff >= 0 THEN
                  PutPRun(diff);
                  INC(h, diff);
                ELSE
                  PutZRun(-diff);
                  DEC(h, diff);
                END;
              END;
            ELSIF x = Zero THEN
              VAR diff := Cscale.FinishZRun(
                     ADR(th[h]), MIN(wGroups-h,EscapeRunMax));
              BEGIN
                PutZRun(diff);
                INC(h, diff);
              END;
            ELSE
              IF unscaledBitmap THEN x := rawToLGM[x]; END;
              INC(literalCount);
              IF ORD(x) >=FirstNonLiteral AND ORD(x) <= LastNonLiteral THEN
                INC(literalEscCount);
                PutChar(LiteralEscape);
                PutChar(ORD(x));
              ELSE
                PutChar(ORD(x));
              END;
              INC(h);
            END;
          END;  (* VAR x *)
        END;  (* WHILE *)
      END;
    END;
    FOR i := 1 TO 2 DO PutChar(EOF) END;
    FlushCharBuff();
    IF FALSE THEN
      Wr.PutText(Stdio.stderr,
                 Fmt.Int(runZCount) & " zero runs, including " &
                 Fmt.Int(runZEscCount) & " escaped; " &
                 Fmt.Int(runZBytes) & " total bytes\n");
      Wr.PutText(Stdio.stderr,
                 Fmt.Int(runPCount) & " prev runs, including " &
                 Fmt.Int(runPEscCount) & " escaped; " &
                 Fmt.Int(runPBytes) & " total bytes\n");
      Wr.PutText(Stdio.stderr,
                 Fmt.Int(literalCount) & " literals, including " &
                 Fmt.Int(literalEscCount) & " escaped\n");
      Wr.Flush(Stdio.stderr);
    END;
  END Compress;

(*
PROCEDURE FinishPRun(
    th: UNTRACED REF ARRAY [0..999999] OF CHAR;
    pr: UNTRACED REF ARRAY [0..999999] OF CHAR;
    h, lim: INTEGER): INTEGER =
  VAR c: CHAR := th[h];
      h0 := h;
      hlim := h + lim;
  CONST Zero = VAL(0, CHAR);               (* zero pixel group *)
  BEGIN
    LOOP
      IF (c # Zero) THEN
        (* PRun longer than ZRun *)
        REPEAT INC(h); UNTIL (h = hlim OR th[h] # pr[h]);
        RETURN h-h0;
      END;
      INC(h);
      IF h = hlim THEN
        (* PRun and ZRun equal length, but at run limit *)
        RETURN lim;
      END;
      c := th[h];
      IF c # pr[h] THEN
        IF c = Zero THEN
          (* ZRun longer than PRun *)
          REPEAT INC(h); UNTIL (h = hlim OR th[h] # Zero);
          RETURN -(h-h0);
        ELSE
          (* ZRun and PRun at equal non-limit length *)
          RETURN h-h0;
        END;
      END;
    END;
    <*NOWARN*>
  END FinishPRun;

PROCEDURE FinishZRun(
    th: UNTRACED REF ARRAY [0..999999] OF CHAR;
    h, lim: INTEGER): INTEGER =
  VAR h0 := h;
      hlim := h + lim;
  CONST Zero = VAL(0, CHAR);               (* zero pixel group *)
  BEGIN
    REPEAT INC(h); UNTIL (h = hlim OR th[h] # Zero);
    RETURN h-h0;
  END FinishZRun;
*)


(* *)
(* Exported Procedures (other than "Compress", which occurs earlier) *)
(* *)

PROCEDURE ReadImage(file: RegularFile.T; start, length: INTEGER; VAR t: T)
                    RAISES { Error } =
  BEGIN
    IF t = NIL THEN t := NEW(T) END;
    IF t.dupRd # NIL THEN
      TRY Rd.Close(t.dupRd) EXCEPT Rd.Failure, Thread.Alerted => END;
    END;
    t.file := file;
    t.start := start;
    t.length := length;
    TRY
      EVAL file.seek(RegularFile.Origin.Beginning, 0);
      t.dupRd := NEW(OSUtils.DupRd).init(file);
      IF Rd.Index(t.dupRd) # start THEN Rd.Seek(t.dupRd, start) END;
      TRY
        t.vanilla := NEW(LazyPPM).init(t.dupRd);
      EXCEPT Images.Error =>
        t.vanilla := t.init(t.dupRd, start, length).contents();
      END;
    EXCEPT
      | OSError.E, Rd.Failure, Thread.Alerted =>
          RAISE Error("Can't read image file");
      | Images.Error(msg) => RAISE Error(msg);
    END;
  END ReadImage;

PROCEDURE GetFormat(t: T): Format =
  BEGIN
    TYPECASE t.vanilla OF
    | LazyPPM => RETURN Format.PPM;
    | ImageRd.Contents(iv) => RETURN iv.format;
    ELSE
      <* ASSERT FALSE *>
      RETURN Format.PPM
    END;
  END GetFormat;

PROCEDURE GetHeight(t: T): CARDINAL =
  BEGIN
    RETURN t.vanilla.height;
  END GetHeight;

PROCEDURE Reduce(t: T; scale: INTEGER; gamma: REAL; levels: INTEGER;
                 diffuse: BOOLEAN;
                 verbose: BOOLEAN): Images.Contents
                 RAISES { Error, Wr.Failure, Thread.Alerted } =
  (* Generate compressed LGM from "t" with given scale and gamma. *)
  VAR
    processed: Images.Contents;
    scaleAction, ditherAction: CHAR;
  BEGIN
    TRY
      IF verbose THEN
        Wr.PutText(Stdio.stderr, "  Scale by " & Fmt.Int(scale) & " ... ");
        Wr.Flush(Stdio.stderr);
      END;
      TYPECASE t.vanilla OF
      | LazyPPM(lc) =>
          IF Rd.Index(lc.rd) # t.start THEN
            (* Restart the lazy PPM contents *)
            Rd.Seek(lc.rd, t.start);
            TRY
              EVAL lc.init(lc.rd);
            EXCEPT Images.Error => <*ASSERT FALSE*>
            END;
          END;
      ELSE
      END;
      processed := t.vanilla;
      IF scale = 1 THEN
        (* Don't scale.  Note that this also does no gamma adjustment. *)
        scaleAction := 'N';
      ELSIF t.vanilla.isBW THEN
        processed := NEW(ScaledGray).init(processed, scale, gamma, levels);
        scaleAction := 'B';
      ELSIF t.vanilla.isGray THEN
        processed := NEW(ScaledGray).init(processed, scale, gamma, 255);
        scaleAction := 'G';
      ELSE
        processed := NEW(ScaledRGB).init(processed, scale, gamma);
        scaleAction := 'R';
      END;
      IF t.vanilla.isBW THEN
        ditherAction := 'N';
      ELSE
        processed := NEW(DitheredRGB).init(processed, levels, diffuse);
        ditherAction := 'D';
      END;
      IF verbose THEN
        Wr.PutText(Stdio.stderr, "(");
        Wr.PutChar(Stdio.stderr, scaleAction);
        Wr.PutChar(Stdio.stderr, ditherAction);
        Wr.PutText(Stdio.stderr, ") ");
      END;
    EXCEPT
    | Images.Error(msg) => RAISE Error(msg);
    | Rd.Failure => RAISE Error("Can't read image file");
    END;
    IF verbose THEN
      IF processed.isBW THEN
        Wr.PutText(Stdio.stderr, "black & white.");
      ELSE
        Wr.PutText(Stdio.stderr,  Fmt.Int(NUMBER(processed.map^)));
        IF processed.isGray THEN
          Wr.PutText(Stdio.stderr, " grays.");
        ELSE
          Wr.PutText(Stdio.stderr, " colors.");
        END;
      END;
      Wr.Flush(Stdio.stderr);
    END;
    <* ASSERT NOT ISTYPE(processed, LazyContents) *>
    RETURN processed;
  END Reduce;


(* *)
(* Initialization *)
(* *)

BEGIN
  Cscale.ScaleInit();
  InitBitsIn();
  InitReversedBits();
  IF PaintPrivate.HostByteOrder = PaintPrivate.ByteOrder.MSBFirst THEN
    rawToLGM := plainBits;
  ELSE
    rawToLGM := reversedBits;
  END;
END LGM.
