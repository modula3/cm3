(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* The object class for paintable images. *)

(* Last modified on Fri Apr 14 10:18:46 PDT 1995 by birrell   *)
(*      modified on Tue Jan 31 10:53:38 PST 1995 by kalsow    *)

UNSAFE MODULE Images EXPORTS Images;

IMPORT Fmt, PaintPrivate, Point, Rect, ScrnPixmap, Thread, VBT, Word, Wr;

PROCEDURE RawGetLine(c: RawContents; v: INTEGER; VAR line: ARRAY OF INTEGER) =
  VAR
    raw := c.raw;
    rowStart := (v-raw.bounds.north)*raw.wordsPerRow + raw.offset;
    bitsPerPixel := raw.bitsPerPixel;
    pixelsPerWord := BITSIZE(ScrnPixmap.PixWord) DIV bitsPerPixel;
    sourceBitDelta, sourceBitOrigin: INTEGER;
  BEGIN
    IF raw.pixelOrder = PaintPrivate.ByteOrder.LSBFirst THEN
      sourceBitDelta := raw.bitsPerPixel;
      sourceBitOrigin := 0;
    ELSE
      sourceBitDelta := - raw.bitsPerPixel;
      sourceBitOrigin := (pixelsPerWord-1) * raw.bitsPerPixel;
    END;
    VAR
      pixels := LOOPHOLE(ADR(raw.pixels[rowStart]),
                         UNTRACED REF ARRAY [0..999999] OF ScrnPixmap.PixWord);
      sourceWord := 0;
      sourceBit := sourceBitOrigin;
      sourceCount := pixelsPerWord;
      source := pixels[0];
    BEGIN
      FOR destH := 0 TO Rect.HorSize(raw.bounds)-1 DO
        line[destH] := Word.Extract(source, sourceBit, bitsPerPixel);
        INC(sourceBit, sourceBitDelta);
        DEC(sourceCount);
        IF sourceCount = 0 THEN
          INC(sourceWord);
          source := pixels[sourceWord];
          sourceBit := sourceBitOrigin;
          sourceCount := pixelsPerWord;
        END;
      END;
    END;
  END RawGetLine;

PROCEDURE BitFromGray(g: Gray): Bit =
  BEGIN
    RETURN ORD(g < 128)
  END BitFromGray;

PROCEDURE GrayFromBit(b: Bit): Gray =
  BEGIN
    RETURN (1-b) * 255
  END GrayFromBit;

PROCEDURE GrayFromRGB(rgb: RGB): Gray =
  BEGIN
    RETURN ROUND(0.239*FLOAT(rgb.r) + 0.686*FLOAT(rgb.g) + 0.075*FLOAT(rgb.b))
  END GrayFromRGB;

PROCEDURE RGBFromGray(g: Gray): RGB =
  BEGIN
    RETURN RGB{ r := g, g := g, b := g }
  END RGBFromGray;

PROCEDURE GrayMapFromRGBMap(map: RGBMap): GrayMap =
  VAR
    toGray := NEW(GrayMap, NUMBER(map^));
  BEGIN
    FOR i := 0 TO LAST(toGray^) DO
      toGray[i] := GrayFromRGB(map[i]);
    END;
    RETURN toGray;
  END GrayMapFromRGBMap;

PROCEDURE Lasso(contents: RawContents): Rect.T =
  VAR
    raw := contents.raw;
    bounds := Rect.Inset(raw.bounds, 1);
    seed := raw.get(Rect.NorthWest(bounds));
    res := Rect.T{ west := bounds.east,
                   east := bounds.west,
                   north := bounds.south,
                   south := bounds.north }; (* an improper rectangle, so far *)
    h: INTEGER;
  BEGIN
    FOR v := bounds.north TO bounds.south-1 DO
      h := bounds.west;
      WHILE h < bounds.east DO
        IF h < res.west OR v < res.north OR v >= res.south THEN
          IF raw.get(Point.T{h := h, v := v}) # seed THEN
            res.west := MIN(res.west, h);
            res.north := MIN(res.north, v);
            res.east := MAX(res.east, h+1);
            res.south := MAX(res.south, v+1);
            EXIT (* this row is now boring, except for .east *)
          END;
        ELSE
          EXIT
        END;
        INC(h);
      END;
      h := bounds.east;
      WHILE h > res.east DO
        DEC(h);
        IF raw.get(Point.T{h := h, v := v}) # seed THEN
          res.east := MAX(res.east, h+1);
        END;
      END;
    END;
    IF res.west >= res.east OR res.north >= res.south THEN
      RETURN Rect.Empty (* so that we don't return an improper rectangle *)
    ELSE
      RETURN res
    END;
  END Lasso;

PROCEDURE ToPNM(contents: Contents; wr: Wr.T)
                RAISES { Wr.Failure, Thread.Alerted } =
  VAR
    map := contents.map;
    toGray := GrayMapFromRGBMap(map);
    line := NEW(REF ARRAY OF INTEGER, contents.width);
    chars := NEW(REF ARRAY OF CHAR, contents.width*3);
  BEGIN
    Wr.PutChar(wr, 'P');
    IF contents.isBW THEN
      Wr.PutChar(wr, '4');
    ELSIF contents.isGray THEN
      Wr.PutChar(wr, '5');
    ELSE
      Wr.PutChar(wr, '6');
    END;
    Wr.PutText(wr, "\n" & Fmt.Int(contents.width) &
                   "\n" & Fmt.Int(contents.height) & "\n");
    IF NOT contents.isBW THEN Wr.PutText(wr, "255\n") END;
    FOR v := 0 TO contents.height-1 DO
      contents.getLine(v, line^);
      IF contents.isBW THEN
        <*ASSERT FALSE*> (* not yet implemented *)
      ELSIF contents.isGray THEN
        FOR h := 0 TO contents.width-1 DO
          chars[h] := VAL(toGray[line[h]], CHAR);
        END;
        Wr.PutString(wr, SUBARRAY(chars^, 0, contents.width));
      ELSE
        FOR h := 0 TO contents.width-1 DO
          VAR
            rgb := map[line[h]];
          BEGIN
            chars[h*3] := VAL(rgb.r, CHAR);
            chars[h*3+1] := VAL(rgb.g, CHAR);
            chars[h*3+2] := VAL(rgb.b, CHAR);
          END;
        END;
        Wr.PutString(wr, SUBARRAY(chars^, 0, 3*contents.width));
      END;
    END;
  END ToPNM;

REVEAL EmptyImage = T BRANDED OBJECT
  OVERRIDES
    domain := EmptyDomain;
    paint := EmptyPaint;
    render := EmptyRender;
    contents := EmptyContents;
  END;

PROCEDURE EmptyDomain(<*UNUSED*>i: T; <*UNUSED*>v: VBT.Leaf): Rect.T =
  BEGIN
    RETURN Rect.Empty
  END EmptyDomain;

PROCEDURE EmptyPaint(<*UNUSED*>i: T;
                     <*UNUSED*>v: VBT.Leaf;
                     <*UNUSED*>READONLY clip: Rect.T := Rect.Full;
                     <*UNUSED*>READONLY delta: Point.T;
                     <*UNUSED*>paintChunk := 0) =
  BEGIN
  END EmptyPaint;

PROCEDURE EmptyRender(<*UNUSED*>i: T; <*UNUSED*>v: VBT.Leaf): ScrnPixmap.Raw =
  BEGIN
    RETURN ScrnPixmap.NewRaw(1, Rect.Empty);
  END EmptyRender;

PROCEDURE EmptyContents(<*UNUSED*>i: T): Contents =
  BEGIN
    RETURN NEW(RawContents, width := 0,
                            height := 0,
                            map := NEW(RGBMap, 0),
                            isBW := TRUE,
                            isGray := TRUE,
                            isGrayRamp := FALSE,
                            raw := ScrnPixmap.NewRaw(1, Rect.Empty))
  END EmptyContents;

BEGIN
  Empty := NEW(EmptyImage);
END Images.
