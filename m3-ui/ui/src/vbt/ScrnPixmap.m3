(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Thu Aug  4 20:54:37 PDT 1994 by msm      *)
(*      modified on Mon Feb 24 13:58:12 PST 1992 by muller   *)
(*      modified on Tue Oct 22 21:29:39 PDT 1991 by gnelson  *)
(*      modified on Thu Apr 12 15:21:37 PDT 1990 by steveg   *)
<*PRAGMA LL*>

MODULE ScrnPixmap;

IMPORT Rect, Point, PaintPrivate, Word, Pixmap;

REVEAL 
  Private = BRANDED OBJECT END;
  Pixmap.Raw = Raw_Public BRANDED OBJECT END;
  T = Public BRANDED OBJECT END;

TYPE
  Raw1 = Raw OBJECT
    OVERRIDES
      get := Get1;
      set := Set1;
      sub := SubN
    END;
  Raw8 = Raw OBJECT
    OVERRIDES
      get := Get8;
      set := Set8;
      sub := SubN
    END;
  RawN = Raw OBJECT
    OVERRIDES
      get := GetN;
      set := SetN;
      sub := SubN
    END;

CONST WordSize = BITSIZE(PixWord);

PROCEDURE Get1(raw: Raw1; READONLY pt: Point.T): Pixel RAISES {} =
  BEGIN
    WITH 
      relH = pt.h - raw.westRounded,
      relV = pt.v - raw.bounds.north, 
      ix = raw.offset + relV * raw.wordsPerRow + relH DIV WordSize,
      word = raw.pixels[ix] 
    DO
      IF raw.pixelOrder = ByteOrder.LSBFirst THEN
        RETURN Word.Extract(word, relH MOD WordSize, 1)
      ELSE
        RETURN Word.Extract(word, (-1-relH) MOD WordSize, 1)
      END
    END
  END Get1;
  
PROCEDURE Set1(raw: Raw1; READONLY pt: Point.T; pix: Pixel) RAISES {} =
  BEGIN
    WITH 
      relH = pt.h - raw.westRounded,
      relV = pt.v - raw.bounds.north, 
      ix = raw.offset + relH DIV WordSize + relV * raw.wordsPerRow,
      word = raw.pixels[ix] 
    DO
      IF raw.pixelOrder = ByteOrder.LSBFirst THEN
        word := Word.Insert(word, pix, relH MOD WordSize, 1)
      ELSE
        word := Word.Insert(word, pix, (-1-relH) MOD WordSize, 1)
      END
    END
  END Set1;

PROCEDURE Get8(raw: Raw8; READONLY pt: Point.T): Pixel RAISES {} =
  CONST PixPerWord = WordSize DIV 8;
  BEGIN
    WITH 
      relH = pt.h - raw.westRounded,
      relV = pt.v - raw.bounds.north, 
      ix = raw.offset + relH DIV PixPerWord + relV * raw.wordsPerRow,
      word = raw.pixels[ix] 
    DO
      IF raw.pixelOrder = ByteOrder.LSBFirst THEN
        RETURN Word.Extract(word, 8*(relH MOD PixPerWord), 8)
      ELSE
        RETURN Word.Extract(word, 8*((-1-relH) MOD PixPerWord), 8)
      END
    END
  END Get8;
  
PROCEDURE Set8(raw: Raw8; READONLY pt: Point.T; pix: Pixel) RAISES {} =
  CONST PixPerWord = WordSize DIV 8;
  BEGIN
    WITH 
      relH = pt.h - raw.westRounded,
      relV = pt.v - raw.bounds.north, 
      ix = raw.offset + relH DIV PixPerWord + relV * raw.wordsPerRow,
      word = raw.pixels[ix] 
    DO
      IF raw.pixelOrder = ByteOrder.LSBFirst THEN
        word := Word.Insert(word, pix, 8*(relH MOD PixPerWord), 8)
      ELSE
        word := Word.Insert(word, pix, 8*((-1-relH) MOD PixPerWord), 8)
      END
    END
  END Set8;

PROCEDURE GetN(raw: Raw; READONLY pt: Point.T): Pixel RAISES {} =
  BEGIN
    WITH 
      bpp = raw.bitsPerPixel,
      pixPerWord = WordSize DIV bpp,
      relH = pt.h - raw.westRounded,
      relV = pt.v - raw.bounds.north, 
      ix = raw.offset + relH DIV pixPerWord + relV * raw.wordsPerRow,
      word = raw.pixels[ix] 
    DO
      IF raw.pixelOrder = ByteOrder.LSBFirst THEN
        RETURN Word.Extract(word, bpp * (relH MOD pixPerWord), bpp)
      ELSE
        RETURN Word.Extract(word, bpp*((-1-relH) MOD pixPerWord), bpp)
      END
    END
  END GetN;
  
PROCEDURE SetN(raw: Raw; READONLY pt: Point.T; pix: Pixel) RAISES {} =
  BEGIN
    WITH 
      bpp = raw.bitsPerPixel,
      pixPerWord = WordSize DIV bpp,
      relH = pt.h - raw.westRounded,
      relV = pt.v - raw.bounds.north, 
      ix = raw.offset + relH DIV pixPerWord + relV * raw.wordsPerRow,
      word = raw.pixels[ix] 
    DO
      IF raw.pixelOrder = ByteOrder.LSBFirst THEN
        word := Word.Insert(word, pix, bpp * (relH MOD pixPerWord), bpp)
      ELSE
        word := Word.Insert(word, pix, bpp*((-1-relH) MOD pixPerWord), bpp)
      END
    END
  END SetN;

PROCEDURE SubN(raw: Raw; READONLY rect: Rect.T): Raw RAISES {} =
  BEGIN
    WITH 
      bpp = raw.bitsPerPixel,
      pixPerWord = WordSize DIV bpp,
      dom = Rect.Meet(rect, raw.bounds),
      relH = dom.west - raw.westRounded,
      relV = dom.north - raw.bounds.north, 
      ix = raw.offset + relH DIV pixPerWord + relV * raw.wordsPerRow
    DO
      RETURN
        NEW(RawN, depth := raw.depth, bounds := dom,
          pixels := raw.pixels, bitsPerPixel := bpp,
          wordsPerRow := raw.wordsPerRow, pixelOrder := raw.pixelOrder,
          westRounded := dom.west - (dom.west MOD pixPerWord),
          offset := ix)
    END
  END SubN;

VAR defaultPixelOrder: ByteOrder := PaintPrivate.HostByteOrder;

PROCEDURE NewRaw (dpth: INTEGER; READONLY bnds: Rect.T): Raw RAISES {} =
  VAR res: Raw;
  BEGIN
    IF dpth = 1 THEN
      res := NEW(Raw1)
    ELSIF dpth = 8 THEN
      res := NEW(Raw8)
    ELSE
      res := NEW(RawN)
    END;
    WITH pixPerWord  = WordSize DIV dpth,
         westRounded = bnds.west - (bnds.west MOD pixPerWord),
         pixPerRow   = bnds.east - westRounded,
         wordsPerRow = (pixPerRow + pixPerWord - 1) DIV pixPerWord DO
      res.depth := dpth;
      res.bounds := bnds;
      res.pixels :=
        NEW(REF ARRAY OF PixWord, wordsPerRow * Rect.VerSize(bnds));
      res.offset := 0;
      IF pixPerWord = 1 THEN
        res.bitsPerPixel := WordSize
      ELSE
        res.bitsPerPixel := dpth
      END;
      res.wordsPerRow := wordsPerRow;
      res.pixelOrder := defaultPixelOrder;
      res.westRounded := westRounded
    END;
    RETURN res
  END NewRaw;

BEGIN
END ScrnPixmap.
