(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman *)
(* Last modified on Mon Oct 25 10:11:47 PDT 1993 by mhb    *)
(*      modified on Mon Jun 14 18:36:15 PDT 1993 by meehan *)
(*      modified on Mon Nov  2 13:09:34 PST 1992 by steveg *)
<*PRAGMA LL*>

MODULE Image;

IMPORT Axis, Color, Fmt, IntIntTbl, Math, Palette, Pixmap, Point,
       Rd, Rect, ScreenType, ScrnColorMap, ScrnPixmap, Thread,
       TrestleComm, VBT, Word, Wr;

IMPORT (* for FromVBT *) 
  Region, RigidVBT, ScaleFilter, Filter, Split, Trestle,
  VBTClass;

TYPE 
  PixelMap = ARRAY [0 .. 255] OF INTEGER;
  ColorsArray = ARRAY [0..255] OF RGB;
  Colors = REF ARRAY OF RGB;


TYPE Closure = Palette.PixmapClosure OBJECT raw: Raw END;

PROCEDURE Unscaled (raw: Raw): T =
  BEGIN
    RETURN
      Palette.FromPixmapClosure(
        NEW(Closure, apply := ApplyUnscaled, raw := raw))
  END Unscaled;

PROCEDURE ApplyUnscaled (cl: Closure; st: ScreenType.T):
  ScrnPixmap.T =
  BEGIN
    RETURN ScaleRaw(st, cl.raw, 1, 1)
  END ApplyUnscaled;



PROCEDURE Scaled (raw: Raw): T =
  BEGIN
    RETURN Palette.FromPixmapClosure(
             NEW(Closure, apply := ApplyScaled1, raw := raw))
  END Scaled;

PROCEDURE ApplyScaled1 (cl: Closure; st: ScreenType.T):
  ScrnPixmap.T =
  VAR
    xres := st.res[Axis.T.Hor] * 25.4;
    yres := st.res[Axis.T.Ver] * 25.4;
  BEGIN
    RETURN ScaleRaw(st, cl.raw, ROUND(xres / cl.raw.xres),
                    ROUND(yres / cl.raw.yres))
  END ApplyScaled1;


(* The following procedure allows you to provide a collection of
   pixmaps, each at a different resolution, and scales the most
   appropriate pixmap: *)

(* Returns a pixmap which will scale and display pixmap "raw[i]".
   "i" chosen so that:
      minimize  scale = MIN(maxScale, <screen resolution> / <resolution(raw[i])>)
   and
      ABS(error) < tolerance
   where
      error = 
        (<screen resolution> - scale * <resolution(raw[i])>) / <screen resolution>

   If no "i" satisfies the tolerance, then the "i" is chosen to give the
   smallest error.

   Intention of tolerance is to give some preference to scalings which give
      very good approximations to the screen resolution over smaller 
      scalings which give unacceptable errors.

      ex: screen resolution = 300
          pixmap resolutions of 200 and 150
          if tolerance < 1/3 then 150 will be chosen over 200

   Intention of maxScale is to give some preference to scalings with a
      smaller multiple even if they give a greater error.

      ex: screen resolution = 300
          pixmap resolutions of 50 and 200
          if maxScale <= 4 then 200 will be chosen over 50 
          (even if tolerance is < 1/3 )
 *)


PROCEDURE ScaledN (
    READONLY raws: ARRAY OF Raw;
    tolerance: REAL := 0.25;
    maxScale: CARDINAL := 4): T = 
  VAR new := NEW(REF ARRAY OF Raw, NUMBER(raws));
  BEGIN
    new^ := raws;
    RETURN Palette.FromPixmapClosure(
             NEW(ScaledNClosure, raws := new,
                 tolerance := MAX(tolerance, 0.00001),
                 maxScale := maxScale))
  END ScaledN;

TYPE
  ScaledNClosure = Palette.PixmapClosure OBJECT
                    raws     : REF ARRAY OF Raw;
                    tolerance: REAL;
                    maxScale : INTEGER;
                  OVERRIDES
                    apply := ApplyScaledN
                  END;

PROCEDURE ApplyScaledN (cl: ScaledNClosure; st: ScreenType.T):
  ScrnPixmap.T =
  VAR
    xres               := st.res[Axis.T.Hor] * 25.4;
    yres               := st.res[Axis.T.Ver] * 25.4;
    closest            := -1;
    scaleClosest       := LAST(INTEGER);
    tolerance          := cl.tolerance;
    xtol               := tolerance * xres;
    ytol               := tolerance * yres;
    errorClosest       := MAX(xres, yres);
  BEGIN
    (* find the raw closest to a multiple of the screen resolution *)
    FOR i := 0 TO LAST(cl.raws^) DO
      WITH xs     = MIN(ROUND(xres / cl.raws[i].xres), cl.maxScale),
           ys     = MIN(ROUND(yres / cl.raws[i].yres), cl.maxScale),
           xerror = ABS(xres - FLOAT(xs) * cl.raws[i].xres),
           yerror = ABS(yres - FLOAT(ys) * cl.raws[i].yres)          DO
        IF xerror <= xtol AND yerror <= ytol
             AND (MAX(xs, ys) < scaleClosest)
             OR (MAX(xs, ys) = scaleClosest
                   AND MAX(xerror, yerror) < errorClosest) THEN
          closest := i;
          scaleClosest := MAX(xs, ys);
          errorClosest := MAX(xerror, yerror);
        END;
      END;
    END;

    IF closest = -1 THEN
      (* try again, but go for min error no matter what *)
      errorClosest := MAX(xres, yres); (* can't do worse *)
      closest := 0; (* to be extra extra safe *)
      FOR i := 1 TO LAST(cl.raws^) DO
        WITH xs     = MIN(ROUND(xres / cl.raws[i].xres), cl.maxScale),
             ys     = MIN(ROUND(yres / cl.raws[i].yres), cl.maxScale),
             xerror = ABS(xres - FLOAT(xs) * cl.raws[i].xres),
             yerror = ABS(yres - FLOAT(ys) * cl.raws[i].yres)          DO
          IF MAX(xerror, yerror) <= errorClosest THEN
            closest := i;
            errorClosest := MAX(xerror, yerror);
          END;
        END;
      END;
    END;

    RETURN ScaleRaw(st, cl.raws[closest],
                    MAX(1, ROUND(xres / cl.raws[closest].xres)),
                    MAX(1, ROUND(yres / cl.raws[closest].yres)));
  END ApplyScaledN;

TYPE
  ScaleAction = {UsePixel, Cvt24BitToBW, UseTbl, CvtColorsToBW, UseMap};

PROCEDURE ScaleRaw (st        : ScreenType.T;
                    raw       : Raw;
                    xMul, yMul: INTEGER       ): ScrnPixmap.T =
  VAR
    bounds: Rect.T;
    cmap  : ScrnColorMap.T;
    new   : ScrnPixmap.Raw;
    a     : ScaleAction;
    map   : PixelMap;
    pix   : Pixel;
    tbl   : IntIntTbl.T;
    value : INTEGER;
    colors: Colors;
    gamma : BOOLEAN;
    mode  : Mode;
  BEGIN

    bounds :=
      Rect.T{north := 0, west := 0, south := yMul * raw.height,
             east := xMul * raw.width};
    TRY
      IF st.cmap = NIL THEN
        cmap := NIL
      ELSE
        cmap := st.cmap.standard()
      END;

      TYPECASE raw OF
      | RawBitmap =>
          new := ScrnPixmap.NewRaw(1, bounds);
          a := ScaleAction.UsePixel;

      | RawPixmapCMap (r) =>
          colors := r.colors;
          gamma := r.needsGamma;
          mode := r.colorMode;
          IF cmap = NIL THEN
            new := ScrnPixmap.NewRaw(1, bounds);
            a := ScaleAction.CvtColorsToBW
          ELSE
            new := ScrnPixmap.NewRaw(st.depth, bounds);
            IF NUMBER(r.colors^) <= 256 THEN
              a := ScaleAction.UseMap;
              FOR i := 0 TO LAST(colors^) DO
                map[i] := FromRGB(cmap, colors[i], gamma, mode)
              END
            ELSE
              a := ScaleAction.UseTbl;
              tbl := NEW (IntIntTbl.Default).init (LAST(colors^));
              FOR i := 0 TO LAST(colors^) DO
                EVAL tbl.put(i, FromRGB(cmap, colors[i], gamma, mode))
              END
            END
          END;

      | RawPixmap (r) =>
          gamma := r.needsGamma;
          mode := r.colorMode;
          IF cmap = NIL THEN
            new := ScrnPixmap.NewRaw(1, bounds);
            a := ScaleAction.Cvt24BitToBW
          ELSE
            new := ScrnPixmap.NewRaw(st.depth, bounds);
            a := ScaleAction.UseTbl;
            tbl := NEW (IntIntTbl.Default).init();
            FOR h := 0 TO raw.width - 1 DO
              FOR v := 0 TO raw.height - 1 DO
                pix := raw.get(h, v);
                IF NOT tbl.get(pix, value) THEN
                  EVAL tbl.put(
                         pix, FromRGB(cmap, RGBFrom24Bits(pix),
                                      gamma, mode))
                END
              END
            END
          END;

      | Raw =>                   <* ASSERT FALSE *>
      END;

      (* Now, set the pixels in "new": *)
      VAR
        bg            := st.bg;
        fg            := st.fg;
        dest: Point.T;
      BEGIN
        FOR i := 0 TO xMul - 1 DO
          FOR h := 0 TO raw.width - 1 DO
            FOR j := 0 TO yMul - 1 DO
              FOR v := 0 TO raw.height - 1 DO
                pix := raw.get(h, v);
                CASE a OF
                | ScaleAction.UsePixel =>
                | ScaleAction.UseTbl => EVAL tbl.get(pix, pix);
                | ScaleAction.CvtColorsToBW =>
                    (* should ungamma colors[pix] if necessary *)
                    pix := CvtRGBToBW(colors[pix], bg, fg);
                | ScaleAction.Cvt24BitToBW =>
                    (* should ungamma pix if necessary *)
                    pix := Cvt24BitToBW(pix, bg, fg);
                | ScaleAction.UseMap => pix := map[pix];
                END;
                dest := Point.T{xMul * h + i, yMul * v + j};
                new.set(dest, pix);
              END;
            END;
          END;
        END;
      END;

      RETURN st.pixmap.load(new)

    EXCEPT
    | TrestleComm.Failure =>
        RETURN Palette.ResolvePixmap(st, Pixmap.Solid)
    END

  END ScaleRaw;

PROCEDURE FromRGB (cmap : ScrnColorMap.T;
                   rgb  : RGB;
                   gamma: BOOLEAN;
                   mode : Mode            ): INTEGER
  RAISES {TrestleComm.Failure} =
  CONST Gamma = 2.4D0;
  BEGIN
    IF gamma THEN
      (* do nothing; Trestle gamma-corrects always *)
    ELSE
      (* un-gamma correct *)
      rgb.r := FLOAT(Math.pow(FLOAT(rgb.r, LONGREAL), Gamma));
      rgb.g := FLOAT(Math.pow(FLOAT(rgb.g, LONGREAL), Gamma));
      rgb.b := FLOAT(Math.pow(FLOAT(rgb.b, LONGREAL), Gamma));
    END;
    TRY
      RETURN cmap.fromRGB(rgb, mode)
    EXCEPT
      ScrnColorMap.Failure => RETURN 0
    END
  END FromRGB;

PROCEDURE CvtRGBToBW (rgb: RGB; bg,fg: INTEGER): INTEGER =
  BEGIN
    IF Color.Brightness(rgb) > 0.5 THEN
      RETURN bg
    ELSE
      RETURN fg
    END
  END CvtRGBToBW;

PROCEDURE Cvt24BitToBW (pix: INTEGER; bg, fg: INTEGER): INTEGER =
  BEGIN
    RETURN CvtRGBToBW(RGBFrom24Bits(pix), bg, fg)
  END Cvt24BitToBW;

PROCEDURE RGBFrom24Bits (pix: INTEGER): RGB =
  VAR r, g, b: INTEGER;
  BEGIN
    From24Bits(pix, r, g, b);
    RETURN
      RGB{FLOAT(r) / 255.0, FLOAT(g) / 255.0, FLOAT(b) / 255.0}
  END RGBFrom24Bits;


(***************************************************************************
**
**  Formats of Raw -- for now, all use a ScrnPixmap.Raw to represent
**    the bits. As such, the get and set methods are trivial.
**
***************************************************************************)

TYPE
  ImBitmap =
    RawBitmap OBJECT
      bits: ScrnPixmap.Raw;      (* depth = 1 *)
    METHODS
      init (width, height: INTEGER): ImBitmap := InitBitmap;
    OVERRIDES
      get := GetBitmap;
      set := SetBitmap;
    END;

PROCEDURE InitBitmap (raw: ImBitmap; width, height: INTEGER):
  ImBitmap =
  BEGIN
    raw.width := width;
    raw.height := height;
    raw.bits :=
      ScrnPixmap.NewRaw(1, Rect.FromSize(width, height));
    RETURN raw
  END InitBitmap;

PROCEDURE GetBitmap (raw: ImBitmap; h, v: INTEGER): Pixel =
  BEGIN
    RETURN raw.bits.get(Point.T{h, v})
  END GetBitmap;

PROCEDURE SetBitmap (raw: ImBitmap; h, v: INTEGER; p: Pixel) =
  BEGIN
    raw.bits.set(Point.T{h, v}, p)
  END SetBitmap;


TYPE ImPixmap = RawPixmap OBJECT
     red, green, blue: ScrnPixmap.Raw; (* depth = 8 *)
   METHODS
      init (width, height: INTEGER): ImPixmap := InitPixmap;
   OVERRIDES
     get := GetPixmap;
     set := SetPixmap;
   END;

PROCEDURE InitPixmap (raw: ImPixmap; width, height: INTEGER):
  ImPixmap =
  BEGIN
    raw.width := width;
    raw.height := height;
    raw.red :=
      ScrnPixmap.NewRaw(8, Rect.FromSize(width, height));
    raw.green :=
      ScrnPixmap.NewRaw(8, Rect.FromSize(width, height));
    raw.blue :=
      ScrnPixmap.NewRaw(8, Rect.FromSize(width, height));
    RETURN raw
  END InitPixmap;

PROCEDURE GetPixmap (raw: ImPixmap; h, v: INTEGER): Pixel =
  VAR p := Point.T{h, v};
  BEGIN
    RETURN To24Bits(raw.red.get(p), raw.green.get(p),
                    raw.blue.get(p), 255)
  END GetPixmap;

PROCEDURE SetPixmap (raw: ImPixmap; h, v: INTEGER; pix: Pixel) =
  VAR r, g, b: INTEGER; p := Point.T{h, v};
  BEGIN
    From24Bits(pix, r, g, b);
    raw.red.set(p, r);
    raw.green.set(p, g);
    raw.blue.set(p, b);
  END SetPixmap;


TYPE
  ImGraymap =
    RawPixmap OBJECT
      bits: ScrnPixmap.Raw;      (* depth = 8 *)
    METHODS
      init (width, height: INTEGER): ImGraymap := InitGray;
    OVERRIDES
      get := GetGray;
      set := SetGray;
    END;

PROCEDURE InitGray (raw: ImGraymap; width, height: INTEGER):
  ImGraymap =
  BEGIN
    raw.width := width;
    raw.height := height;
    raw.bits :=
      ScrnPixmap.NewRaw(8, Rect.FromSize(width, height));
    RETURN raw
  END InitGray;

PROCEDURE GetGray (raw: ImGraymap; h, v: INTEGER): Pixel =
  VAR pix := raw.bits.get(Point.T{h, v});
  BEGIN
    RETURN pix * 256 * 256 + pix * 256 + pix
  END GetGray;

PROCEDURE SetGray (raw: ImGraymap; h, v: INTEGER; p: Pixel) =
  VAR rgb := RGBFrom24Bits(p);
  BEGIN
    raw.bits.set(Point.T{h, v}, ROUND(Color.Brightness(rgb) * 255.0))
  END SetGray;


TYPE
  ImPixmapCMap = RawPixmapCMap OBJECT
                   bits: ScrnPixmap.Raw;
                 METHODS
                   init (width, height, depth: INTEGER):
                         ImPixmapCMap := InitPixmapCMap;
                 OVERRIDES
                   get := GetPixmapCMap;
                   set := SetPixmapCMap;
                 END;

PROCEDURE InitPixmapCMap (raw                 : ImPixmapCMap;
                          width, height, depth: INTEGER       ):
  ImPixmapCMap =
  BEGIN
    raw.width := width;
    raw.height := height;
    raw.bits :=
      ScrnPixmap.NewRaw(depth, Rect.FromSize(width, height));
    RETURN raw
  END InitPixmapCMap;


PROCEDURE GetPixmapCMap (raw: ImPixmapCMap; h, v: INTEGER): Pixel =
  BEGIN
    RETURN raw.bits.get(Point.T{h, v})
  END GetPixmapCMap;

PROCEDURE SetPixmapCMap (raw : ImPixmapCMap;
                         h, v: INTEGER;
                         p   : Pixel         ) =
  BEGIN
    raw.bits.set(Point.T{h, v}, p)
  END SetPixmapCMap;


(***************************************************************************
**
**  FromVBT
**
***************************************************************************)

PROCEDURE FromVBT (v: VBT.T; width, height: REAL): Raw 
  RAISES { TrestleComm.Failure } =
  VAR
    trsl          := Trestle.ScreenOf(v, Point.Origin).trsl;
    st            := VBT.ScreenTypeOf(v);
    parent        := VBT.Parent(v);
    pred  : VBT.T;
    pm : ScrnPixmap.T;
    br : Region.T;
  <* FATAL Split.NotAChild *>
  BEGIN
    IF trsl = NIL OR st = NIL THEN RAISE TrestleComm.Failure END;
    TYPECASE parent OF
    | NULL =>
    | Filter.T (f) => EVAL Filter.Replace(f, NIL);
    | Split.T (s) =>
        pred := Split.Pred(s, v);
        Split.Delete(s, v);
    END;

    TRY
      WITH filter = NEW(Filter.T).init(v),
           scale  = NEW(ScaleFilter.T).init(filter),
           srH    = RigidVBT.SizeRange{width, width, width},
           srV    = RigidVBT.SizeRange{height, height, height},
           off = NEW(RigidVBT.T).init(
                   scale, RigidVBT.Shape{srH, srV}) DO
        ScaleFilter.AutoScale(scale);
        Trestle.Attach(off, trsl);
        Trestle.InstallOffscreen(
          off, ROUND(VBT.MMToPixels(parent, width, Axis.T.Hor)),
          ROUND(VBT.MMToPixels(parent, height, Axis.T.Ver)), st);
        pm := VBT.Capture(v, VBT.Domain(v), br);
        EVAL Filter.Replace(filter, NIL);
        Trestle.Delete(off);
        VBT.Discard(off);
      END;

      TYPECASE parent OF
      | NULL =>
      | Filter.T (f) => EVAL Filter.Replace(f, v);
      | Split.T (s) => Split.Insert(s, pred, v);
      END;

      RETURN FromScrnPixmap(pm, st)
    FINALLY
      IF pm # NIL THEN pm.free() END;
    END
  END FromVBT;


(***************************************************************************
**
**  FromScrnPixmap
**
***************************************************************************)

EXCEPTION Unimplemented;
<* FATAL Unimplemented *>

PROCEDURE FromScrnPixmap (pm: ScrnPixmap.T; st: VBT.ScreenType):
  Raw RAISES {TrestleComm.Failure} =
  VAR
    cmap     : ScrnColorMap.T;
    spm                       := pm.localize(pm.bounds);
    width                     := Rect.HorSize(spm.bounds);
    height                    := Rect.VerSize(spm.bounds);
  BEGIN
    IF st.cmap = NIL THEN
      cmap := NIL
    ELSE
      cmap := st.cmap.standard()
    END;
    IF spm.depth = 1 THEN
      VAR raw := NEW(ImBitmap).init(width, height);
      BEGIN
        raw.xres := st.res[Axis.T.Hor];
        raw.yres := st.res[Axis.T.Ver];
        FOR h := 0 TO raw.width - 1 DO
          FOR v := 0 TO raw.height - 1 DO
            WITH pix = spm.get(Point.T{h + spm.bounds.west,
                                       v + spm.bounds.north}) DO
              (* more efficient than calling raw.put(h, v, pix) *)
              raw.bits.set(Point.T{h, v}, pix)
            END
          END
        END;
        RETURN raw
      END
    ELSIF spm.depth <= 8 THEN
      <* ASSERT cmap # NIL *>
      VAR
        raw := NEW(ImPixmapCMap).init(width, height, spm.depth);
        map                 := PixelMap{-1, ..};
        cntPix              := 0;
        colors: ColorsArray;
        entry: ARRAY [0 .. 0] OF ScrnColorMap.Entry;
      BEGIN
        raw.xres := st.res[Axis.T.Hor];
        raw.yres := st.res[Axis.T.Ver];
        FOR h := 0 TO raw.width - 1 DO
          FOR v := 0 TO raw.height - 1 DO
            WITH pix = spm.get(Point.T{h + spm.bounds.west,
                                       v + spm.bounds.north}) DO
              IF map[pix] = -1 THEN
                map[pix] := cntPix;
                entry[0].pix := pix;
                cmap.read(entry);
                colors[cntPix] := entry[0].rgb;
                INC(cntPix)
              END;
              (* more efficient than calling raw.put(h, v, map[pix]) *)
              raw.bits.set(Point.T{h, v}, map[pix])
            END
          END
        END;
        raw.colors := NEW(Colors, cntPix);
        FOR i := 0 TO cntPix - 1 DO
          raw.colors[i] := colors[i]
        END;
        RETURN raw
      END
    ELSIF spm.depth = 24 THEN
      VAR raw := NEW(ImPixmap).init(width, height);
      BEGIN
         FOR h := spm.bounds.west TO spm.bounds.east - 1 DO
           FOR v := spm.bounds.north TO spm.bounds.south - 1 DO
            raw.set(h, v, spm.get(Point.T{h, v}));
          END
        END;
        RETURN raw
      END
    ELSE
      RAISE Unimplemented
    END;
  END FromScrnPixmap;


(***************************************************************************
**
**  ToWr
**
***************************************************************************)

PROCEDURE ToWr (raw: Raw; wr: Wr.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    TYPECASE (raw) OF
    | RawBitmap (r) => BitmapToWr(r, wr);
    | RawPixmapCMap (r) => PixmapCMapToWr(r, wr);
    | RawPixmap (r) => PixmapToWr(r, wr);
    ELSE <* ASSERT FALSE *>
    END;
  END ToWr;

PROCEDURE BitmapToWr (bm: RawBitmap; wr: Wr.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  BEGIN
    Wr.PutText(wr, "P1\n");
    Wr.PutText(wr, Fmt.Int(bm.width));
    Wr.PutText(wr, " ");
    Wr.PutText(wr, Fmt.Int(bm.height));
    Wr.PutText(wr, "\n");
    FOR v := 0 TO bm.height - 1 DO
      FOR h := 0 TO bm.width - 1 DO
        Wr.PutText(wr, Fmt.Int(bm.get(h, v)) & " ")
      END;
      Wr.PutText(wr, "\n")
    END
  END BitmapToWr;

PROCEDURE PixmapToWr (pm: RawPixmap; wr: Wr.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR pix, r, g, b: INTEGER;
  BEGIN
    Wr.PutText(wr, "P3\n");
    Wr.PutText(wr, Fmt.Int(pm.width));
    Wr.PutText(wr, " ");
    Wr.PutText(wr, Fmt.Int(pm.height));
    Wr.PutText(wr, "\n");
    Wr.PutText(wr, "256\n");
    FOR v := 0 TO pm.height - 1 DO
      FOR h := 0 TO pm.width - 1 DO
        pix := pm.get(h, v);
        From24Bits(pix, r, g, b);
        Wr.PutText(wr, Fmt.Int(r) & " " & Fmt.Int(g) & " "
                         & Fmt.Int(b) & "\n");
      END
    END
  END PixmapToWr;

PROCEDURE PixmapCMapToWr (pm: RawPixmapCMap; wr: Wr.T)
  RAISES {Thread.Alerted, Wr.Failure} =
  VAR rgb: RGB;
  BEGIN
    Wr.PutText(wr, "P3\n");
    Wr.PutText(wr, Fmt.Int(pm.width));
    Wr.PutText(wr, " ");
    Wr.PutText(wr, Fmt.Int(pm.height));
    Wr.PutText(wr, "\n");
    Wr.PutText(wr, "256\n");
    FOR v := 0 TO pm.height - 1 DO
      FOR h := 0 TO pm.width - 1 DO
        rgb := pm.colors[pm.get(h, v)];
        Wr.PutText(wr, Fmt.Int(ROUND(rgb.r * 255.0)) & " "
                         & Fmt.Int(ROUND(rgb.g * 255.0)) & " "
                         & Fmt.Int(ROUND(rgb.b * 255.0)) & "\n");
      END
    END
  END PixmapCMapToWr;


(***************************************************************************
**
**  FromRd
**
***************************************************************************)

PROCEDURE FromRd (rd: Rd.T): Raw
  RAISES {Thread.Alerted, Rd.Failure, Error} =
  VAR ch: CHAR;
  BEGIN
    TRY
      ch := Rd.GetChar(rd);
      IF ch # 'P' THEN RAISE Error END;
      ch := Rd.GetChar(rd);
      CASE ch OF
      | '1' => RETURN pbm(rd)
      | '2' => RETURN pgm(rd)
      | '3' => RETURN ppm(rd)
      | '4' => RETURN pbm2(rd)
      | '5' => RETURN pgm2(rd)
      | '6' => RETURN ppm2(rd)
      ELSE
        RAISE Error
      END;
    EXCEPT
      Rd.EndOfFile => RAISE Error
    END
  END FromRd;

PROCEDURE pbm (rd: Rd.T): Raw
  RAISES {Thread.Alerted, Rd.Failure, Error} =
  VAR
    width  := ScanInt(rd);
    height := ScanInt(rd);
    raw := NEW(ImBitmap).init(width, height);
  BEGIN
    FOR v := 0 TO height - 1 DO
      FOR h := 0 TO width - 1 DO
        WITH b = ScanInt(rd) DO
          raw.bits.set(Point.T{h, v}, b)
        END
      END
    END;
    RETURN raw
  END pbm;

PROCEDURE pbm2 (rd: Rd.T): Raw RAISES {Thread.Alerted, Rd.Failure, Error} =
  VAR
    width          := ScanInt(rd);
    height         := ScanInt(rd);
    raw            := NEW(ImBitmap).init(width, height);
    word  : Word.T;
  BEGIN
    TRY
      WHILE Rd.GetChar(rd) # '\n' DO END;
    EXCEPT
      Rd.EndOfFile => RAISE Error
    END;
    FOR v := 0 TO height - 1 DO
      FOR h := 0 TO width - 1 DO
        IF h MOD 8 = 0 THEN word := ScanByte(rd) END;
        raw.bits.set(
          Point.T{h, v}, ORD(128 = Word.And(word, 128)));
        word := Word.LeftShift(word, 1)
      END
    END;
    RETURN raw
  END pbm2;

PROCEDURE pgm (rd: Rd.T): Raw
  RAISES {Thread.Alerted, Rd.Failure, Error} =
  VAR
    width  := ScanInt(rd);
    height := ScanInt(rd);
    maxval := ScanInt(rd);
    raw    := NEW(ImGraymap).init(width, height);
  BEGIN
    FOR v := 0 TO height - 1 DO
      FOR h := 0 TO width - 1 DO
        WITH gray = ScanInt(rd) DO
          raw.bits.set(Point.T{h, v}, To8Bits(gray, maxval))
        END
      END
    END;
    RETURN raw
  END pgm;

PROCEDURE pgm2 (rd: Rd.T): Raw
  RAISES {Thread.Alerted, Rd.Failure, Error} =
  VAR
    width  := ScanInt(rd);
    height := ScanInt(rd);
    maxval := ScanInt(rd);
    raw    := NEW(ImGraymap).init(width, height);
  BEGIN
    TRY
      WHILE Rd.GetChar(rd) # '\n' DO END;
    EXCEPT
      Rd.EndOfFile => RAISE Error
    END;
    FOR v := 0 TO height - 1 DO
      FOR h := 0 TO width - 1 DO
        WITH gray = ScanByte(rd) DO
          raw.bits.set(Point.T{h,v}, To8Bits(gray, maxval))
        END
      END
    END;
    RETURN raw
  END pgm2;

PROCEDURE ppm (rd: Rd.T): Raw
  RAISES {Thread.Alerted, Rd.Failure, Error} =
  VAR
    width            := ScanInt(rd);
    height           := ScanInt(rd);
    maxval           := ScanInt(rd);
    raw              := NEW(ImPixmap).init(width, height);
    p      : Point.T;
    r, g, b: INTEGER;
  BEGIN
    FOR v := 0 TO height - 1 DO
      FOR h := 0 TO width - 1 DO
        r := ScanInt(rd);
        g := ScanInt(rd);
        b := ScanInt(rd);
        IF maxval # 255 THEN
          r := To8Bits(r, maxval);
          g := To8Bits(g, maxval);
          b := To8Bits(b, maxval);
        END;
        p := Point.T{h, v};
        raw.red.set(p, r);
        raw.green.set(p, g);
        raw.blue.set(p, b);
      END
    END;
    RETURN raw
  END ppm;

PROCEDURE ppm2 (rd: Rd.T): Raw
  RAISES {Thread.Alerted, Rd.Failure, Error} =
  VAR
    width            := ScanInt(rd);
    height           := ScanInt(rd);
    maxval           := ScanInt(rd);
    raw              := NEW(ImPixmap).init(width, height);
    p      : Point.T;
    r, g, b: INTEGER;
  BEGIN
    TRY
      WHILE Rd.GetChar(rd) # '\n' DO END;
    EXCEPT
      Rd.EndOfFile => RAISE Error
    END;
    FOR v := 0 TO height - 1 DO
      FOR h := 0 TO width - 1 DO
        r := ScanByte(rd);
        g := ScanByte(rd);
        b := ScanByte(rd);
        IF maxval # 255 THEN
          r := To8Bits(r, maxval);
          g := To8Bits(g, maxval);
          b := To8Bits(b, maxval);
        END;
        p := Point.T{h, v};
        raw.red.set(p, r);
        raw.green.set(p, g);
        raw.blue.set(p, b);
      END
    END;
    RETURN raw
  END ppm2;

PROCEDURE To8Bits (v, maxval: INTEGER): INTEGER =
  BEGIN
    IF maxval = 255 THEN
       RETURN v
    ELSE
      RETURN ROUND(FLOAT(v) * 255.0 / FLOAT(maxval))
    END;
  END To8Bits;

PROCEDURE To24Bits (r, g, b, maxval: INTEGER): INTEGER =
  BEGIN
    IF maxval # 255 THEN
      WITH m = FLOAT(maxval) DO
        r := ROUND(FLOAT(r) * 255.0 / m);
        g := ROUND(FLOAT(g) * 255.0 / m);
        b := ROUND(FLOAT(b) * 255.0 / m);
      END
    END;
    RETURN r * 256 * 256 + g * 256 + b
  END To24Bits;

PROCEDURE From24Bits (pix: Pixel; VAR (* OUT *) r, g, b: INTEGER) =
  BEGIN
    b := pix MOD 256;
    pix := pix DIV 256;
    g := pix MOD 256;
    pix := pix DIV 256;
    r := pix MOD 256
  END From24Bits;

(* ScanInt -- return value of next integer in rd; skip any
   whitespace before the first digit. *)
PROCEDURE ScanInt (rd: Rd.T): INTEGER
  RAISES {Thread.Alerted, Rd.Failure, Error} =
  CONST Digits = SET OF CHAR{'0'.. '9'};
  VAR
    res: INTEGER;
    ch : CHAR;
  BEGIN
    TRY ch := GetChar(rd) EXCEPT Rd.EndOfFile => RAISE Error END;
    IF ch IN Digits THEN
      res := ORD(ch) - ORD('0')
    ELSE
      RAISE Error
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

(* ScanByte -- return value of next byte from rd *)
PROCEDURE ScanByte (rd: Rd.T): INTEGER
  RAISES {Thread.Alerted, Rd.Failure, Error} =
  VAR ch: CHAR;
  BEGIN
    TRY
      ch := Rd.GetChar(rd)
    EXCEPT
      Rd.EndOfFile => RAISE Error
    END;
    RETURN ORD(ch)
  END ScanByte;

(* GetChar -- return next non-whitespace character *)
PROCEDURE GetChar (rd: Rd.T): CHAR
  RAISES {Thread.Alerted, Rd.Failure, Rd.EndOfFile} =
  CONST
    Spaces  = SET OF CHAR{' ', '\t', '\n', '\r'};
    Comment = '#';
  VAR ch := Rd.GetChar(rd);
  BEGIN
    WHILE (ch = Comment) OR (ch IN Spaces) DO
      IF ch = Comment THEN EVAL Rd.GetLine(rd) END;
      ch := Rd.GetChar(rd)
    END;
    RETURN ch
  END GetChar;

BEGIN
END Image.
