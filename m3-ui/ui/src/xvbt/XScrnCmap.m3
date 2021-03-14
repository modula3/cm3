(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Tue Nov 21 15:04:02 PST 1995 by msm     *)
(*      modified on Mon Nov 22 13:48:59 PST 1993 by steveg  *)
(*      modified on Fri May  7 17:43:58 PDT 1993 by mjordan *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE MODULE XScrnCmap;

IMPORT ScrnColorMap, TrestleComm, Word, X, XClient, XScreenType,
       XScrnTpRep, TrestleOnX, Math, Ctypes;

TYPE
  ColorMapOracle =
    ScrnColorMap.Oracle OBJECT
      st       : XScreenType.T;
      defaultCM: XColorMap;
    METHODS
      <* LL.sup = SELF.st.trsl *>
      init (st: XScreenType.T; READONLY vinfo: X.XVisualInfo):
            ColorMapOracle RAISES {TrestleComm.Failure} := InitColorMapOracle;
    OVERRIDES
      standard := ColorMapDefault;
      new      := ColorMapNew;
      list     := ColorMapList;
      lookup   := ColorMapLookup
    END;

PROCEDURE NewOracle (scrn: XScreenType.T; READONLY vinfo: X.XVisualInfo):
  ScrnColorMap.Oracle RAISES {TrestleComm.Failure} =
  BEGIN
    RETURN NEW(ColorMapOracle).init(scrn, vinfo)
  END NewOracle;

TYPE
  Prim = ScrnColorMap.Primary;
  XColorMap = ScrnColorMap.T OBJECT
                st    : XScreenType.T;
                direct: BOOLEAN;
                xid   : X.Colormap;
              OVERRIDES
                fromRGB := ColorMapFromRGB;
                new     := ColorMapCube;
                read    := ColorMapRead;
                write   := ColorMapWrite;
                free    := ColorMapFreeCube;
              END;
(* For all v: VBT.T, all cm: XColorMap, cm < v *)

PROCEDURE ColorMapID (cm: ScrnColorMap.T): X.Colormap =
  BEGIN
    TYPECASE cm OF
      NULL => RETURN X.None
    | XColorMap (xcm) => RETURN xcm.xid
    ELSE
      RETURN X.None
    END
  END ColorMapID;

PROCEDURE ColorMapFromRGB (cm  : XColorMap;
                           rgb : ScrnColorMap.RGB;
                           mode: ScrnColorMap.Mode ): ScrnColorMap.Pixel
  RAISES {ScrnColorMap.Failure, TrestleComm.Failure} =
  VAR
    xcol: X.XColor;
    ent : ScrnColorMap.Entry;
    trsl                     := cm.st.trsl;
  BEGIN
    TRY
    IF cm.direct AND mode = ScrnColorMap.Mode.Accurate THEN
      mode := ScrnColorMap.Mode.Normal
    END;
    IF mode # ScrnColorMap.Mode.Accurate THEN
      rgb.r := FLOAT(ROUND(rgb.r * FLOAT(cm.ramp.last[Prim.Red]))) / FLOAT(
                 cm.ramp.last[Prim.Red]);
      rgb.g := FLOAT(ROUND(rgb.g * FLOAT(cm.ramp.last[Prim.Green])))
                 / FLOAT(cm.ramp.last[Prim.Green]);
      rgb.b := FLOAT(ROUND(rgb.b * FLOAT(cm.ramp.last[Prim.Blue])))
                 / FLOAT(cm.ramp.last[Prim.Blue]);
    END;
    ent.rgb := rgb;
    XColorFromEntry(xcol, ent);
    TrestleOnX.Enter(trsl);
    TRY
      IF mode # ScrnColorMap.Mode.Accurate AND cm.ramp.base # -1 THEN
        ent.pix := cm.ramp.base;
        INC(ent.pix, cm.ramp.mult[Prim.Red] * ROUND(
                       rgb.r * FLOAT(cm.ramp.last[Prim.Red])));
        INC(ent.pix, cm.ramp.mult[Prim.Green] * ROUND(
                       rgb.g * FLOAT(cm.ramp.last[Prim.Green])));
        INC(ent.pix, cm.ramp.mult[Prim.Blue] * ROUND(
                       rgb.b * FLOAT(cm.ramp.last[Prim.Blue])));
      ELSE
        IF X.XAllocColor(trsl.dpy, cm.xid, ADR(xcol)) = 0 THEN
          RAISE ScrnColorMap.Failure
        END;
        ent.pix := xcol.pixel
      END
    FINALLY
      TrestleOnX.Exit(trsl)
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
    RETURN ent.pix
  END ColorMapFromRGB;

PROCEDURE ColorMapRead (cm: XColorMap; VAR res: ARRAY OF ScrnColorMap.Entry)
  RAISES {TrestleComm.Failure} =
  VAR xres := NEW(UNTRACED REF ARRAY OF X.XColor, NUMBER(res));
  BEGIN
    TRY
    TRY
      IF NUMBER(res) = 0 THEN RETURN END;
      FOR i := 0 TO LAST(res) DO
        xres[i].pixel := res[i].pix;
        xres[i].flags := X.DoRed + X.DoGreen + X.DoBlue
      END;
      X.XLockDisplay(cm.st.trsl.dpy);
        X.XQueryColors(cm.st.trsl.dpy, cm.xid, ADR(xres[0]), NUMBER(res));
        FOR i := 0 TO LAST(res) DO
          res[i].xrgb.red := xres[i].red;
          res[i].xrgb.blue := xres[i].blue;
          res[i].xrgb.green := xres[i].green;
          res[i].xrgb.alpha := 65535;
          EntryFromXColor(res[i], xres[i])
        END;
      X.XUnlockDisplay(cm.st.trsl.dpy);
    FINALLY
      DISPOSE(xres)
    END
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END ColorMapRead;

PROCEDURE ColorMapWrite (         cm : XColorMap;
                         READONLY new: ARRAY OF ScrnColorMap.Entry)
  RAISES {ScrnColorMap.Failure, TrestleComm.Failure} =
  BEGIN
    IF cm.readOnly THEN RAISE ScrnColorMap.Failure END;
    InnerColorMapWrite(cm, new)
  END ColorMapWrite;

PROCEDURE ColorMapCube (cm: XColorMap; d: CARDINAL): ScrnColorMap.Cube
  RAISES {ScrnColorMap.Failure, TrestleComm.Failure} =
  VAR
    res : ScrnColorMap.Cube;
    pm  : UNTRACED REF ARRAY OF INTEGER;
    trsl                                := cm.st.trsl;
    dpy                                 := trsl.dpy;
  BEGIN
    TRY
    IF cm.readOnly THEN RAISE ScrnColorMap.Failure END;
    pm := NEW(UNTRACED REF ARRAY OF INTEGER, MAX(d, 1));
    TRY
      TrestleOnX.Enter(trsl);
      TRY
        IF X.XAllocColorCells(
             dpy, cm.xid, X.False, ADR(pm[0]), d, ADR(res.lo), 1) = 0 THEN
          RAISE ScrnColorMap.Failure
        END;
        res.hi := res.lo;
        FOR i := 0 TO d - 1 DO INC(res.hi, pm[i]) END
      FINALLY
        TrestleOnX.Exit(trsl)
      END
    FINALLY
      DISPOSE(pm)
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
    RETURN res
  END ColorMapCube;

PROCEDURE ColorMapFreeCube (cm: XColorMap; READONLY cb: ScrnColorMap.Cube)
  RAISES {TrestleComm.Failure} =
  VAR
    pm  : UNTRACED REF ARRAY OF INTEGER;
    trsl                                := cm.st.trsl;
    dpy                                 := trsl.dpy;
  BEGIN
    TRY
    pm := NEW(UNTRACED REF ARRAY OF INTEGER, cm.depth);
    TRY
      TrestleOnX.Enter(trsl);
      TRY
        X.XFreeColors(dpy, cm.xid, ADR(cb.lo), 1, cb.hi - cb.lo)
      FINALLY
        TrestleOnX.Exit(trsl)
      END
    FINALLY
      DISPOSE(pm)
    END
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END ColorMapFreeCube;

PROCEDURE InnerColorMapWrite (         cm : XColorMap;
                              READONLY new: ARRAY OF ScrnColorMap.Entry)
  RAISES {ScrnColorMap.Failure, TrestleComm.Failure} =
  VAR
    trsl                                    := cm.st.trsl;
    dpy                                     := trsl.dpy;
    xcolor : X.XColor;
    xcolors: UNTRACED REF ARRAY OF X.XColor;
  BEGIN
    TRY
    TrestleOnX.Enter(trsl);
    TRY
      IF NUMBER(new) = 1 THEN
        XColorFromEntry(xcolor, new[0]);
        X.XStoreColor(dpy, cm.xid, ADR(xcolor))
      ELSE
        xcolors := NEW(UNTRACED REF ARRAY OF X.XColor, NUMBER(new));
        TRY
          FOR i := 0 TO LAST(new) DO
            XColorFromEntry(xcolors[i], new[i])
          END;
          X.XStoreColors(dpy, cm.xid, ADR(xcolors[0]), NUMBER(new))
        FINALLY
          DISPOSE(xcolors)
        END
      END
    FINALLY
      TrestleOnX.Exit(trsl)
    END
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END InnerColorMapWrite;

<* UNUSED *> PROCEDURE Sqrt (x: REAL): REAL =
  CONST epsilon = 0.25 / FLOAT(Word.Shift(1, 30));
  VAR
    r    : REAL;
    scale       := 0;
  BEGIN
    IF x <= epsilon THEN RETURN 0.0 END;
    WHILE x < 0.25 DO INC(scale); x := 4.0 * x END;
    r := (x + 1.0) / 2.0;
    FOR i := 1 TO 5 DO r := (r + x / r) / 2.0 END;
    IF scale # 0 THEN r := r / FLOAT(Word.Shift(1, scale)) END;
    RETURN r
  END Sqrt;

<* UNUSED *> PROCEDURE Cbrt (x: REAL): REAL =
  CONST epsilon = 1.0 / FLOAT(Word.Shift(1, 24));
  VAR
    r    : REAL;
    scale       := 0;
  BEGIN
    IF x <= epsilon THEN RETURN 0.0 END;
    WHILE x < 0.125 DO INC(scale); x := 8.0 * x END;
    r := (x + 2.0) / 3.0;
    FOR i := 1 TO 5 DO r := (2.0 * r + x / (r * r)) / 3.0 END;
    IF scale # 0 THEN r := r / FLOAT(Word.Shift(1, scale)) END;
    RETURN r
  END Cbrt;

CONST
  Gamma        = 2.4D0;
  GammaInverse = 1.0D0 / Gamma;

PROCEDURE XColorFromEntry (VAR      xcolor: X.XColor;
                           READONLY ent   : ScrnColorMap.Entry)
  RAISES {ScrnColorMap.Failure} =
  CONST
    Scale = FLOAT(LAST(Card16), LONGREAL);
    DoAll = X.DoRed + X.DoGreen + X.DoBlue;
  BEGIN
    IF ent.rgb.r < 0.0 OR ent.rgb.r > 1.0 OR ent.rgb.g < 0.0
         OR ent.rgb.g > 1.0 OR ent.rgb.b < 0.0 OR ent.rgb.b > 1.0 THEN
      RAISE ScrnColorMap.Failure
    END;
    xcolor.pixel := ent.pix;
    (*
    VAR rr := Cbrt(ent.rgb.r); gg := Cbrt(ent.rgb.g);
      bb := Cbrt(ent.rgb.b); BEGIN
      xcolor.red := ROUND(Scale * rr * rr);
      xcolor.green := ROUND(Scale * gg * gg);
      xcolor.blue := ROUND(Scale * bb * bb)
    END;
    *)
    VAR
      rr := FLOAT(ent.rgb.r, LONGREAL);
      gg := FLOAT(ent.rgb.g, LONGREAL);
      bb := FLOAT(ent.rgb.b, LONGREAL);
    BEGIN
      xcolor.red := ROUND(Scale * Math.pow(rr, GammaInverse));
      xcolor.green := ROUND(Scale * Math.pow(gg, GammaInverse));
      xcolor.blue := ROUND(Scale * Math.pow(bb, GammaInverse));
    END;
    xcolor.flags := DoAll
  END XColorFromEntry;

PROCEDURE EntryFromXColor (VAR      ent   : ScrnColorMap.Entry;
                           READONLY xcolor: X.XColor            ) =
  CONST Scale = FLOAT(LAST(Card16), LONGREAL);
  BEGIN
    ent.pix := xcolor.pixel;
    VAR
      rr := FLOAT(xcolor.red, LONGREAL) / Scale;
      gg := FLOAT(xcolor.green, LONGREAL) / Scale;
      bb := FLOAT(xcolor.blue, LONGREAL) / Scale;
    BEGIN
      (* ent.rgb.r := rr * Sqrt(rr); ent.rgb.g := gg * Sqrt(gg); ent.rgb.b
         := bb * Sqrt(bb) *)
      ent.rgb.r := FLOAT(Math.pow(rr, Gamma));
      ent.rgb.g := FLOAT(Math.pow(gg, Gamma));
      ent.rgb.b := FLOAT(Math.pow(bb, Gamma));
    END
  END EntryFromXColor;

TYPE Card16 = BITS 16 FOR [0 .. 16_ffff];

PROCEDURE InitColorMapOracle (         orc  : ColorMapOracle;
                                       st   : XScreenType.T;
                              READONLY vinfo: X.XVisualInfo   ):
  ColorMapOracle RAISES {TrestleComm.Failure}
  <* LL.sup = st.trsl *> =

  PROCEDURE RampMask (VAR ramp : ScrnColorMap.Ramp;
                          mask : Ctypes.unsigned_long;
                          index: Prim               ) =
    VAR mult := Word.And(mask, Word.Not(mask - 1));
    BEGIN
      ramp.mult[index] := mult;
      ramp.last[index] := Word.Divide(mask, mult);
    END RampMask;

  VAR
    vis   := vinfo.visual;
    xid   := X.XDefaultColormap(st.trsl.dpy, st.screenID);
    class := vis.class;
  BEGIN
    TRY
      orc.st := st;
      orc.defaultCM :=
        InnerColorMapNew(
          orc, xid, NIL, class # X.GrayScale AND class # X.PseudoColor,
          vinfo.depth, direct := class = X.DirectColor);
      VAR
        atm             := XClient.ToAtom(st.trsl, "RGB_DEFAULT_MAP");
        n  : Ctypes.int;
        xsc, xscp: X.XStandardColormapStar;
        success := X.XGetRGBColormaps(
                     st.trsl.dpy, st.root, ADR(xsc), ADR(n), atm) # 0;
        x, y: X.VisualID;
      BEGIN
        IF success THEN
          success := FALSE;
          xscp := xsc;
          FOR i := 0 TO n - 1 DO
            x := xscp.visualid;
            y := orc.st.visual.visualid;
            IF x = y THEN
              success := TRUE;
              WITH ramp = orc.defaultCM.ramp DO
                ramp.last[Prim.Red] := xscp.red_max;
                ramp.last[Prim.Green] := xscp.green_max;
                ramp.last[Prim.Blue] := xscp.blue_max;
                ramp.mult[Prim.Red] := xscp.red_mult;
                ramp.mult[Prim.Green] := xscp.green_mult;
                ramp.mult[Prim.Blue] := xscp.blue_mult;
                IF xscp.colormap = xid THEN
                  ramp.base := xscp.base_pixel
                ELSE
                  ramp.base := -1
                END
              END
            END;
            INC(xscp, ADRSIZE(X.XStandardColormap))
          END;
          X.XFree(LOOPHOLE(xsc, Ctypes.char_star))
        END;
        IF NOT success THEN
          WITH ramp = orc.defaultCM.ramp DO
            IF class = X.DirectColor OR class = X.TrueColor THEN
              RampMask(ramp, vis.red_mask, Prim.Red);
              RampMask(ramp, vis.green_mask, Prim.Green);
              RampMask(ramp, vis.blue_mask, Prim.Blue);
              ramp.base := 0
            ELSE
              WITH np = Word.Shift(1, vis.bits_per_rgb) - 1 DO
                ramp.last[Prim.Red] := np;
                ramp.last[Prim.Green] := np;
                ramp.last[Prim.Blue] := np;
                ramp.base := -1
              END;
            END
          END
        END
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
    RETURN orc
  END InitColorMapOracle;

PROCEDURE ColorMapDefault (orc: ColorMapOracle): ScrnColorMap.T RAISES {} =
  BEGIN
    RETURN orc.defaultCM
  END ColorMapDefault;

PROCEDURE ColorMapList (<*UNUSED*> orc       : ColorMapOracle;
                        <*UNUSED*> pat       : TEXT;
                        <*UNUSED*> maxResults: CARDINAL        ):
  REF ARRAY OF TEXT RAISES {} =
  BEGIN
    RETURN NIL
  END ColorMapList;

PROCEDURE ColorMapLookup (<*UNUSED*> orc: ColorMapOracle;
                          <*UNUSED*> pat: TEXT            ): ScrnColorMap.T
  RAISES {} =
  BEGIN
    RETURN NIL
  END ColorMapLookup;

PROCEDURE ColorMapNew (           orc      : ColorMapOracle;
                                  nm       : TEXT             := NIL;
                       <*UNUSED*> preLoaded                   := TRUE ):
  ScrnColorMap.T RAISES {TrestleComm.Failure} =
  VAR
    nxid: X.Colormap;
    res : ScrnColorMap.T;
  BEGIN
    TRY
    IF orc.defaultCM.readOnly THEN RETURN orc.defaultCM END;
    TrestleOnX.Enter(orc.st.trsl);
    TRY
      nxid := X.XCreateColormap(
                orc.st.trsl.dpy, orc.st.root, orc.st.visual, X.AllocNone);
      res := InnerColorMapNew(orc, nxid, nm, FALSE, orc.defaultCM.depth,
                              orc.defaultCM.direct);
      res.ramp := orc.defaultCM.ramp;
      res.ramp.base := -1;
      RETURN res
    FINALLY
      TrestleOnX.Exit(orc.st.trsl)
    END
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
  END ColorMapNew;

PROCEDURE InnerColorMapNew (           orc     : ColorMapOracle;
                                       cm      : X.Colormap;
                            <*UNUSED*> nm      : TEXT             := NIL;
                                       readOnly: BOOLEAN;
                                       depth   : INTEGER;
                                       direct  : BOOLEAN                  ):
  XColorMap =
  BEGIN
    RETURN NEW(XColorMap, st := orc.st, xid := cm, readOnly := readOnly,
               depth := depth, direct := direct)
  END InnerColorMapNew;

BEGIN
END XScrnCmap.

