(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Mon Jan  8 14:16:32 PST 1996 by heydon  *)
(*      modified on Tue Jan 31 10:08:57 PST 1995 by kalsow  *)
(*      modified on Sun Aug  7 02:52:01 PDT 1994 by msm     *)
(*      modified on Fri Mar 11 15:30:28 PST 1994 by gnelson *)
(*      modified on Mon Nov 22 13:39:27 PST 1993 by steveg  *)
(*      modified on Fri May  7 17:28:53 PDT 1993 by mjordan *)
(*      modified on Mon Feb 24 13:59:53 PST 1992 by muller *)
<*PRAGMA LL*>

UNSAFE MODULE XScrnPxmp;

IMPORT Ctypes, Palette, Pixmap, Point, Rect, ScrnPixmap, ScreenType,
       TrestleComm, Word, X, XClientF, XScreenType, XScrnTpRep, TrestleOnX,
       PaintPrivate;

REVEAL
  T = T_Pub BRANDED OBJECT
        copyGC := ARRAY BOOLEAN OF X.GC{NIL, NIL};
        (* copyGC[FALSE] is for initializing depth 1 pixmaps, copyGC[TRUE]
           is for initializing deep pixmaps. *)
        bestX, bestY := ARRAY BOOLEAN OF INTEGER{-1, ..};
        (* [FALSE] => stipple, [TRUE] => tile *)
        tileGC            := ARRAY BOOLEAN OF X.GC{NIL, ..};
        pmcount: CARDINAL := 0
        (* number of entries in pmtable *)
      END;

TYPE
  XPixmap = ScrnPixmap.T OBJECT
              st: XScreenType.T;
            OVERRIDES
              unload   := PixmapUnregister;
              localize := PixmapLocalize;
              free     := PixmapFree
            END;

  PixmapOracle = ScrnPixmap.Oracle OBJECT
                   st: XScreenType.T;
                 OVERRIDES
                   load    := PixmapRegister;
                   list    := PixmapList;
                   lookup  := PixmapLookup;
                   builtIn := PixmapBuiltIn
                 END;

PROCEDURE NewOracle (st: XScreenType.T): ScrnPixmap.Oracle =
  BEGIN
    RETURN NEW(PixmapOracle, st := st)
  END NewOracle;

PROCEDURE FromXPixmap (         st   : XScreenType.T;
                                xpm  : X.Pixmap;
                       READONLY dom  : Rect.T;
                                depth: INTEGER        ): ScrnPixmap.T =
  BEGIN
    RETURN NewPixmap(
             st, XScrnTpRep.PixmapRecord{pixmap := xpm, domain := dom,
                                         depth := depth, isLazy := FALSE})
  END FromXPixmap;

PROCEDURE FakeCapture (         st    : XScreenType.T;
                                w     : X.Drawable;
                       READONLY dom: Rect.T;
                                depth : INTEGER        ): ScrnPixmap.T =
  BEGIN
    RETURN NewPixmap(
             st, XScrnTpRep.PixmapRecord{pixmap := w, domain := dom,
                                         depth := depth, isLazy := TRUE})
  END FakeCapture;

PROCEDURE PixmapDomain (st: XScreenType.T; pmId: INTEGER): Rect.T =
  BEGIN
    IF pmId < 0 THEN
      IF pmId = XScrnTpRep.SolidPixmap THEN RETURN rawSolid.bounds END;
      pmId := XScrnTpRep.SolidPixmap - pmId;
      st := st.bits
    END;
    IF pmId < NUMBER(st.pmtable^) THEN
      RETURN st.pmtable[pmId].domain
    ELSE
      RETURN Rect.Empty
    END
  END PixmapDomain;

PROCEDURE IsLazy(st: XScreenType.T; pmId: INTEGER): BOOLEAN =
  BEGIN
    IF pmId < 0 THEN
      IF pmId = XScrnTpRep.SolidPixmap THEN RETURN FALSE END;
      pmId := XScrnTpRep.SolidPixmap - pmId;
      st := st.bits
    END;
    IF pmId < NUMBER(st.pmtable^) THEN
      RETURN st.pmtable[pmId].isLazy
    ELSE
      RETURN FALSE
    END
  END IsLazy;

PROCEDURE GetDrawable(st: XScreenType.T; pmId: INTEGER): X.Drawable =
  BEGIN
    IF pmId < 0 THEN
      IF pmId = XScrnTpRep.SolidPixmap THEN RETURN X.None END;
      pmId := XScrnTpRep.SolidPixmap - pmId;
      st := st.bits
    END;
    IF pmId < NUMBER(st.pmtable^) THEN
      RETURN st.pmtable[pmId].pixmap
    ELSE
      RETURN X.None
    END
  END GetDrawable;

PROCEDURE FinishCapture (st: XScreenType.T; pmId: INTEGER; xpm: X.Pixmap) =
  BEGIN
    IF pmId < 0 THEN
      IF pmId = XScrnTpRep.SolidPixmap THEN RETURN END;
      pmId := XScrnTpRep.SolidPixmap - pmId;
      st := st.bits
    END;
    IF pmId < NUMBER(st.pmtable^) THEN
      st.pmtable[pmId].isLazy := FALSE;
      st.pmtable[pmId].pixmap := xpm
    END
  END FinishCapture; 

PROCEDURE NewPixmap (         st : XScreenType.T;
                     READONLY rec: XScrnTpRep.PixmapRecord): XPixmap
  <* LL.sup = st.trsl *> =
  VAR res := NEW(XPixmap, depth := rec.depth, bounds := rec.domain);
  BEGIN
    IF rec.depth = 1 THEN st := st.bits END;
    res.st := st;
    WITH n = NUMBER(st.pmtable^) DO
      IF n = st.pmcount THEN
        WITH new = NEW(REF ARRAY OF XScrnTpRep.PixmapRecord, 2 * n) DO
          FOR i := 0 TO n - 1 DO new[i] := st.pmtable[i] END;
          st.pmtable := new
        END
      END
    END;
    IF st.bits = st THEN
      res.id := XScrnTpRep.SolidPixmap - st.pmcount
    ELSE
      res.id := st.pmcount
    END;
    st.pmtable[st.pmcount] := rec;
    INC(st.pmcount);
    RETURN res
  END NewPixmap;

<*INLINE*> PROCEDURE XDestroyImage (xim: X.XImageStar) =
  BEGIN
    EVAL xim.f.destroy_image(xim)
  END XDestroyImage;

<*INLINE*> PROCEDURE XGetPixel (xim: X.XImageStar; x, y: Ctypes.int):
  Ctypes.unsigned_long =
  BEGIN
    RETURN xim.f.get_pixel(xim, x, y)
  END XGetPixel;

(* PixmapRegister, List, and Lookup must be changed to use the names. *)

PROCEDURE PixmapRegister (                    orc: PixmapOracle;
                                     READONLY pm : ScrnPixmap.Raw;
                          <*UNUSED*>          nm : TEXT             := NIL):
  ScrnPixmap.T RAISES {TrestleComm.Failure} =
  VAR rec: XScrnTpRep.PixmapRecord;
  BEGIN
    WITH st   = orc.st,
         trsl = st.trsl DO
      TrestleOnX.Enter(trsl);
      TRY
        IF pm.depth # 1 AND pm.depth # st.depth THEN Crash() END;
        rec.domain := pm.bounds;
        rec.depth := pm.depth;
        rec.pixmap := PixmapFromRaw(st, pm);
        rec.isLazy := FALSE;
        RETURN NewPixmap(st, rec)
      FINALLY
        TrestleOnX.Exit(trsl)
      END
    END
  END PixmapRegister;

PROCEDURE PixmapFromRaw (st: XScreenType.T; pm: ScrnPixmap.Raw): X.Pixmap
  RAISES {TrestleComm.Failure} <* LL.sup = st.trsl *> =
  VAR
    gcv: X.XGCValues;
    xim: X.XImageStar;
    res: X.Pixmap;
    w_ret, h_ret: Ctypes.unsigned_int;
  BEGIN
    TRY
      IF Rect.IsEmpty(pm.bounds) THEN RETURN X.None END;
      WITH dpy    = st.trsl.dpy,
           width  = Rect.HorSize(pm.bounds),
           height = Rect.VerSize(pm.bounds),
           depth  = pm.depth                 DO
        res := X.XCreatePixmap(dpy, st.root, width, height, depth);
        WITH deep = (depth # 1) DO
          IF st.copyGC[deep] = NIL THEN
            gcv.graphics_exposures := X.False;
            st.copyGC[deep] :=
              X.XCreateGC(dpy, res, X.GCGraphicsExposures, ADR(gcv))
          END;
          IF st.bestX[deep] = -1 THEN
            IF deep THEN
              EVAL
                X.XQueryBestTile(dpy, st.root, width, height,
                                 ADR(w_ret), ADR(h_ret))
            ELSE
              EVAL X.XQueryBestStipple(
                     dpy, st.root, width, height, ADR(w_ret),
                     ADR(h_ret))
            END;
            st.bestX[deep] := w_ret;
            st.bestY[deep] := h_ret
          END;
          xim := X.XCreateImage(
                   dpy, st.visual, depth, X.ZPixmap,
                   pm.bounds.west MOD (BITSIZE(ScrnPixmap.PixWord) DIV pm.bitsPerPixel),
                   LOOPHOLE(ADR(pm.pixels[pm.offset]), Ctypes.char_star),
                   width, height,
                   BITSIZE(ScrnPixmap.PixWord), 
                   BYTESIZE(ScrnPixmap.PixWord) * pm.wordsPerRow);
          TRY
            IF pm.pixelOrder = ScrnPixmap.ByteOrder.LSBFirst THEN
              xim.bitmap_bit_order := X.LSBFirst
            ELSE
              xim.bitmap_bit_order := X.MSBFirst
            END;
            IF PaintPrivate.HostByteOrder = PaintPrivate.ByteOrder.LSBFirst THEN
              xim.byte_order := X.LSBFirst
            ELSE
              xim.byte_order := X.MSBFirst
            END;
            xim.bitmap_unit := BITSIZE(ScrnPixmap.PixWord);
            xim.bits_per_pixel := pm.bitsPerPixel;
            X.XPutImage(
              dpy, res, st.copyGC[deep], xim, 0, 0, 0, 0, width, height);
            IF width <= st.bestX[deep] AND height <= st.bestY[deep]
                 AND (width # st.bestX[deep] OR height # st.bestY[deep])
                 AND st.bestX[deep] MOD width = 0
                 AND st.bestY[deep] MOD height = 0 THEN
              VAR
                tmp := X.XCreatePixmap(dpy, st.root, st.bestX[deep],
                                       st.bestY[deep], depth);
              BEGIN
                IF st.tileGC[deep] = NIL THEN
                  gcv.graphics_exposures := X.False;
                  gcv.fill_style := X.FillTiled;
                  st.tileGC[deep] :=
                    X.XCreateGC(
                      dpy, tmp, X.GCGraphicsExposures + X.GCFillStyle,
                      ADR(gcv))
                END;
                X.XSetTile(dpy, st.tileGC[deep], res);
                X.XFreePixmap(dpy, res);
                res := tmp;
              END;
              X.XFillRectangle(dpy, res, st.tileGC[deep], 0, 0,
                               st.bestX[deep], st.bestY[deep])
            END
          FINALLY
            xim.data := NIL;
            XDestroyImage(xim)
          END
        END
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
    RETURN res
  END PixmapFromRaw;

PROCEDURE PixmapList (<*UNUSED*> orc       : PixmapOracle;
                      <*UNUSED*> pat       : TEXT;
                      <*UNUSED*> maxResults: CARDINAL       := 1):
  REF ARRAY OF TEXT =
  BEGIN
    RETURN NIL
  END PixmapList;

PROCEDURE PixmapLookup (<*UNUSED*> orc: PixmapOracle; <*UNUSED*> name: TEXT):
  ScrnPixmap.T =
  BEGIN
    RETURN NIL
  END PixmapLookup;

PROCEDURE PixmapBuiltIn (orc: PixmapOracle; pm: Pixmap.Predefined):
  ScrnPixmap.T =
  VAR res: ScrnPixmap.T;
  BEGIN
    IF orc.st.bits # orc.st THEN
      res := Palette.ResolvePixmap(orc.st.bits, Pixmap.T{pm});
      IF pm = Pixmap.Empty.pm THEN orc.st.empty := res.id END;
      RETURN res
    END;
    TRY
      CASE pm OF
        Pixmap.Solid.pm =>
          WITH res = PixmapRegister(orc, rawSolid) DO
            res.id := XScrnTpRep.SolidPixmap;
            RETURN res
          END
      | Pixmap.Gray.pm => RETURN PixmapRegister(orc, rawGray)
      | Pixmap.Empty.pm =>
          res := PixmapRegister(orc, rawEmpty);
          orc.st.empty := res.id;
          RETURN res
      ELSE
        Crash(); <*ASSERT FALSE*>
      END
    EXCEPT
      TrestleComm.Failure =>
        RETURN NEW(XPixmap, id := 0, depth := 1, bounds := Rect.Empty)
    END
  END PixmapBuiltIn;

PROCEDURE PixmapLocalize (pm: XPixmap; READONLY rect: Rect.T):
  ScrnPixmap.Raw RAISES {TrestleComm.Failure} =
  VAR
    res: ScrnPixmap.Raw;
    id                  := pm.id;
  BEGIN
    TRY
    IF id = XScrnTpRep.SolidPixmap THEN RETURN rawSolid END;
    WITH r      = Rect.Meet(rect, pm.bounds),
         st     = pm.st,
         trsl   = st.trsl,
         dpy    = trsl.dpy,
         width  = Rect.HorSize(r),
         height = Rect.VerSize(r)             DO
      IF Rect.IsEmpty(r) THEN RETURN NIL END;
      IF id < 0 THEN id := XScrnTpRep.SolidPixmap - id END;
      TrestleOnX.Enter(trsl);
      TRY
        WITH xim = X.XGetImage(
                     dpy, st.pmtable[id].pixmap, r.west - pm.bounds.west,
                     r.north - pm.bounds.north, width, height, -1,
                     X.ZPixmap) DO
          res := ScrnPixmap.NewRaw(xim.depth, r);
          FOR v := r.north TO r.south - 1 DO
            FOR h := r.west TO r.east - 1 DO
              res.set(
                Point.T{h, v}, XGetPixel(xim, h - r.west, v - r.north))
            END
          END;
          XDestroyImage(xim)
        END
      FINALLY
        TrestleOnX.Exit(trsl)
      END
    END;
    EXCEPT X.Error => RAISE TrestleComm.Failure END;
    RETURN res
  END PixmapLocalize;

PROCEDURE PixmapUnregister (<*UNUSED*> pm: ScrnPixmap.T) =
  BEGIN
  END PixmapUnregister;

PROCEDURE PixmapFree (pm: XPixmap) RAISES {TrestleComm.Failure} =
  VAR
    id   := pm.id;
    st   := pm.st;
    trsl := st.trsl;
    dpy  := trsl.dpy;
  BEGIN
    TRY
      IF id = XScrnTpRep.SolidPixmap THEN RETURN END;
      IF id < 0 THEN id := XScrnTpRep.SolidPixmap - id END;
      TrestleOnX.Enter(trsl);
      TRY
        IF st.pmtable[id].isLazy THEN
          st.pmtable[id].pixmap := X.None;
          st.pmtable[id].isLazy := FALSE
        ELSE
          WITH xpm = st.pmtable[id].pixmap DO
            IF xpm # X.None THEN X.XFreePixmap(dpy, xpm) END;
            xpm := X.None
          END
        END
      FINALLY
        TrestleOnX.Exit(trsl)
      END;
    EXCEPT
      X.Error => RAISE TrestleComm.Failure
    END;
  END PixmapFree;

EXCEPTION FatalError;

PROCEDURE Crash() =        
  <* FATAL FatalError *>
  BEGIN
    RAISE FatalError
  END Crash;

VAR
  rawSolid, rawGray, rawEmpty: ScrnPixmap.Raw;
  gp                         : Word.T         := 5;
  i                                           := 4;

BEGIN 
  rawSolid := ScrnPixmap.NewRaw(1, Rect.FromSize(1, 1));
  IF FIRST(ScrnPixmap.PixWord) < 0 THEN
    rawSolid.pixels[rawSolid.offset] := FIRST(ScrnPixmap.PixWord);
  ELSE
    rawSolid.pixels[rawSolid.offset] := LAST(ScrnPixmap.PixWord);
  END;
  rawEmpty := ScrnPixmap.NewRaw(1, Rect.FromSize(1, 1));
  rawEmpty.pixels[rawEmpty.offset] := 0;
  rawGray := ScrnPixmap.NewRaw(1, Rect.FromSize(2, 2));
  WHILE i < BITSIZE(ScrnPixmap.PixWord) DO
    gp := Word.Or(gp, Word.LeftShift(gp, i));
    INC(i,i)
  END;
  rawGray.pixels[rawGray.offset] := gp;
  IF FIRST(ScrnPixmap.PixWord) < 0 THEN
    rawGray.pixels[rawGray.offset + rawGray.wordsPerRow] :=
      Word.Not(gp)
  ELSE
    rawGray.pixels[rawGray.offset + rawGray.wordsPerRow] := 
      Word.And(LAST(ScrnPixmap.PixWord), Word.Not(gp));
  END
END XScrnPxmp.
