(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Mon Apr 24 16:55:29 PDT 1995 by msm      *)
(*      modified on Tue Jan 31 09:06:03 PST 1995 by kalsow   *)
(*      modified on Mon Nov 22 13:51:36 PST 1993 by steveg   *)
(*      modified on Mon Oct 11 16:13:00 PDT 1993 by sfreeman *)

UNSAFE MODULE XPicture;

IMPORT Completion, Ctypes, Picture, PictureRep, Point, Rect, TrestleComm, VBT, X,
       XClientF, XImUtil, XScreenType;

(* New() exported by XPictureFree *)

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init    := Init;
        destroy := Destroy;
        put     := Put;
      END;

CONST Offset = 0;

PROCEDURE Init (picture      : Picture.T;
                screenType   : VBT.ScreenType;
                width, height: CARDINAL        ): Picture.T
  RAISES {Picture.ScreenTypeNotSupported, Picture.TrestleFail} =
  (* these calculations taken from XShm.c *)

  PROCEDURE BytesPerLine (bitsPerPix, width, bitPad: X.Int): X.Int =
    BEGIN
      WITH nbytes = bitsPerPix * width,
           pad    = bitPad * 8          DO
        RETURN (nbytes + (pad - 1)) DIV (pad * pad); (* roundup *)
      END
    END BytesPerLine;

  VAR t := NARROW(picture, T);
  (* this should have been checked before calling Init *)
  BEGIN
    TYPECASE screenType OF
    | XScreenType.T (st) =>
        TRY
          WITH depth        = X.XDefaultDepth(st.trsl.dpy, st.screenID),
               scanlinePad  = XImUtil.ScanlinePad(st.trsl.dpy, depth),
               bitsPerPixel = XImUtil.BitsPerPixel(st.trsl.dpy, depth),
               ximage = X.XCreateImage(
                          st.trsl.dpy, st.visual, depth, X.ZPixmap, Offset,
                          NIL, width, height, scanlinePad,
                          BytesPerLine(bitsPerPixel, width, scanlinePad)) DO
            IF ximage = NIL THEN RAISE Picture.TrestleFail; END;
            t.allocByCaller := FALSE;
            t.image := LOOPHOLE(ximage, Picture.ImageStar);
            EVAL Picture.T.init(t, screenType, width, height);
            RETURN t;
          END;
        EXCEPT
          X.Error => RAISE Picture.TrestleFail
        END;
    ELSE
      RAISE Picture.TrestleFail;
    END;
  END Init;

PROCEDURE Put (                    t         : T;
                                   dpy       : X.DisplayStar;
                                   d         : X.Drawable;
                                   gc        : X.GC;
                          READONLY clip      : Rect.T;
                          READONLY delta     : Point.T;
               <*UNUSED*>          completion: Completion.T   )
  RAISES {TrestleComm.Failure} =
  VAR
    image := LOOPHOLE(t.image, X.XImageStar);
    clp := Rect.Meet(
             clip, Rect.FromCorner(delta, image.width, image.height));
    width  := clp.east - clp.west;
    height := clp.south - clp.north;
  BEGIN
    IF width > 0 AND height > 0 THEN
      TRY
        X.XPutImage(
          dpy, d, gc, image, clp.west - delta.h, clp.north - delta.v,
          clp.west, clp.north, width, height);
      EXCEPT
        X.Error => RAISE TrestleComm.Failure;
      END;
      (* the client is Sync'd by Picture.Put after this call *)
    END;
  END Put;

PROCEDURE Destroy (t: T) =
  BEGIN
    (* don't free the ximage if it was allocated by someone else. *)
    IF t.image # NIL AND NOT t.allocByCaller THEN
      t.image.data := NIL;       (* XDestroyImage frees the data as well,
                                    but it doesn't belong to us *)
      EVAL t.image.f.destroy_image(LOOPHOLE(t.image, X.XImageStar));
      t.image := NIL;
    END;
  END Destroy;

PROCEDURE MakeImage (screenType    : VBT.ScreenType;
                     width, height : Ctypes.int;
                     xoffset       : Ctypes.int       := 0;
                     bitmap_pad    : Ctypes.int       := 0;
                     bytes_per_line: Ctypes.int       := 0  ):
  Picture.ImageStar
  RAISES {Picture.TrestleFail} =
  BEGIN
    TYPECASE screenType OF
    | XScreenType.T (st) =>
        TRY
          WITH depth = X.XDefaultDepth(st.trsl.dpy, st.screenID),
               ximage = X.XCreateImage(st.trsl.dpy, st.visual, depth,
                                       X.ZPixmap, xoffset, NIL, width,
                                       height, bitmap_pad, bytes_per_line) DO
            IF ximage = NIL THEN RAISE Picture.TrestleFail; END;
            RETURN LOOPHOLE(ximage, Picture.ImageStar);
          END;
        EXCEPT
          X.Error => RAISE Picture.TrestleFail
        END;
    ELSE
      RAISE Picture.TrestleFail;
    END;
  END MakeImage;

PROCEDURE MakeCompletion (<*UNUSED*> picture: T): Completion.T =
  BEGIN
    RETURN Completion.New();
  END MakeCompletion;

BEGIN
END XPicture.
