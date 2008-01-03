(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: interface for accessing images stored in readers *)

(* Last modified on Thu Mar 23 15:06:24 PST 1995 by birrell   *)

INTERFACE ImageRd;

IMPORT Images, PaintOp, Rd, Thread, VBT, Wr;

TYPE T <: Public;
  (* Represents a pixel map or bit map stored in an Rd.T *)

TYPE Format = { PBM, PGM, PPM, LGM };

TYPE Contents = Images.RawContents OBJECT
    format: Format;
      (* Indicates the format of the data in the reader.  This allows
         the client to determine which image format is involved without
         writing more parsing routines, and without wasting a non-seekable
         reader (this is admittedly somewhat specialized; it's designed
         for a usage pattern in BuildLectern). *)
  END;

TYPE Public = Images.T OBJECT
    (* An image object, suitable for painting by ImageVBT.T.  The "domain"
       method will return the domain of the image specified by "init", and
       the "paint" method will paint the appropriate part of it.  The "paint"
       method applies optional gamma correction using the "gamma"
       argument given to "init":  if each channel of each pixel is viewed as a
       number in [0..1], the pixel painted will be the pixel from the original
       pixmap with each channel raised to the power "1/gamma".  Thus gamma > 1
       tends to lighten the image. *)
  METHODS
    init(rd: Rd.T; start, length: CARDINAL;
         op: PaintOp.T := PaintOp.Copy;
         st: VBT.ScreenType := NIL;
         gamma: REAL := 1.0): T;
      (* LL = any *)
      (* Initializes this object to represent the pixel map
         recorded in "rd", which must be seekable and immutable (except that
         it need be neither seekable nor immutable if your only access to
         the image is by calling the "contents" method).  The pixel
         map starts at index "start" in "rd" and contains "length" bytes.
         The pixel map should be in pbm, pgm, ppm, or lgm format.
         If the pixel map has color/gray information (e.g. pgm or ppm), the
         "paint" method paints it using the given colors and PaintOp.Copy;
         in this case the "op" argument is ignored.  If the pixel map is
         actually a bit map (e.g. pbm), and "op" is PaintOp.Copy, then the
         "paint" method paints "1" bits black and "0" bits white.  If the
         pixel map is actually a bit map and "op" is not PaintOp.Copy, then
         the "paint" method paints the bits using the given "op"; in this case
         "op" should generally be a tint pair such as PaintOp.BgFg.
         The argument "st" is an optional performance hint: if it is not
         NIL, the implementation might pre-compute a ScrnPixmap.Raw for
         painting the pixel map on "st".  The argument "gamma" is recorded for
         use when painting: see the comment above.  It is legal to call the
         "init" method more than once, to recycle the object and its
         internal storage. *)
    close();
      (* LL = any *)
      (* Closes the image (but not its reader).  Subsequent method calls are
         illegal. *)
    toEPSF(wr: Wr.T; binary: BOOLEAN)
           RAISES { Wr.Failure, Thread.Alerted, Images.Error };
      (* LL = any *)
      (* Writes an EPSF form of the image on "wr".  If "binary", EPSF might
         involve binary data, and then might be much smaller *)
  END;

PROCEDURE Copy(rd: Rd.T; wr: Wr.T)
               RAISES { Rd.Failure, Wr.Failure, Thread.Alerted, Images.Error };
  (* Copies an image in PBM, PGM or PPM format from "rd" into "wr". *)

END ImageRd.
