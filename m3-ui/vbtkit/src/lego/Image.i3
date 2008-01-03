(* Copyright (C) 1992, Digital Equipment Corporation                      *)
(* All rights reserved.                                                   *)
(* See the file COPYRIGHT for a full description.                         *)
(*                                                                        *)
(* by Steve Glassman                                                      *)
(* Last modified on Mon Jun 14 20:57:53 PDT 1993 by meehan                *)
(*      modified on Tue Feb  9 11:41:24 PST 1993 by mhb                   *)
(*      modified on Mon Nov 2 12:45:44 PST 1992 by steveg                 *)
<* PRAGMA LL *>

(* An "Image.T" is a screen-independent specification of an {\it
   image}.  An image is a pixmap that includes specifications for
   both color and resolution. It is rendered consistently across
   screen types in terms of its colors and size. *)

INTERFACE Image;

IMPORT Pixmap, Rd, ScrnPixmap, Thread, TrestleComm, 
  VBT, Word, Wr;

TYPE T = Pixmap.T;

EXCEPTION Error;

(* An "Image.Raw" is an array of pixels, with both resolution and
   color information.  It is like a "Pixmap.Raw", with the
   addition of resolution and color information. An "Image.T" is
   built from an "Image.Raw" pixmap using procedures in this interface.

   There are three types of "Image.Raw" pixmaps: The "RawBitmap"
   represents bitmaps (1-bit deep pixmaps); the "RawPixmap"
   represents pixmaps that do not have a color table; and the
   "RawPixmapCMap" represents pixmaps that use a color table. *)

TYPE
  Pixel = Word.T;
  Mode = {Stable, Normal, Accurate};
  RGB = RECORD r, g, b: REAL END;

TYPE
  Raw = OBJECT
      width, height: INTEGER;
      xres, yres: REAL := 75.0; (* in pixels per inch *)
    METHODS
      get (h, v: INTEGER): Pixel;
      set (h, v: INTEGER; pixel: Pixel);
    END;

  RawBitmap = Raw BRANDED OBJECT END;

  RawPixmap = Raw OBJECT
      needsGamma := TRUE;
      colorMode  := Mode.Normal;
    END;

  RawPixmapCMap = RawPixmap OBJECT 
      colors: REF ARRAY OF RGB;  
    END;

(* If "pm" is a "Raw" pixmap, then "pm" contains "pm.height"
   rows, and each row contains "pm.width" elements.  These fields
   are read-only after they are initialized.  The pixels are
   accessed with (0,0) in the northwest corner and
   "(width-1,height-1)" in the southeast corner.  The "pm.xres"
   and "pm.yres" fields specify the resolution at which "pm" was 
   designed. The "get" and "set" methods retrieve and store
   individual elements of the pixmap. 

   Each subtype of "Raw" can interpret a ``pixel'' in whatever way it
   chooses. The three subtypes defined here do the following:

   \begin{itemize}

   \item If "pm" is a "RawBitmap" pixmap, then it is guaranteed that
   the method "pm.get" will return a 0 or 1.  In the call
   "pm.set(h,v,pixel)", only the least significant bit of "pixel"
   is used.

   \item
   If "pm" is a "RawPixmap", the pixels in "pm" encode an RGB
   value each of whose components is 8 bits.  An "(r,g,b)" triple
   is stored as

|  r * 256 * 256 + g * 256 + b

   and each of "r", "g", and "b" is between 0 and 255.  The field
   "pm.needsGamma" indicates whether to let Trestle gamma-correct
   the colors.  The "pm.colorMode" field determines how each RGB
   value in the pixmap should be displayed on color-mapped
   display.  

   \item
   If "pm" is a "RawPixmapCMap", the pixels in "pm" are used as
   an index into the color table stored in the field
   "pm.colors".

   \end{itemize}

   The colors used to display a colored pixmap "pm" depends 
   on a number of factors. The "pm.colorMode" field is 
   used to match colors in
   the pixmap with colors in the color table, as described in the
   "ScrnPixmap" interface.  The matching 
   depends on other applications running, on other pixmaps
   being displayed, and on the depth of the screen.

   The current implementation does not perform any dithering, except
   on monochrome screens.  On monochrome screen, a very crude
   ``thresholding'' is performed: if the brightness of the color
   is more than 50\% of the maximum brightness, the screen's
   foreground color is used.  Otherwise, the screen's background
   color is used. *)


(* \subsubsection{Retrieving and storing ``raw'' pixmaps}

   An "Image.Raw" can be built from a reader containing an image in
   Jef Poskanzer's ``portable anymap file'' (``pnm'') format, and a
   ``pnm'' description of an "Image.Raw" can be stored into a writer.

   There are many tools available in the public domain for
   manipulating images in ``pnm'' format and for converting
   between that format and other formats (e.g., GIF, X11,
   Macintosh PICT, HP PaintJet, and so on).

   There are three types of ``pnm'' files: \begin{itemize}

   \item ``pbm'' -- portable bitmap file

   \item ``pgm'' -- portable graymap file

   \item ``ppm'' -- portable pixmap file

   \end{itemize} Each of these format has two variants: ``raw''
   and ``ASCII.''  In the ``ASCII'' version, pixel values are
   stored as ASCII decimal numbers.  In the ``raw'' version,
   pixel values must be less than 256 and are stored as plain
   bytes. *)

PROCEDURE FromRd (rd: Rd.T): Raw
  RAISES {Thread.Alerted, Rd.Failure, Error};
<* LL = arbitrary *>
(* Returns an "Image.Raw" from the reader "rd" containing an
   image in ``pnm'' format.  Pixels in ``ppm'' files are
   normalized to 8 bits per channel and intensity values of
   ``pgm'' files are normalized to 8 bits. *)

PROCEDURE ToWr (raw: Raw; wr: Wr.T)
  RAISES {Thread.Alerted, Wr.Failure};
<* LL = arbitrary *>
(* Store an ASCII description of "raw" into the writer "wr" using
   ``pnm'' format. *)

(* Procedures "FromRd" and "ToWr" are not guaranteed to be
   idempotent because pixel values are normalized by "FromRd" to
   be 8 bits.  Also, the ``pnm'' format produced by "ToWr" is
   either ASCII ``pbm'' for subtypes of "RawBitmap" or ASCII
   ``ppm'' for subtypes of "RawPixmap", whereas procedure
   "FromRd" can accept these formats as well as the ``raw''
   variants and grayscale formats (``pgm'').

   The more serious limitation of using ``pnm'' format is that
   ``pnm'' does include any information about the pixmap
   resolution or color matching.  "FromRd" will use the default
   resolution of a "Raw" and the default color parameters of a
   "RawPixmap"; "ToWr" simply ignores the resolution and color
   fields. *)

(* \subsubsection{Creating ``raw'' pixmaps from a VBT}

   "FromVBT" captures the information in an arbitrary VBT into an
   "Image.Raw" of particular dimensions: *)

PROCEDURE FromVBT(v : VBT.T; width, height: REAL): Raw
   RAISES {TrestleComm.Failure};
<* LL = VBT.mu *>
(* Return a screen-independent "Raw" that describes "v" when 
   "v" is scaled to be "width" by "height" millimeters. *)

(* The current implementation of "FromVBT" will cause "v" to be
   redisplayed multiple times: First "v" is detached from its parent
   "pm" (unless "pm = NIL"). Next, "v" is installed in an offscreen
   Trestle window, with an appropriate "ScaleFilter" inserted to make
   "v" the correct size. A call to "VBT.Capture" creates a
   screen-dependent version of the offscreen window. At this point,
   "v" is detached from the offscreen window, and reattached to "pm"
   (unless "pm = NIL").  Each time that "v" changes its parent,
   various VBT methods (reshape, rescreen, redisplay, and so on) are
   called.

   The following procedure converts a 
   screen-dependent pixmap (such as that returned
   by "VBT.Capture"), into one that is screen-independent: *)

PROCEDURE FromScrnPixmap (
    spm: ScrnPixmap.T; 
    st: VBT.ScreenType): Raw RAISES {TrestleComm.Failure};
<* LL.sup <= VBT.mu *>
(* Returns a screen-independent "Raw" that describes the pixmap
   "spm" when displayed on "st". Any field of "Raw" that cannot be 
   computed from "spm" and "st" is given its default value. For example,
   the "needsGamma" and the "colorMode" fields of pixmaps that are
   deeper than 1-bit. *)

(* \subsubsection{Building an image from ``raw'' pixmaps}

   The remaining procedures in this interface create an "Image.T"
   from an "Image.Raw" pixmap: *)

PROCEDURE Unscaled (raw: Raw): T;
<* LL.sup <= VBT.mu *>
(* Returns a pixmap that will display as "raw".  The pixels in
   "raw" will not be scaled regardless of the screen's
   resolution. *)

(* For example, consider a pixmap "pm" whose dimensions are 150 wide
   by 50 high.  On a 75dpi screen (a typical 1993-vintage monitor),
   the pixmap "pm" would appear 2 inches wide and 2/3 inches high.  On
   a high-resolution monitor of 300 dpi, "pm" would appear 1/2 inch
   wide and 1/6 inch high.  The "pm.xres" and "pm.yres" fields are
   ignored.

   If you want "pm" always to appear as 2 inches by 2/3 inches,
   regardless of the pixel density of the monitor, you'd use
   "Scaled" instead: *)

PROCEDURE Scaled (raw: Raw): T;
<* LL.sup <= VBT.mu *>
(* Return a pixmap that will display as "raw", scaled for the
   screen's resolution.  The horizontal and vertical dimensions
   are scaled independently. *)

(* The current implementation scales pixmaps by non-negative
   integer amounts: horizontally by "ROUND(dpiX/pm.xres)" and
   vertically by "ROUND(dpiY/pm.yres)", where "dpiX" and "dpiY"
   are the horizontal and vertical resolution of the screen,
   respectively, expressed in dots-per-inch.

   In the example above, suppose that "pm.xres" and "pm.yres" were
   both 75.  On a 300 dpi screen, "pm" would appear 2 inches wide and
   2/3 inches high.  Each pixel in "pm" would appear as a block of 4x4
   screen pixels.  If the screen were 250 dpi horizontally and 175 dpi
   vertically, then "pm" would appear $1\frac{1}{2}$ inches wide and
   $1\frac{1}{3}$ inches high.  Each pixel in "pm" would appear as a
   block of 3x2 screen pixels.

   Procedure "ScaledN" allows you to provide a collection of
   pixmaps, each at a different resolution, and scales the most
   appropriate pixmap: *)

PROCEDURE ScaledN (READONLY raws: ARRAY OF Raw;
                   tolerance: REAL     := 0.25;
                   maxScale : CARDINAL := 4     ): T;
<* LL.sup <= VBT.mu *>
(* Return a pixmap which will scale and display pixmap
   "raws[i]", where "i" is chosen so that "raws[i]" has the ``most
   appropriate'' resolution. *)

(* Specifically, "i" is chosen such to minimize the {\em scale factor}
   (the amount that a ``raw'' pixmap must be scaled) while remaining
   within the given error "tolerance".

   The scale factor of pixmap "pm" is

|  MAX (dpiX/pm.xres, dpiY/pm.yres)

   where "dpiX" and "dpiY" are the horizontal and vertical
   resolutions of the screen, respectively, expressed in
   dots-per-inch.

   For a given scale factor "s", the error is

| ABS (MAX ((dpiX - MAX(s, maxScale) * pm.xres) / dpiX, 
|           (dpiY - MAX(s, maxScale) * pm.yres) / dpiY))

   If none of the pixmaps in the "raws" array satisfies the
   tolerance, then the pixmap giving the smallest error is
   chosen.

   The purpose of "tolerance" and "maxScale" is to allow the user
   control over the interpretation of ``most appropriate'' when
   chosing the pixmap.

   \begin{itemize}

   \item A small "tolerance" ensures a small error, which can
   mean a larger scale factor.

   For example, suppose the screen has a resolution of 300 dpi and
   pixmaps that are 150 and 250 dpi.  When "tolerance" \verb|<| 1/6,
   then "ScaledN" chooses the 150 dpi pixmap with a scale factor equal
   to 2, rather than the 250 dpi pixmap with a scale factor equal to 1.

   \item A small "maxScale" makes it less likely that a very
   low-resolution pixmap (which happens to give very small error)
   is chosen over a higher-resolution pixmap (which gives a
   larger error).


   For example, suppose the screen has a resolution of 300 dpi and
   pixmaps that are 50 and 200 dpi.  If "tolerance" \verb|>| 1/3, then
   "ScaledN" always chooses the 200 dpi pixmap, because the error,
   (300 - 200)/300=1/3, is within the tolerance and the scale factor
   for 200 dpi is less than the scale factor for the 50 dpi pixmap.
   However, when "tolerance" \verb|<| 1/3, the 50 dpi pixmap is chosen
   unless "maxScale" \verb|<=| 4.

   \end{itemize}

*)

END Image.



