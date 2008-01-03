(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* The object class for paintable images. *)

(* Last modified on Mon Mar 27 15:11:48 PST 1995 by birrell   *)

INTERFACE Images;

IMPORT Point, Rect, ScrnPixmap, Thread, VBT, Wr;

EXCEPTION Error(TEXT);
  (* Raised by "contents", "paint" and "render" methods if the image data is
     somehow invalid.  The argument is a human-sensible explanation. *)

TYPE T = OBJECT
    (* An object of this class represents an image in a device-independent way.
       Actually, you need some sub-class: the default methods here are NIL.
       Note that an image's coloring is inherent in the image, so there are no
       PaintOp.T's involved in this interface. *)
  METHODS
    domain(v: VBT.Leaf): Rect.T;
      (* LL = any *)
      (* The domain of the image when painted on v's screen.  Note that this
         might lie outside of v's domain; typically, the image must be
         relocated when painting, e.g., by the difference between the
         middle of the image's domain and the middle of v's domain.
         If v=NIL, returns the domain of the image's raw data. *)
    paint(v: VBT.Leaf;
          READONLY clip: Rect.T := Rect.Full;
          READONLY delta: Point.T;
          paintChunk := 0)
         RAISES { Thread.Alerted, Error };
      (* LL <= VBT.mu *)
      (* Paints the image, offset by delta and clipped by clip, into v.
         The arguments and effect as analogous to VBT.PaintScrnPixmap.  Note
         that some sub-classes might be able to paint the requisite part of
         their image without ever constructing the full contents that would be
         returned from the "contents" or "render" methods.  The "paintChunk"
         parameter is a hint to the implementation about how many pixels
         to paint at a time, for performance and user interaction purposes.
         paintChunk=0 suggests painting all the pixels at once.  *)
    render(v: VBT.Leaf): ScrnPixmap.Raw RAISES { Thread.Alerted, Error };
      (* Returns a pixel map suitable for painting the image on v's
         screen.  The pixel map's bounds will be the image's domain, as
         would be returned by .domain(v).  "v" must not be NIL.  Hides
         errors (e.g., by substituting an empty image). Raises Error if the
         image data is malformed or unreadable. *)
    contents(): Contents RAISES { Thread.Alerted, Error };
      (* Returns the data constituting the image in device independent form.
         Particular sub-classes of Images.T might chose to return extra
         information by sub-classing "Contents". Raises Error if the
         image data is malformed or unreadable. *)
  END;

TYPE Bit = [0..1];
TYPE Channel = BITS 8 FOR [0..255];
TYPE Gray = Channel;
TYPE RGB = RECORD r, g, b, spare: Channel := 0 END;
TYPE RGBMap = REF ARRAY OF RGB;
TYPE GrayMap = REF ARRAY OF Gray;

TYPE Contents = OBJECT
    (* An object of this class represents the pixels of an image in
       device-independent form.  Note that screen-dependent data given by
       an image's ".render" method (for "v#NIL") might be quite different,
       with different colors, different equalities amongst the pixels, and
       different bounds.  This class is designed so that one could create
       sub-classes that perform image processing before delivering the pixels;
       this includes the possibility of constructing image processing filters.
       This class provides line-at-a-time access to the image.  The sub-class
       "RawContents" provides random access.
       *)
    width, height: INTEGER;
      (* Number of pixels in each scan line, and number of scan lines. *)
    map: RGBMap;
      (* Indexed by a pixel from .getLine, delivers the corresponding color.
         This field should never be NIL, although it might have no elements.
         The map applies to the entire image. *)
    isBW: BOOLEAN := FALSE;
      (* If TRUE, all the image's pixels are either black or white. *)
    isGray: BOOLEAN := FALSE;
      (* If TRUE, all the image's pixels are pure gray, including the
         possibilities of black and white. *)
    isGrayRamp: BOOLEAN := FALSE;
      (* If TRUE, the image's .map is a linear gray ramp with map[0]=white.
         Yes, I know this is peculiar; but it's mildly convenient for Lectern's
         compressed file formats.  In case of doubt, leave it FALSE. *)
  METHODS
    getLine(v: INTEGER; VAR line: ARRAY OF INTEGER);
      (* Read scan line numbered "v" (the top line is 0) into "line", which
         the caller asserts is at least .width elements long.  Each pixel
         is placed into one element on "line".  The pixel values should be
         interpreted by using them to index .map.  Note that this
         method has no default - it must be defined in a sub-class. *)
  END;

TYPE RawContents = Contents OBJECT
  (* A sub-class where the pixels are also available simultaneously, in a
     ScrnPixmap.Raw (whose bounds should be consistent with the Content's
     .width and .height fields.  Note that this sub-class has a concrete
     method for "GetLine". *)
    raw: ScrnPixmap.Raw;
  OVERRIDES
    getLine := RawGetLine;
  END;

PROCEDURE RawGetLine(c: RawContents; v: INTEGER; VAR line: ARRAY OF INTEGER);
  (* Delivers scan line "v" from c.raw. *)


(* *)
(* Utilities *)
(* *)

PROCEDURE BitFromGray(g: Gray): Bit;
  (* Returns 1 if "g" is < 128, otherwise 0; i.e. black is 1, white is 0. *)

PROCEDURE GrayFromBit(b: Bit): Gray;
  (* Returns 0 if "b" is 1, otherwise 255; i.e. maps 1 to black, 0 to white. *)

PROCEDURE GrayFromRGB(rgb: RGB): Gray;
  (* Returns a grayscale approximation of "rgb", using a standard formula
     for the perceptual brightness of R, G and B. *)

PROCEDURE RGBFromGray(g: Gray): RGB;
  (* Returns the RGB equivalent of "g"; i.e. RGB{r := g, g := g, b := g}. *)

PROCEDURE GrayMapFromRGBMap(map: RGBMap): GrayMap;
  (* Returns an array, each element of which was obtained by calling
     GrayFromRGB of the corresponding element of "map". *)

PROCEDURE Lasso(contents: RawContents): Rect.T;
  (* Returns the smallest rectangle enclosing all the pixels of the image,
     that are not equal to its north-west pixel. *)

PROCEDURE ToPNM(contents: Contents; wr: Wr.T)
                RAISES { Wr.Failure, Thread.Alerted };
  (* Writes on "wr" a representation of "contents" in PBM, PGM, or PPM
     format (depending on the .isBW and isGray fields of "contents"). *)

TYPE EmptyImage <: T;

VAR (*CONST*) Empty: EmptyImage;
  (* An image whose domain is empty, whose paint method does nothing, and
     whose lasso method returns Rect.Empty.  It's "contents" and "render"
     methods return a ScrnPixmap.Raw whose bounds are empty. *)

END Images.
