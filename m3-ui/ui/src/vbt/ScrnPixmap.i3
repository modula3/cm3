(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Thu Aug  4 20:54:42 PDT 1994 by msm      *)
(*      modified on Mon Feb 24 13:58:11 PST 1992 by muller   *)
(*      modified on Thu Dec 12  2:31:53 PST 1991 by gnelson  *)
(*      modified on Thu Apr 12 15:21:37 PDT 1990 by steveg   *)
<*PRAGMA LL*>

(* A "ScrnPixmap.T" is a handle on a rectangular array of pixels that
   is valid for use on a particular screentype, called the {\it owner}
   of the handle.  Some handles have names; others are anonymous.  A
   named handle is valid forever; the pixmap referenced by an anonymous
   handle will be garbage-collected when all handles to it have been
   dropped.  *)
   
INTERFACE ScrnPixmap;

IMPORT Point, Rect, Word, TrestleComm, Pixmap, BasicCtypes;

EXCEPTION Failure;

TYPE Raw = Pixmap.Raw;

(* The raw representation of a pixmap is revealed at the end of this 
   interface. 

\subsubsection{Obtaining handles from the oracle} *)

TYPE
  Oracle = Private OBJECT
  METHODS 
    <* LL.sup <= VBT.mu *>
    load(READONLY r: Raw; nm: TEXT := NIL): T 
      RAISES {TrestleComm.Failure};
    list(pat: TEXT; maxResults: CARDINAL := 1)
      : REF ARRAY OF TEXT RAISES {TrestleComm.Failure};
    lookup(name: TEXT): T RAISES {TrestleComm.Failure};
    builtIn(pm: Pixmap.Predefined): T;
  END;
  Private <: ROOT;

(* For a screentype "st", the field "st.pixmap" is an "Oracle" that 
   produces pixmaps owned by "st".

   The method call "st.pixmap.load(r, nm)" allocates and returns
   a pixmap handle "p" owned by "st" whose contents are equal to "r".  The
   depth of "r" must either be "1" or "st.depth", otherwise there is
   a checked runtime error.  If "nm # NIL", "p" receives the name "nm",
   and any pixmap handle owned by "st" that previously had the name
   "nm" becomes anonymous.

   The method call "st.pixmap.list(pat, maxResults)" returns the names
   of all pixmaps owned by "st" that match the pattern "pat".  The list
   of results may be truncated to length "maxResults".  A "*" matches
   any number of characters and a "?" matches any single character.

   The method call "st.pixmap.lookup(name)" return the pixmap with the
   given name, or "NIL" if no pixmap has this name.

   The method call "st.pixmap.builtIn(pm)" returns the screen-dependent
   pixmap valid for "st" that corresponds to the predefined
   screen-independent "Pixmap.T{pm}".
   
   The locking level for all methods is "LL.sup <= VBT.mu". *)

(* \subsubsection{The handle object} *)

TYPE
  T <: Public; 
  Public = OBJECT (*CONST*)
    id: INTEGER;
    depth: INTEGER;
    bounds: Rect.T
  METHODS
    <* LL.sup <= VBT.mu *>
    localize(READONLY rect: Rect.T): Raw 
      RAISES {TrestleComm.Failure};
    unload() RAISES {TrestleComm.Failure};
    free() RAISES {TrestleComm.Failure}
  END;

(* If "pm" is a "ScrnPixmap.T", then "pm.id" is an identifier whose
   interpretation depends on the screentype that owns "pm".  The field
   "pm.depth" is the number of bits in each pixel of "pm", and
   "pm.bounds" is the rectangular extent of "pm".
    
   The method call "pm.localize(rect)" returns a raw pixmap equal to
   a rectangualr subpixmap of the one on which "pm" is a handle.  The
   bounds of the raw pixmap returned by "localize" is "Rect.Meet(rect,
   pm.bounds)".
    
   The method call "pm.unload()" causes "pm" to become anonymous.

   Pixmaps consume large amounts of memory.  The method call "pm.free()"
   releases the memory associated with the pixmap.  You must make sure
   that all "VBT"s using "pm" have finished painting before you
   free it. After a call to "free", the pixmap bounds and contents
   are arbitrary. *)

(* \subsubsection{The raw representation}

   A raw pixmap allows the client to directly locate and modify the
   bits of the pixmap.  The following procedure produces a new raw
   pixmap: *)
      
PROCEDURE NewRaw(dpth: INTEGER; 
  READONLY bnds: Rect.T): Raw;
<* LL arbitrary *>
(* Allocate and return a raw pixmap with the given depth and bounds. *)
  
(* The initial contents of the pixmap returned by "NewRaw" are undefined.

   Here is the representation of a raw pixmap: *)

REVEAL Pixmap.Raw <: Raw_Public;
       
TYPE
  Raw_Public = OBJECT
    depth: INTEGER; 
    bounds: Rect.T; 
    pixels: REF ARRAY OF PixWord;
    offset: INTEGER;
    bitsPerPixel: INTEGER; 
    wordsPerRow: INTEGER;
    pixelOrder: ByteOrder;
    westRounded: INTEGER;
  METHODS
    get(READONLY pt: Point.T): Pixel;
    set(READONLY pt: Point.T; pix: Pixel);
    sub(READONLY rect: Rect.T): Raw;
  END;
  
  PixWord = BasicCtypes.unsigned_int;
  Pixel = Word.T;
  ByteOrder = {MSBFirst, LSBFirst};

(* The methods provide the easiest way to operate on a raw pixmap, and
   we will explain them first.  Let "pm" be a "ScrnPixmap.Raw", then:

   The method call 

| pm.get(pt)

   returns the pixel value at the point "pt" in the pixmap.  The result
   is undefined if "pt" is not in "pm.bounds".

   The method call 

| pm.set(pt, pix)

   sets the pixel value at the point "pt" of the pixmap "pm" to the
   value "pix".  It is a noop if "pt" is not in "pm.bounds".

   The method call 
   
| pm.sub(rect)

   returns a pixmap whose bounds are "Rect.Meet(rect, pm.bounds)" and
   whose contents are shared with "pm"'s.

   It is also possible to bypass the methods and access the
   data in the raw pixmap directly. Here is the specification
   for the internal layout of pixels in a raw pixmap:
   
   A value "pm" of type "Pixmap.Raw" is a rectangular subregion of a
   larger rectangular pixmap, which we shall call the {\it surround}.
   The surround is a word-aligned pixmap, stored in raster-scan order
   by rows.  Pixels do not cross word boundaries.  More precisely, the
   westmost pixel in each row of the surround is always a pixel whose
   "h"-coordinate is a multiple of "pixelsPerWord" (which is equal to
   "BITSIZE(PixWord) DIV pm.bitsPerPixel").  The eastmost pixel in each row
   of the surround is always a pixel whose "h"-coordinate modulo
   "pixelsPerWord" is congruent to "pixelsPerWord-1".  Hence, the number
   of pixels in each row of the surround is a multiple of
   "pixelsPerWord".  The value "pm.wordsPerRow" is the number of words
   that are needed to store one row of the surround.

   The value "pm.bitsPerPixel" might be greater than "pm.depth"; for
   example, a twelve-bit deep pixmap might be stored with 
   sixteen bits per pixel.

   The pixels of the surround are stored in the array "pm.pixels".  Each
   row is represented in "pm.wordsPerRow" adjacent words; the first
   of these words stores the westmost "pixelsPerWord" pixels of the
   row, the following word stores the adjacent "pixelsPerWord" pixels,
   and so on until the last word, which stores the eastmost
   "pixelsPerWord" pixels.

   The order in which pixels are packed into words is indicated by
   "pm.pixelOrder".  In this discussion, bit "0" is the least
   significant bit and bit "BITSIZE(PixWord) - 1" is the most significant bit
   of a word.

   If "pm.pixelOrder = LSBFirst", the bits of the pixels are as follows
   (where "bpp" is "pm.bitsPerPixel"):

| `pixel "0":  bits` 0..bpp-1
| `pixel "1":  bits` bpp..2*bpp-1
| ...
| `pixel "i":  bits` i*bpp..(i+1)*bpp-1
   
   If "pm.pixelOrder = MSBFirst", the pixels are stored in reverse order, 
   so that pixel "i" occupies the same bits as pixel "pixelsPerWord-i-1"
   occupies for "LSBFirst".

   A "Word.Extract" of the bits indicated above, from the correct word,
   gives the pixel's value.  If the word size does not contain an
   integral number of pixels, the unused bits in the word have
   undefined values.
   
   The pixmap "pm" itself is a rectangular region selected from the surround;
   the value "pm.bounds", of type "Rect.T", specifies the domain of "pm".  The
   value "pm.offset" specifies where in "pm.pixels" the words containing the
   pixels of "pm" can be found.  In particular, the northwestern-most bit of
   "pm", the bit with coordinates

| h = pm.bounds.west `and` v = pm.bounds.north,

   is stored in word "pm.pixels[pm.offset]".  The pixel is the 
   "(pm.bounds.west MOD pixelsPerWord)"'th pixel of the word.  
   Its bits can be found by the earlier formulas.

   The general formula for the word containing the pixel with position 
   "h, v" is

| pm.pixels[
|  (v - pm.bounds.north) * pm.wordsPerRow + 
|  (h - pm.westRounded) DIV pixelsPerWord) + pm.offset].

   Here is another useful formula.  The surround rectangle must be at
   least wide enough to contain the subrectangle "pm.bounds", even after
   we have rounded the west edge of "pm.bounds" westward to the next word
   boundary and rounded the east edge of "pm.bounds" eastward to the next
   word boundary.  As a result, we have the inequality:

|  pm.wordsPerRow >= 
|    ((pm.bounds.east - 1) DIV pixelsPerWord) - 
|    (pm.bounds.west DIV pixelsPerWord) + 1


Finally, the value "pm.westRounded" is provided for convenience; it
is equal to

| bounds.west - (bounds.west MOD pixelsPerWord),

that is, the western boundary moved west to the nearest word boundary.
*)

END ScrnPixmap.


