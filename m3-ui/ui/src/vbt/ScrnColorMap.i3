(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:57:57 PST 1992 by muller   *)
(*      modified on Wed Dec 11 18:57:36 PST 1991 by gnelson  *)
(*      modified on Thu Apr 12 15:21:37 PDT 1990 by steveg   *)
<*PRAGMA LL*>

(* A "ScrnColorMap.T" is a handle on a colormap that is valid for some
   particular screentype, called the {\it owner} of the handle. Some
   handles have names; others are anonymous.  A named handle is valid
   forever.  The colormap referenced by an anonymous handle will be
   garbage-collected when all handles to it have been dropped.

   Every colormap has a {\it depth}; the pixel values defined by the
   color map are in the range "[0..(2^depth)-1]".  Every color-mapped
   screentype defines a set of {\it preferred} colors that cover the
   spectrum reasonably densely.  Some preferred colors are designated
   as {\it stable}.

   Clients can allocate pixels out of a color map as read-only shared
   entries or as writable exclusive entries.  The implementation
   maintains reference counts on the read-only entries so that an entry
   can be freed when it is no longer allocated to any client. *)

INTERFACE ScrnColorMap;

IMPORT TrestleComm;

(* \subsubsection{Obtaining handles from the oracle} *)

TYPE
  Oracle = Private OBJECT
    METHODS
      <* LL.sup <= VBT.mu *>
      standard(): T RAISES {TrestleComm.Failure};
      new(name: TEXT := NIL; preLoaded := TRUE): T
        RAISES {TrestleComm.Failure, Failure};
      lookup(name: TEXT): T
        RAISES {TrestleComm.Failure};
      list(pat: TEXT; maxResults: CARDINAL := 1)
        : REF ARRAY OF TEXT RAISES {TrestleComm.Failure}
    END;
  Private <: ROOT;

EXCEPTION Failure;

(* Every color-mapped screentype "st" contains a field "st.cmap" of
   type "Oracle", which hands out colormaps owned by "st":

   The method call

| st.cmap.standard()

   returns the default colormap owned by "st". This is the colormap
   that a top-level window will initially have when it is rescreened
   to "st". Initially, the stable colors are allocated read-only with
   a reference count of one.

   The method call

| st.cmap.new(name, preLoaded)

   creates and returns a new colormap owned by "st" with the given name.
   If "preLoaded" is true, the stable colors are initially allocated
   read-only; otherwise nothing is allocated initially.

   The method call

| st.cmap.lookup(name)

   returns the colormap owned by "st" with the given name, or "NIL"
   if no colormap has this name.

   The method call

| st.cmap.list(pat, maxResults)

   returns the names of colormaps owned by "st" that match the pattern
   "pat".  The list of results may be truncated to length "maxResults".
   A "*" matches any number of characters and a "?" matches any single
   character. *)

(* \subsubsection{The handle object} *)

TYPE
  T <: Public;
  Public = OBJECT (*CONST*)
      depth: INTEGER;
      readOnly: BOOLEAN;
      ramp: Ramp;
    METHODS
      <* LL.sup <= VBT.mu *>
      fromRGB(rgb: RGB; mode := Mode.Normal): Pixel
        RAISES {Failure, TrestleComm.Failure};
      read(VAR res: ARRAY OF Entry)
        RAISES {TrestleComm.Failure};
      write(READONLY new: ARRAY OF Entry)
        RAISES {Failure, TrestleComm.Failure};
      new(dim: CARDINAL): Cube RAISES
        {Failure, TrestleComm.Failure};
      free(READONLY cb: Cube)
        RAISES {TrestleComm.Failure};
    END;
  Mode = {Stable, Normal, Accurate};
  Ramp = RECORD
    base: INTEGER;
    last, mult: ARRAY Primary OF INTEGER;
  END;
  Primary = {Red, Green, Blue};
  Cube = RECORD lo, hi: Pixel END;
  Pixel = INTEGER;
  RGB = RECORD r, g, b: REAL END;
  XRGB = RECORD red, green, blue, alpha: INTEGER END;
  Entry = RECORD pix: Pixel; rgb: RGB; xrgb : XRGB END;

(* The field "cm.depth" is the depth of "cm", and "cm.readOnly" is
   "TRUE" if "cm" cannot be written.  The field "cm.ramp" defines
   a three dimensional lattice of colors preallocated in "cm",
   as follows.

   If "cm.ramp.base" is "-1", the lattice of preallocated colors is empty.

   If "cm.ramp.base" is not "-1", then the pixel value

| base + r*mult[Red] + g*mult[Green] + b*mult[Blue]

   represents the color "(r/last[Red], g/last[Green], b/last[Blue])", for "r"
   in the range "[0..last[Red]]", "g" in the range "[0..last[Green]]", and
   "b" in the range "[0..last[Blue]]".

   \medskip An "RGB" represents the color with the given blend of red,
   green, and blue.  Each of the numbers is in the range "[0.0..1.0]";
   thus the triple "(0.0, 0.0, 0.0)" specifies black.  In case of a gray scale
   display, only the "r" component is relevant.

   The method call

| cm.fromRGB(rgb, mode)

   extends the read-only portion of "cm" with a new entry whose value
   is near "rgb" and returns the pixel of the new entry.  If the
   read-only portion of "cm" already contains an entry whose value is
   near "rgb", that entry's pixel is returned.  The "mode" argument
   controls how near the new entry's value will be to "rgb", as follows.
   If "mode" is "Stable", the new entry's color is the nearest stable
   color to "rgb".  If "mode" is "Normal", the new entry's color is
   the nearest preferred color to "rgb".  If "mode" is "Accurate", the
   new entry's color is the nearest color to "rgb" that the hardware
   supports.  The method raises "Failure" if a new entry is
   required but the colormap is full.

   For each entry "e" in the array "res", the method call

| cm.read(res)

   sets "e.rgb" to the color in "cm" of the pixel "e.pixel".

   The method call

| cm.write(new)

   changes the value of "cm" at "p" to be "rgb", for each pair "(p, rgb)"
   in the array "new", assuming all these pixels are writable.  Otherwise
   the method raises "Failure".  The array "new" must be sorted.

   The method call

| cm.new(dim)

   extends the writable portion of "cm" with a set of $2^{"dim"}$ new
   entries whose pixels form a cube, and returns the cube.  The method
   raises "Failure" if the free entries of the colormap do not contain
   a cube of the given dimension.

   A "Cube" "cb" represents a set of pixels by the following rule:
   a pixel "p" is in "cb" if "Word.And(lo, pix) = lo" and
   "Word.Or(hi, pix) = hi".

   The method call "cm.free(cb)" deallocates from the writable portion
   of "cm" each entry whose pixel is in the cube "cb", assuming all
   of these pixels are allocated. *)

END ScrnColorMap.
