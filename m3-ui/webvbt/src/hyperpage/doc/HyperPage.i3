(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Thu Jan 12 10:56:26 PST 1995 by kalsow  *)

(* A "HyperPage.T" is the in-memory representation of an HTML page.
   Each page has the following properties:
   
|     an underlying body of HTML
|     a line breaking width specified in pixels
|     a set of display "looks"
|     a bounding box specified in pixels with northwest corner at (0,0)
|     a region with a highlighted background
|     a numbered set of links, each one hilighted or not
|     a set of named anchor locations
|     a collection of named images
|     a title

*)

INTERFACE HyperPage;

IMPORT Rd, VBT, Point, Rect, PaintOp;

TYPE
  T <: Public;
  Public = OBJECT METHODS

    init (src           : Rd.T;
          width         : CARDINAL := 800;
          looks         : Looks    := NIL;
          delayedImages : BOOLEAN  := FALSE): T;

    resetWidth (newWidth: INTEGER;  READONLY before := Point.Origin): Point.T;
    resetLooks (newLooks: Looks;  READONLY before := Point.Origin): Point.T;

    (*------------------------------------------------------- VBT support ---*)

    size (): Point.T;
    paint (dest: VBT.T;  READONLY offset: Point.T;  READONLY clip: Rect.T);
    hiliteBackground (READONLY from, to: Point.T);
    getText (READONLY from, to: Point.T): TEXT;
    translate (READONLY p: Point.T): Location;

    (*------------------------------------------------------ HTML support ---*)

    getTitle (): TEXT;
    locateAnchor (nm: TEXT): Point.T;
    getLink (n: CARDINAL): TEXT;
    setLinkLooks (n: CARDINAL;  hilite: BOOLEAN);
    useImage (nm: TEXT;  image: Rd.T);
    fetchImage (nm: TEXT): Rd.T;
    noteError (msg: TEXT);

  END;

(* Given a hyperpage "p",

   "p.init (s, w, l, d)" initializes "p" by parsing the HTML in "s"
   and breaking lines when necessary to keep the image in a rectangle
   no more than "w" pixels wide.  The resulting image is painted
   with looks "l".  If "d" is "TRUE", inline images are not fetched
   until they're needed by the "paint" method.  Otherwise, inline images
   are fetched during the "init" call.  After "p.init" returns, "s" is
   no longer used.

   "p.resetWidth (w, x)" recomputes the line breaking attempting to keep
   "p"'s bounding box no more than "w" pixels wide.  The value returned
   is the new location in "p"'s image that corresponds to the location "x"
   prior to the call.

   "p.resetLooks (l, x)" recomputes "p"'s bounding box using the new looks
   "l".  The value returned is the new location in "p"'s image that
   corresponds to the location "x" prior to the call.  Changing the values
   of a Looks record after it's attached to a hyperpage may cause
   unpredictable painting.

   "p.size()" returns the southeast corner of "p"'s bounding box.  Note
   that changing "p"'s line breaking width, changing its looks, or
   providing a new image may change its bounding box.

   "p.paint (v, o, c)" paints the piece of "p"'s image contained
   in "clip" (in "p"'s coordinate system) at offset "o" (in "v"'s
   coordinate system) in "v".  Note that if "p" was initialized with
   "delayedImages" "TRUE", "p.fetchImage" may be called during the
   painting.

   "p.hiliteBackground (a, b)" sets "p"'s background hilight to the region
   from "a" to "b".  This call does not automatically repaint "p".

   "p.getText (a, b)" returns the plain text contained in the region from
   "a" to "b".  No HTML markup or images contained in the region are
   returned.

   "p.translate (x)" returns the HTML interpretion corresponding to the
   point "x" in "p"'s image.  See below for a description of the possible
   interpretations.

   "p.getTitle()" returns "p"'s title string.   "NIL" is returned if no
   title was specified in "p"'s underlying HTML.
   
   "p.locateAnchor (n)" returns the location in "p"'s coordinate system
   that corresponds to the anchor named "n".  If there is no such anchor,
   "(-1,-1)" is returned.

   "p.getLink (n)" returns the URL attached to the "n"-th hypertext link
   in "p".  If "n" is greater than the number of links in "p", "NIL" is
   returned.

   "p.setLinkLooks (n, b)" sets the hilight of "p"'s "n"-th link to "b".
   Hilighted links are displayed in the background "linkColor" of "p"'s
   looks.  Non-highlighted links are displayed in the corresponding
   foreground color.  Initially no links are hilighted.

   "p.useImage (n, r)" causes "p" to use the image in "r" wherever it
   it must display an image named "n".  "r" is not used after the
   call returns.  Note that providing a new image may change "p"'s
   bounding box.

   "p.fetchImage (n)" should return a reader containing the bits
   of the image named "n".  If the call returns "NIL", "p" paints
   a default image.  Clients should override this a method since
   the meaning of "n" is usually relative to "p".  The default
   method always returns "NIL".  Once all images of "p" have been
   fetched, "p.fetchImage (NIL)" is called to indicate that whatever
   connection state might be retained is no longer needed.  Note that
   providing a new image may change "p"'s bounding box.

   "p.noteError (x)" is called to report illegal HTML constructs.
   The default method ignores the message "x".
*)

TYPE (* "static" display attributes of the image *)
  Looks = REF RECORD
    fontFamily : TEXT;      (* "Helvetica", "Times-Roman", ... *)
    fontSize   : INTEGER;   (* the point size of "normal" text. *)
    background : PaintOp.T; (* background colors  *)
    textColor  : PaintOp.T; (* normal text colors *)
    linkColor  : PaintOp.T; (* link colors        *)
  END;

TYPE
  LocKind = { Other, OnLink, OnImage, OnMap };

  Location = RECORD
    kind   : LocKind;
    text   : TEXT;
    offset : Point.T;
  END;

(* Each point in an HTML image has some semantic interpretation.  The
   possible interpretations for a point "p" are:

|    "(OnLink, t, o)" means that "p" is on a hypertext link named "t".
|    "(OnImage, t, o)" means that "p" is on an image named "t".
|    "(OnMap, t, o)" means that "p" is over an active map named "t"
|        at offset "o" relative to the map's northwest corner.
|    "(Other, t, o)" means that "p" is somewhere else.
*)
   

END HyperPage.
