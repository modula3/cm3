(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 14:01:20 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* A "Cursor.T" is a screen-independent specification of a cursor shape. 
   The call "VBT.SetCursor(v, cs)" sets the cursor of "v" to
   be "cs". 

   The locking level is "LL.sup <= VBT.mu" for all of the procedures
   in this interface. *)

INTERFACE Cursor;

IMPORT Pixmap, Point;

TYPE T = RECORD cs: INTEGER END; Predefined = [0..2];

CONST 
  DontCare = T{0};
  TextPointer = T{1};
  NotReady = T{2};

(* You should set "Cursor.DontCare" when you don't care about the cursor
   shape; "Cursor.TextPointer" when the cursor is to be used for editing
   text, and "Cursor.NotReady" to indicate that the application is not
   receptive to user input.  *)

TYPE Raw = RECORD
    plane1, plane2: Pixmap.Raw;
    hotspot: Point.T;
    color1, color2, color3: RGB;
  END;
  BW = {UseBg, UseFg, UseIntensity};
  RGB = RECORD 
    r, g, b: REAL; 
    gray := -1.0; 
    bw := BW.UseIntensity 
  END;

(* A "Raw" represents a cursor with explicit offset, 
   bitmaps, and colors.    

   The "plane1" and "plane2" are depth-1 pixmaps.  They must
   have the same bounding rectangle, and the hotspot must lie
   within the bounding rectangle or on its east or south edge.
   If the hotspot is illegal, it will be moved to the closest
   legal position.

   The cursor's hotspot is kept on top of the mouse's location on the
   screen.  The cursor's image tracks the mouse relative to the hotspot.
   For example, if the hotspot is (0, 0), the (0, 0) bit of the cursor's
   image will be located over the mouse's location.  The remainder of
   the cursor will appear to the south and east.

   The color of each pixel in the cursor's image is determined from the
   corresponding bits in "plane1" and "plane2" ("p1" and "p2"):

| p1 = 0, p2 = 0  => `transparent`
| p1 = 0, p2 = 1  => color1
| p1 = 1, p2 = 0  => color2
| p1 = 1, p2 = 1  => color3

   The colors for the cursor are matched as closely as possible to the
   selection of cursor colors that the screentype supports.  If the
   screentype allows only two colors for the cursor, then the pixels that
   would have been "color3" will be "color1".  The "gray" and "bw"
   values control the color on gray-scale and monochrome displays, 
   according to the same rule used in "PaintOp.FromRGB".  *)
     
PROCEDURE FromRaw(READONLY r: Raw): T;
(* Return a cursor that looks like "r" on all screens. *)

(* If the screentype does not support "r"'s colors or size, "FromRaw"
   will clip or convert colors as necessary.  On a screentype that does
   not allow user-defined cursors, the cursor returned by "FromRaw"
   will behave like "DontCare".  *)

PROCEDURE FromName(READONLY names: ARRAY OF TEXT): T;
(* Return the first available cursor of those named in the array "names". *)

(* The entries of "names" are cursor names as specified in the
   "ScrnCursor" interface, possibly containing wild card characters.
   On any particular screentype, "FromName(names)" iterates through
   "names" in order and returns an arbitrary match from the first name
   that matches anything.  If no name has any matches, it returns
   "DontCare".

   Standard X screentypes support the cursors named in {\it X Window
   System} by Scheifler et.  al. \cite{XSpec} Appendix B. Therefore, for
   example,

| FromName(ARRAY OF TEXT{"XC_arrow"})

   returns a cursor that behaves like the X arrow cursor on 
   X screentypes, and like "DontCare" on screentypes 
   that have no cursor named "XC_arrow".  *)

END Cursor.
