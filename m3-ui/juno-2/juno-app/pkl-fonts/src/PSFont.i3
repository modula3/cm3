(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Dec 15 13:43:19 PST 1994 by heydon                   *)

INTERFACE PSFont;

(* Types for PostScript font name and metric data *)

IMPORT TextRefTbl, JunoRect, JunoValue;

TYPE
  Data = REF RECORD
    fontTbl: TextRefTbl.T;
    metricTbl: TextRefTbl.T;
  END;

  (* "fontTbl" maps PostScript font names concatenated with the size of
     the font to information on the corresponding X font, of type "XInfo"
     below.

     "metricTbl" maps PostScript font names (without point sizes) to that
     font's metric data, of type "Metric" below. *)

  XInfo = REF RECORD
    name: TEXT;
    ptSize: JunoValue.Real;
  END;

  (* An "XInfo" record constains information for a font face at a particular
     font size. If "x: XInfo", then "x.name" is the full name of the X font,
     and "x.ptSize" is the point size of the corresponding PostScript font
     that will cause capital letters in the screen (X) and printer
     (PostScript) versions of the font to have roughly the same height. *)

  BBox = JunoRect.T;			         (* font bounding box *)
  Code = [0..255];
  CharMapped = ARRAY Code OF BOOLEAN;            (* character data valid? *)
  CharWidth = ARRAY Code OF JunoValue.Real;	 (* array of char widths *)
  CharBBox = ARRAY Code OF REF BBox;             (* array of char bboxes *)

  Metric = REF RECORD
    bbox: BBox;
    mapped := CharMapped{ FALSE, .. };
    width: CharWidth;
    charBB: CharBBox;
  END;

  (* All fields of a "PSFont.Metric" are for a 1 pt font. Therefore, they
     must be scaled by the size of the current font before they are used.

     "bbox" is the bounding box of the font: if all characters in the font are
     rendered at the same origin point, it is the smallest rectangle
     containing them in the coordinate system whose origin is the origin
     point, and whose coordinate axes increase to the right in the "x"
     direction and up in the "y" direction.

     "mapped[i]" indicates whether or not metric data for character "i" are
     valid. If "mapped[i] = FALSE", then no metric data was specified for
     character "i", and the values of "width[i]" and "charBB[i]" are
     undefined.

     "width[i]" is the width of the character with code "i", that is, the
     amount the reference point should be advanced when this character is
     rendered.

     "charBB[i]" is the bounding box for the character with code "i". The
     bounding box is the smallest rectangle enclosing the ink for the
     character when it is rendered with its reference point at the origin. If
     the bounding box is empty (i.e., if rendering character "i" requires no
     ink), then "charBB[i] = NIL". *)

END PSFont.
