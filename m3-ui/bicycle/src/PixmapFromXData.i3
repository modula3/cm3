(* Copyright (C) 1991, Digital Equipment Corporation                    *)
(* All rights reserved.                                                 *)
(* See the file COPYRIGHT for a full description.                       *)

(* Last modified on Thu Sep 12 17:59:34 PDT 1991 by msm     *)

INTERFACE PixmapFromXData;

IMPORT Pixmap;

TYPE T = RECORD width, height: CARDINAL; t: TEXT END;

PROCEDURE P(t: T; halftone := FALSE): Pixmap.T;
(* t should be the text for the X bitmap of the given width and height, 
   optionally with all of the 0x and commas removed. *)

PROCEDURE Flip(t: T; halftone := FALSE): Pixmap.T;
(* Return a pixmap just like P(t), but upside-down *)

(* If halftone is true, the pixmap will be ANDed with Gray on a depth-1
   display *)

END PixmapFromXData.
