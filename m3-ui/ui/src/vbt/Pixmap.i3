(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 24 14:01:18 PST 1992 by muller                   *)
<*PRAGMA LL*>

(* A "Pixmap.T" is a screen-independent specification of a pixmap.  
   Many procedures interpret "Pixmap.Ts" as textures, by tiling the
   plane with translated copies of the pixmap. There are three
   predefined pixmaps:

   The locking level is "LL.sup <= VBT.mu" for all of the procedures
   in this interface. *)

INTERFACE Pixmap;

TYPE T = RECORD pm: INTEGER END; Predefined = [0..2];

CONST 
  Solid = T{0};
  Empty = T{1};
  Gray = T{2}; 
  
(* "Solid" represents a pixmap of all ones. 
   "Empty" represents a pixmap of all zeros. 
   "Gray" represents a checkerboard of ones and zeros. 
   
   The domains of these pixmaps may vary from screentype to
   screentype, but they will always be non-empty.
   
   When used on a screentype "st", they will have type "st.bits"
   (see the "PaintOp" interface). *)

TYPE Raw <: ROOT;

(* A "Pixmap.Raw" represents a pixmap as a packed array of
   pixels.  The "ScrnPixmap" interface reveals the representation. *)

PROCEDURE FromBitmap(bits: Raw): T;
(* Return a pixmap that looks like "bits" on all screens. *)

(* "FromBitmap" causes a checked runtime error if the depth of 
   "bits" is not one.  On a screentype "st", it will have type
   "st.bits". *)

END Pixmap.
