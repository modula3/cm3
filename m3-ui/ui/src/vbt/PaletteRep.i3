(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:57:31 PST 1992 by muller   *)
(*      modified on Fri Sep 13  1:40:23 PDT 1991 by msm      *)
(*      modified on Mon Sep  9 21:33:26 PDT 1991 by gnelson  *)
(*      modified on Fri Aug  3 16:59:37 PDT 1990 by steveg   *)
<*PRAGMA LL*>

INTERFACE PaletteRep;

(* Each entry in a palette can be NIL, or a distinguished value to indicate
   that it is under evaluation.  Each palette will be allocated large enough
   to contain every entry, even if it isn't resolved. *)
   
IMPORT ScrnPaintOp, ScrnFont, ScrnCursor, ScrnPixmap;

VAR
  noOp: ScrnPaintOp.T;
  noFont: ScrnFont.T;
  noCursor: ScrnCursor.T;
  noPixmap:ScrnPixmap.T;

END PaletteRep.
