(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Jan 25 18:15:22 PST 1993 by msm      *)
(*      modified on Tue Mar 10 19:05:26 1992 by steveg   *)
(*      modified on Mon Feb 24 13:57:29 PST 1992 by muller   *)
(*      modified on Tue Oct 22 22:45:40 PDT 1991 by gnelson  *)

<*PRAGMA LL*>

INTERFACE PlttFrnds;

(* Each entry in a palette can be NIL, or a distinguished value to indicate
   that it is under evaluation.  Each palette will be allocated large enough
   to contain every entry, even if it isn't resolved. *)
   
IMPORT ScrnPaintOp, ScrnFont, ScrnCursor, ScrnPixmap, Palette, PaintOp, Font,
       Pixmap, Cursor;

TYPE 
  Context = MUTEX OBJECT
    ops: REF ARRAY OF Palette.OpClosure := NIL;
    nextOp := NUMBER(PaintOp.Predefined);
    fonts: REF ARRAY OF Palette.FontClosure := NIL;
    nextFont := NUMBER(Font.Predefined);
    cursors: REF ARRAY OF Palette.CursorClosure := NIL;
    nextCursor := NUMBER(Cursor.Predefined);
    pixmaps: REF ARRAY OF Palette.PixmapClosure := NIL;
    nextPixmap := NUMBER(Pixmap.Predefined)
  END;

VAR
  con: Context;

  noOp: ScrnPaintOp.T;
  noFont: ScrnFont.T;
  noCursor: ScrnCursor.T;
  noPixmap:ScrnPixmap.T;

END PlttFrnds.
