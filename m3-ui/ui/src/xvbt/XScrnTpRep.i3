(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Wed Oct 16 14:08:24 PDT 1996 by msm     *)
(*      modified on Thu Mar 10 19:16:30 PST 1994 by gnelson *)
(*      modified on Fri May  7 16:47:07 PDT 1993 by mjordan *)
(* modified on Mon Feb 24 13:59:50 PST 1992 by muller *)

<*PRAGMA LL*>

UNSAFE INTERFACE XScrnTpRep;

IMPORT X, PaintPrivate, Rect, Ctypes, XScreenType;

REVEAL
  XScreenType.T <: Public;

TYPE
  Public = XScreenType.Public OBJECT
             optable: REF ARRAY OF OpRecord;
             pmtable: REF ARRAY OF PixmapRecord;
             empty                                := 0
           END;

  (* If pm is a ScrnPixmap.T whose screentype is st, a T, then pm.id is
     either SolidPixmap, or pm.id is the index into st.pmtable of the
     PixmapRecord describing pm, unless st.bits = st, in which case
     SolidPixmap - pm.id is the index in st.bits.pmtable. *)

  OpRecord = RECORD
               function, fill_style              : Ctypes.int;
               plane_mask, foreground, background: Ctypes.unsigned_long;
             END;

  PixmapRecord = RECORD
                   pixmap    : X.Drawable;
                   isLazy    : BOOLEAN;
                   domain    : Rect.T;
                   depth     : INTEGER;
                   generation: INTEGER;
                 END;

CONST SolidPixmap: PaintPrivate.Pixmap = -1;

END XScrnTpRep.

