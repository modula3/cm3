(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug  6 11:28:26 PDT 1996 by najork                   *)
(*       Created on Fri Jan 20 21:12:26 PST 1995 by najork                   *)


INTERFACE WinScreenTypePrivate;

IMPORT WinScrnPaintOp, WinScrnPixmap;

FROM WinScreenType IMPORT T, Public;

TYPE
  Private = Public OBJECT
    optable: REF ARRAY OF WinScrnPaintOp.OpRecord;
      (* dynamic array of registered paint operations *)
    opcount: CARDINAL := 0;
      (* numbers of entries in optable. *)
    pmtable: REF ARRAY OF WinScrnPixmap.PixmapRecord;
      (* dynamic array of registered pixmaps *)
    pmcount: CARDINAL := 0;
      (* number of entries in pmtable *)
    pmfree: INTEGER := -1;
      (* index of first free list entry in pmtable below pmcount;
         negative if none *)
  END;
(*
 * In the xvbt version, there is also a field "empty" that caches the id
 * (the index into st.pmtable, not the XID) of the empty pixmap 
 * ("Pixmap.Empty"). This field is updated by "XScrnPxmp.BuiltIn", and 
 * accessed by various "XGC.ResolveTextureGC", "XGC.ResolveFillGC", and 
 * "XGC.ResolveStrokeGC".  Its main purpose seems to be performance 
 * optimization.
 *)


REVEAL T <: Private;

END WinScreenTypePrivate.
