(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug  6 11:39:32 PDT 1996 by najork                   *)
(*       Created on Tue Jan 17 16:08:07 PST 1995 by najork                   *)


INTERFACE WinScrnPixmap;

IMPORT PaintPrivate, Rect, ScrnPixmap, WinDef, WinScreenType;

(* The moral equivalent of "PixmapRecord" in xvbt world is 
   "XScrnTpRep.PixmapRec". *)

TYPE
  PixmapRecord = RECORD
    hbmp   : WinDef.HBITMAP;
    domain : Rect.T;  (* domain.north is used to link free records *)
  END;

CONST 
  SolidPixmap: PaintPrivate.Pixmap = -1;


PROCEDURE NewOracle (st: WinScreenType.T): ScrnPixmap.Oracle;

PROCEDURE NewPixmap (         st   : WinScreenType.T;
                              hbmp : WinDef.HBITMAP;
                     READONLY dom  : Rect.T;
                              depth: INTEGER): ScrnPixmap.T;

PROCEDURE PixmapDomain (st: WinScreenType.T; pmId: INTEGER): Rect.T;


PROCEDURE PixmapFromRaw (st: WinScreenType.T; 
                         pm: ScrnPixmap.Raw): WinDef.HBITMAP;

(*
 * Debugging gear
 *)
PROCEDURE DumpPixmap (pm: ScrnPixmap.T);
PROCEDURE DumpPixmapRecord (pmr: PixmapRecord);
PROCEDURE DumpRaw (pm: ScrnPixmap.Raw);

END WinScrnPixmap.
