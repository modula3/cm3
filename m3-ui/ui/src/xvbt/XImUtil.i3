(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Thu Nov 11 10:44:51 PST 1993 by kalsow   *)

UNSAFE INTERFACE XImUtil;

(* some of the calls from XImUtil.c *)

IMPORT X;

<* EXTERNAL "_XGetBitsPerPixel" *>
  PROCEDURE BitsPerPixel (dpy: X.DisplayStar; depth: X.Int): X.Int;

<* EXTERNAL "_XGetScanlinePad" *>
  PROCEDURE ScanlinePad (dpy: X.DisplayStar; depth: X.Int): X.Int;

END XImUtil.
