(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Mon Nov  4 14:10:54 PST 1996 by najork                   *)
(*       Created on Mon Jun 26 09:36:11 PDT 1995 by najork                   *)


INTERFACE WinPaint;

IMPORT Batch, Trestle, VBT, WinDef;

PROCEDURE PaintBatch (self: Trestle.T; v: VBT.T; ba: Batch.T; hdc: WinDef.HDC);

END WinPaint.
