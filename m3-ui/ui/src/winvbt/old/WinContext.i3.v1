(* Copyright (C) 1995, Digital Equipment Corporation                         *)
(* Digital Internal Use Only                                                 *)
(* All rights reserved.                                                      *)
(*                                                                           *)
(* Last modified on Tue Aug  6 11:22:56 PDT 1996 by najork                   *)
(*       Created on Wed Feb 15 15:52:21 PST 1995 by najork                   *)


INTERFACE WinContext;

IMPORT Ctypes, PaintPrivate, Point, VBT, WinDef, WinScreenType;

(* This module provides procedures for modifying device context, and for
   undoing those modifications.  These procedures fall into two categories:
   "PushMumble(hdc, ...)" changes the device context "hdc" to be appropriate 
   for subsequent mumble operations; "Pop(ctxt)" reverts the device context 
   back to its prior state. 

   Push and pop operations must always be paired. The push operation returns
   a context record, which must be passed as a parameter to the corresponding 
   pop operation.  The context record contains the information needed to
   restore the original device context. Using a record type instead of an 
   object type avoids heap allocations.

   A possible further optimization would be to pass a caller-allocated context
   record to push, to avoid copying of the return argument.  *)


TYPE
  T = RECORD
    hdc  : WinDef.HDC;            (* device context *)
    rop2 : Ctypes.int    := 0;    (* binary raster operation code *)
        (* Note: We do not need to save the ternary raster operation code *)
    pen  : WinDef.HPEN   := NIL;
    brush: WinDef.HBRUSH := NIL;
  END;
  (* The following aspects of a device context are not saved 
     (and therefore not restored either):
     FillStyle, Text Color, Background Color, ... *)


PROCEDURE PushTint (hdc: WinDef.HDC;
                    st : WinScreenType.T;
                    op : PaintPrivate.PaintOp): T;
(* Modify "hdc" to be suitable for tint painting. This procedure 
   is the moral equivalent of "XGC.ResolveTintGC" in xvbt. *)


PROCEDURE PushTexture (hdc  : WinDef.HDC;
                       st   : WinScreenType.T;
                       op   : PaintPrivate.PaintOp;
                       pm   : PaintPrivate.Pixmap;
                       delta: Point.T): T;
(* Modify "hdc" to be suitable for texture painting. This procedure 
   is the moral equivalent of "XGC.ResolveTextureGC" in xvbt. *)


PROCEDURE PushFill (hdc  : WinDef.HDC;
                    st   : WinScreenType.T;
                    op   : PaintPrivate.PaintOp;
                    pm   : PaintPrivate.Pixmap;
                    delta: Point.T;
                    wind : VBT.WindingCondition): T;
(* Modify "hdc" to be suitable for filling polygons. This procedure 
   is the moral equivalent of "XGC.ResolveFillGC" in xvbt. *)


PROCEDURE PushStroke (hdc  : WinDef.HDC;
                      st   : WinScreenType.T;
                      op   : PaintPrivate.PaintOp;
                      pm   : PaintPrivate.Pixmap;
                      delta: Point.T;
                      width: CARDINAL;
                      end  : VBT.EndStyle;
                      join : VBT.JoinStyle): T;
(* Modify "hdc" to be suitable for stroking lines. This procedure 
   is the moral equivalent of "XGC.ResolveStrokeGC" in xvbt. *)


PROCEDURE Pop (READONLY ctxt: T);
(* Undo the modifications to the device context "ctxt.hdc". *)

END WinContext.
