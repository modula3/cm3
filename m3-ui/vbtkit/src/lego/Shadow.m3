(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jan 25 14:10:11 PST 1994 by mhb        *)
(*      modified on Tue Jan 18 17:41:34 1994 by harrison   *)
(*      modified on Tue Jun 16 13:08:19 PDT 1992 by muller     *)

MODULE Shadow;

IMPORT PaintOp, ScreenType, VBT;

PROCEDURE New (size : REAL      := 0.5;
               bg   : PaintOp.T := PaintOp.Bg;
               fg   : PaintOp.T := PaintOp.Fg;
               light: PaintOp.T := PaintOp.Fg;
               dark : PaintOp.T := PaintOp.Fg  ): T =
  VAR 
    shadow := NEW(T);
    cs := PaintOp.MakeColorScheme(bg, fg);
  BEGIN
    shadow.bg := cs.bg;
    shadow.fg := cs.fg;
    shadow.bgFg := cs.bgFg;
    shadow.transparentFg := cs.transparentFg;
    shadow.swap := cs.swap;
    shadow.bgTransparent := cs.bgTransparent;
    shadow.bgSwap := cs.bgSwap;
    shadow.fgBg := cs.fgBg;
    shadow.fgTransparent := cs.fgTransparent;
    shadow.fgSwap := cs.fgSwap;
    shadow.transparentBg := cs.transparentBg;
    shadow.transparentSwap := cs.transparentSwap;
    shadow.swapBg := cs.swapBg;
    shadow.swapFg := cs.swapFg;
    shadow.swapTransparent := cs.swapTransparent;
    shadow.size := size;
    shadow.light := light;
    shadow.dark := dark;
    shadow.both := PaintOp.Pair(light, dark);
    shadow.reversed := PaintOp.Pair(dark, light);
    RETURN shadow;
  END New;

PROCEDURE Supported (shadow: T; v: VBT.T): BOOLEAN =
  VAR st: ScreenType.T;
  BEGIN
    st := VBT.ScreenTypeOf(v);
    RETURN (shadow.size > 0.0) AND (st # NIL) AND (st.depth > 1);
  END Supported;

BEGIN
  None := New (0.0);
END Shadow.
