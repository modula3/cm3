(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Fri Jul 16 11:43:28 PDT 1993 by msm      *)
(*      modified on Mon Feb 24 14:01:27 PST 1992 by muller   *)
(*      modified on Sun Nov 10 22:39:52 PST 1991 by gnelson  *)
<*PRAGMA LL*>

MODULE ColorMonster EXPORTS Main;
IMPORT Trestle, HVSplit, Axis, HVBar, VBT, HighlightVBT,
  BorderedVBT, Pixmap, PaintOp, ScrnPaintOp, 
  Palette, ScreenType, Word, TrestleComm, Fmt, TextVBT;

<*FATAL ANY*>

PROCEDURE NewOp(pix: INTEGER): PaintOp.T =
  BEGIN
    RETURN Palette.FromOpClosure(NEW(MyOpClosure, pix := pix))
  END NewOp;

TYPE MyOpClosure = Palette.OpClosure 
  OBJECT pix: INTEGER
  OVERRIDES apply := MyOpApply END;

PROCEDURE MyOpApply(cl: MyOpClosure; st: ScreenType.T): ScrnPaintOp.T =
  VAR pix := cl.pix MOD Word.Shift(1, st.depth);
  BEGIN
    TRY
      RETURN st.op.opaque(pix)
    EXCEPT
      ScrnPaintOp.Failure, TrestleComm.Failure => 
        RETURN Palette.ResolveOp(st, PaintOp.Bg)
    END
  END MyOpApply;

PROCEDURE New(lo, hi: INTEGER; hv: Axis.T): VBT.T =
  BEGIN
    IF hi - lo = 1 THEN
      RETURN BorderedVBT.New(TextVBT.New(Fmt.Int(lo), 
        bgFg := PaintOp.MakeColorQuad(NewOp(lo), PaintOp.Fg)))
    ELSE
      WITH vh = Axis.Other[hv], mid = (lo + hi) DIV 2 DO
        RETURN HVSplit.Cons(hv,
          New(lo, mid, vh),
          HVBar.New(),
          New(mid, hi, vh))
      END
    END
  END New;
  
VAR count := 256;
    v := BorderedVBT.New(HighlightVBT.New(New(0, count, Axis.T.Hor)), 
      BorderedVBT.Default, PaintOp.BgFg, Pixmap.Gray);

BEGIN
Trestle.Install(v);
Trestle.AwaitDelete(v)
END ColorMonster.
