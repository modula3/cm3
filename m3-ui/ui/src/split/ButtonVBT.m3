(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:53:01 PST 1992 by muller   *)
(*      modified on Sun Nov 10 18:20:22 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:27:59 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE ButtonVBT;

IMPORT VBT, Filter, Rect, HighlightVBT, BtnVBTClass, Split, 
  VBTClass, Axis, PackSplit, PaintOp;

FROM VBT IMPORT ClickType;

REVEAL 
  T = BtnVBTClass.T BRANDED OBJECT
    highlighter: HighlightVBT.T := NIL
  OVERRIDES 
    mouse := Mouse;
    position := Position;
    shape := Shape;
    pre := Pre;
    post := Post;
    cancel := Post; (*sic*)
    init := Be
  END;
  
PROCEDURE Be(v: T; ch: VBT.T; p: Proc; ref: REFANY := NIL): T RAISES {} =
  BEGIN
    v.action := p;
    IF ref # NIL THEN VBT.PutProp(v, ref) END;
    EVAL Filter.T.init(v, ch);
    RETURN v
  END Be;

PROCEDURE New(
    ch: VBT.T; 
    action: Proc; 
    ref: REFANY := NIL): T RAISES {} =
  BEGIN
    RETURN Be(NEW(T), ch, action, ref)
  END New;
  
PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) RAISES {} =
  BEGIN
    Filter.T.mouse(v, cd);
    IF cd.clickType = ClickType.FirstDown THEN
      v.ready := TRUE;
      v.armed := TRUE;
      v.pre();
      VBT.SetCage(v, VBT.InsideCage)
    ELSE
      IF (cd.clickType = ClickType.LastUp) AND NOT cd.cp.gone AND v.armed
      THEN
        IF NOT v.ready THEN v.pre() END;
        v.action(v, cd);
        v.post()
      ELSIF v.ready THEN
        v.cancel()
      END;
      v.ready := FALSE;
      v.armed := FALSE
    END
  END Mouse;

PROCEDURE Position(v: T; READONLY cd: VBT.PositionRec) RAISES {} =
  BEGIN
    Filter.T.position(v, cd);
    IF v.armed THEN
      IF cd.cp.gone THEN
        IF v.ready THEN
          v.cancel();
          v.ready := FALSE
        END;
        VBT.SetCage(v, VBT.GoneCage)
      ELSE
        IF NOT v.ready THEN
          v.ready := TRUE;
          v.pre()
        END;
        VBT.SetCage(v, VBT.InsideCage)
      END
    ELSE
      VBT.SetCage(v, VBT.EverywhereCage)
    END
  END Position;

PROCEDURE Pre(v: T) RAISES {} =
  BEGIN
    v.highlighter := HighlightVBT.Find(v);
    HighlightVBT.Invert(v.highlighter, VBT.Domain(v), 99999)
  END Pre;

PROCEDURE Post(v: T) RAISES {} =
  BEGIN
    HighlightVBT.SetRect(v.highlighter, Rect.Empty, 0);
    v.highlighter := NIL
  END Post;

PROCEDURE Shape(v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange RAISES {} =
  BEGIN
    WITH sh = VBTClass.GetShape(Filter.Child(v), ax, n) DO
      RETURN VBT.SizeRange{lo := sh.lo, hi := sh.lo+1, pref := sh.lo}
    END
  END Shape;

PROCEDURE MenuBar(
  ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8, ch9: VBT.T := NIL;
  op: PaintOp.T := PaintOp.Bg)
  : PackSplit.T RAISES {} =
  VAR res := NEW(Bar);
  BEGIN
    EVAL PackSplit.T.init(res, op := op);
    Split.AddChild(res, ch0, ch1, ch2, ch3, ch4, ch5, ch6, ch7, ch8, ch9);
    RETURN res
  END MenuBar;

PROCEDURE BarShape(v: Bar; ax: Axis.T; n: CARDINAL): VBT.SizeRange RAISES {} =
  VAR sh := PackSplit.T.shape(v, ax, n); BEGIN
    IF ax # PackSplit.AxisOf(v) THEN sh.hi := sh.lo+1; sh.pref := sh.lo END;
    RETURN sh
  END BarShape;

TYPE Bar = PackSplit.T OBJECT OVERRIDES shape := BarShape END;

BEGIN END ButtonVBT.
    
