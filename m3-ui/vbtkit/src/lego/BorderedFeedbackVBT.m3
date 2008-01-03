(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Feb  4 10:53:25 PST 1993 by mhb        *)
(*      modified on Fri Jul 31  1:31:48 PDT 1992 by meehan     *)
(*      modified on Tue Jun 16 13:09:01 PDT 1992 by muller     *)

MODULE BorderedFeedbackVBT;

IMPORT BorderedVBT, FeedbackVBT, Filter, HighlightVBT, MultiClass, PaintOp,
       Pixmap, Rect, TranslateVBT, VBT;

REVEAL
  T = Public BRANDED OBJECT
        hl: HighlightVBT.T;
      OVERRIDES
        init    := Init;
        normal  := Normal;
        excited := Excited;
      END;

TYPE
  MC = MultiClass.Filter BRANDED OBJECT
       OVERRIDES
         succ    := Succ;
         pred    := Succ;
         replace := Replace;
       END;

PROCEDURE Init (v: T; ch: VBT.T; size := 2.0; op := PaintOp.BgFg):
  T =
  BEGIN
    v.hl := HighlightVBT.New(ch);
    EVAL FeedbackVBT.T.init(v, TranslateVBT.New(v.hl));
    MultiClass.Be(v, NEW(MC));
    IF ch # NIL THEN MultiClass.BeChild(v, ch) END;
    FeedbackVBT.Normal(v);
    RETURN v
  END Init;

PROCEDURE Normal (v: T) =
  VAR pm: Pixmap.T;
  BEGIN
    IF FeedbackVBT.GetState(v) THEN
      HighlightVBT.SetTexture(v.hl, Pixmap.Solid);
      HighlightVBT.SetRect(v.hl, Rect.Full, LAST(CARDINAL))
    ELSE
      HighlightVBT.SetRect(v.hl, Rect.Empty, 0)
    END
  END Normal;

PROCEDURE Excited (v: T) =
  BEGIN
    HighlightVBT.SetTexture(v.hl, Pixmap.Gray);
    HighlightVBT.SetRect(v.hl, Rect.Full, LAST(CARDINAL))
  END Excited;

PROCEDURE Replace (m: MC; <* UNUSED *> ch: VBT.T; new: VBT.T) =
  BEGIN
    EVAL Filter.Replace(NARROW(m.vbt, T).hl, new)
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  BEGIN
    IF ch = NIL THEN
      RETURN Filter.Child(NARROW(m.vbt, T).hl)
    ELSE
      RETURN NIL
    END
  END Succ;

BEGIN
END BorderedFeedbackVBT.


(* 
MODULE BorderedFeedbackVBT;

IMPORT BorderedVBT, FeedbackVBT, Filter, MultiClass, PaintOp,
       Pixmap, VBT;

REVEAL
  T = Public BRANDED OBJECT
        op: PaintOp.T
      OVERRIDES
        init    := Init;
        normal  := Normal;
        excited := Excited;
      END;

TYPE
  MC = MultiClass.Filter BRANDED OBJECT
       OVERRIDES
         succ    := Succ;
         pred    := Succ;
         replace := Replace;
       END;

PROCEDURE Init (v: T; ch: VBT.T; size := 2.0; op := PaintOp.BgFg):
  T =
  BEGIN
    EVAL FeedbackVBT.T.init(
           v, BorderedVBT.New(ch, size, op, Pixmap.Solid));
    v.op := op;
    MultiClass.Be(v, NEW(MC));
    IF ch # NIL THEN MultiClass.BeChild(v, ch) END;
    FeedbackVBT.Normal(v);
    RETURN v
  END Init;

PROCEDURE Normal (v: T) =
  VAR pm: Pixmap.T;
  BEGIN
    IF FeedbackVBT.GetState(v) THEN
      pm := Pixmap.Solid;
    ELSE
      pm := Pixmap.Empty;
    END;
    BorderedVBT.SetColor(Filter.Child(v), v.op, pm)
  END Normal;

PROCEDURE Excited (v: T) =
  BEGIN
    BorderedVBT.SetColor(Filter.Child(v), v.op, Pixmap.Gray);
  END Excited;

PROCEDURE Replace (m: MC; <* UNUSED *> ch: VBT.T; new: VBT.T) =
  BEGIN
    WITH feedback = m.vbt,
         shadow   = Filter.Child(feedback) DO
      EVAL Filter.Replace(shadow, new)
    END
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  BEGIN
    WITH feedback = m.vbt,
         shadow   = Filter.Child(feedback) DO
      IF ch = NIL THEN
        RETURN Filter.Child(shadow)
      ELSE
        RETURN NIL
      END
    END
  END Succ;

BEGIN
END BorderedFeedbackVBT.

*)
