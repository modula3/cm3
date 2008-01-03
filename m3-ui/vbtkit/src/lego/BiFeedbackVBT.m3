(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb  1 14:02:48 PST 1993 by mhb        *)
(*      modified on Wed Aug  5 21:38:20 PDT 1992 by meehan     *)
(*      modified on Tue Jun 16 13:09:04 PDT 1992 by muller     *)


MODULE BiFeedbackVBT;

IMPORT FeedbackVBT, Filter, MultiClass, MultiFilter, MultiSplit,
       VBT;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init    := Init;
        normal  := Normal;
        excited := Excited;
      END;

TYPE
  MC = MultiClass.Filter OBJECT
       OVERRIDES
         pred    := Succ;
         succ    := Succ;
         replace := Replace;
       END;

PROCEDURE Init (v: T; f1, f2: FeedbackVBT.T; ch: VBT.T): T =
  BEGIN
    <* ASSERT MultiFilter.Child(f1) = NIL *>
    <* ASSERT MultiFilter.Child(f2) = NIL *>
    EVAL MultiFilter.Replace(f2, ch);
    EVAL MultiFilter.Replace(f1, f2);
    EVAL FeedbackVBT.T.init(v, f1);
    MultiClass.Be(v, NEW(MC));
    IF ch # NIL THEN MultiClass.BeChild(v, ch) END;
    FeedbackVBT.Normal(v);
    RETURN v
  END Init;

PROCEDURE Normal (v: T) =
  VAR
    f1: FeedbackVBT.T := Filter.Child(v);
    f2: FeedbackVBT.T := MultiFilter.Child(f1);
  BEGIN
    FeedbackVBT.SetState(f1, FeedbackVBT.GetState(v));
    FeedbackVBT.Normal(f1);
    FeedbackVBT.SetState(f2, FeedbackVBT.GetState(v));
    FeedbackVBT.Normal(f2)
  END Normal;

PROCEDURE Excited (v: T) =
  VAR
    f1: FeedbackVBT.T := Filter.Child(v);
    f2: FeedbackVBT.T := MultiFilter.Child(f1);
  BEGIN
    FeedbackVBT.SetState(f1, FeedbackVBT.GetState(v));
    FeedbackVBT.Excited(f1);
    FeedbackVBT.SetState(f2, FeedbackVBT.GetState(v));
    FeedbackVBT.Excited(f2)
  END Excited;
  
PROCEDURE Replace (m: MC; <* UNUSED *> ch: VBT.T; new: VBT.T) =
  VAR
    v : T             := m.vbt;
    f1: FeedbackVBT.T := Filter.Child(v);
    f2: FeedbackVBT.T := MultiFilter.Child(f1);
  BEGIN
    EVAL MultiFilter.Replace(f2, new);
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  <* FATAL MultiSplit.NotAChild *>
  VAR
    v : T             := m.vbt;
    f1: FeedbackVBT.T := Filter.Child(v);
    f2: FeedbackVBT.T := MultiFilter.Child(f1);
  BEGIN
    IF ch = NIL THEN
      RETURN MultiSplit.Succ(f2, ch)
    ELSE
      RETURN NIL
    END
  END Succ;

BEGIN
END BiFeedbackVBT.
