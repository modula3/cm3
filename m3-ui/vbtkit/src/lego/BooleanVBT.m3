(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Feb  5 17:41:16 PST 1993 by mhb        *)
(*      modified on Thu Jul 30 23:17:14 PDT 1992 by meehan     *)
(*      modified on Tue Jun 16 13:09:03 PDT 1992 by muller     *)
(*      modified on Fri Mar 27 02:30:12 1992 by steveg     *)


MODULE BooleanVBT;

IMPORT ButtonVBT, FeedbackVBT, Filter, HighlightVBT, MultiClass,
       MultiFilter, MultiSplit, VBT;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init := Init;
        callback := Callback;
      END;

TYPE
  MC = MultiClass.Filter OBJECT
       OVERRIDES
         replace := Replace;
         succ    := Succ
       END; 

PROCEDURE Init (v: T; btn: ButtonVBT.T): T =
  BEGIN
    EVAL HighlightVBT.T.init(v, btn);
    (* Wipe out the Action on the button. *)
    btn.action := Action;
    MultiClass.Be(v, NEW(MC));
    WITH ch = MultiFilter.Child(btn) DO
      IF ch # NIL THEN MultiClass.BeChild(v, ch) END
    END;
    RETURN v
  END Init;

PROCEDURE Callback (<* UNUSED *>          v : T;
                    <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END Callback;

PROCEDURE Action (btn: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    WITH f = Filter.Child(btn) DO
      FeedbackVBT.SetState(f, NOT FeedbackVBT.GetState(f));
    END;
    NARROW(VBT.Parent(btn), T).callback(cd);
  END Action;

PROCEDURE Put (v: T; state: BOOLEAN) =
  BEGIN
    WITH btn = Filter.Child(v),
         f   = Filter.Child(btn)   DO
      FeedbackVBT.SetState(f, state)
    END
  END Put;

PROCEDURE Get (v: T): BOOLEAN =
  BEGIN
    WITH btn = Filter.Child(v),
         f   = Filter.Child(btn)   DO
      RETURN FeedbackVBT.GetState(f)
    END
  END Get;

PROCEDURE Replace (m: MC; <* UNUSED *> ch: VBT.T; new: VBT.T) =
  BEGIN
    WITH v   = NARROW(m.vbt, T),
         btn = Filter.Child(v) DO
      EVAL MultiFilter.Replace(btn, new)
    END
  END Replace;
    
PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  <* FATAL MultiSplit.NotAChild *>
  BEGIN
    WITH v   = NARROW(m.vbt, T),
         btn = Filter.Child(v) DO
      RETURN MultiSplit.Succ(btn, ch)
    END
  END Succ;
    
BEGIN
END BooleanVBT.
