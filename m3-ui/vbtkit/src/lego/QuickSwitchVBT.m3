(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb  1 21:37:01 PST 1993 by mhb        *)
(*      modified on Sat Aug  1  0:26:55 PDT 1992 by meehan     *)
(*      modified on Tue Jun 16 13:08:28 PDT 1992 by muller     *)

MODULE QuickSwitchVBT;

IMPORT ButtonVBT, FeedbackVBT, MultiClass, MultiFilter, QuickBtnVBT,
       SwitchVBT, VBT;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init     := Init;
        callback := Callback;
      END;

TYPE MC = SwitchVBT.MC BRANDED OBJECT END;

PROCEDURE Init (v: T; f: FeedbackVBT.T): T =
  BEGIN
    EVAL QuickBtnVBT.T.init(v, f, Action);
    MultiClass.Be (v, NEW(MC));
    WITH ch = MultiFilter.Child(f) DO
      IF ch # NIL THEN MultiClass.BeChild(v, ch) END;
    END;
    RETURN v
  END Init;

PROCEDURE Callback (<* UNUSED *>          v : T;
                    <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END Callback;

PROCEDURE Action (selfAsButtonVBT: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    NARROW(selfAsButtonVBT, T).callback(cd)
  END Action;

BEGIN
END QuickSwitchVBT.
