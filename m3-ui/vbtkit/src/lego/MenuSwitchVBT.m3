(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 15 15:50:58 PDT 1993 by meehan     *)
(*      modified on Mon Feb  1 21:37:18 PST 1993 by mhb        *)
(*      modified on Tue Jun 16 13:08:37 PDT 1992 by muller     *)

MODULE MenuSwitchVBT;

IMPORT ButtonVBT, FeedbackVBT, MenuBtnVBT, MultiFilter,
       MultiClass, SwitchVBT, VBT;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init     := Init;
        pre      := SwitchVBT.Pre;
        post     := SwitchVBT.Post;
        cancel   := SwitchVBT.Cancel;
      END;

TYPE 
  MC = SwitchVBT.MC BRANDED OBJECT END;

PROCEDURE Init (v: T; f: FeedbackVBT.T): T =
  BEGIN
    EVAL MenuBtnVBT.T.init(v, f, Action);
    MultiClass.Be(v, NEW(MC));
    WITH ch = MultiFilter.Child(f) DO
      IF ch # NIL THEN MultiClass.BeChild(v, ch) END;
    END;
    RETURN v
  END Init;

PROCEDURE Action (selfAsButtonVBT: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    NARROW (selfAsButtonVBT, T).callback (cd)
  END Action;

BEGIN
END MenuSwitchVBT.




