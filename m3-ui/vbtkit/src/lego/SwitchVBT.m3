(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Jan 29 23:08:04 PST 1993 by mhb        *)
(*      modified on Sun Aug  9 19:51:36 PDT 1992 by meehan     *)
(*      modified on Tue Jun 16 13:08:11 PDT 1992 by muller     *)
(*      modified on Fri Jul 26 15:36:28 PDT 1991 by steveg     *)

MODULE SwitchVBT;

IMPORT ButtonVBT, FeedbackVBT, Filter, MultiClass, MultiFilter, VBT;

REVEAL
  T = Public BRANDED OBJECT
      OVERRIDES
        init     := Init;
        callback := Callback;
        pre      := Pre;
        post     := Post;
        cancel   := Cancel;
      END;

  MC = MultiClass.Filter BRANDED OBJECT
       OVERRIDES
         succ    := Succ;
         pred    := Succ;
         replace := Replace;
       END;

PROCEDURE Init (v: T; f: FeedbackVBT.T): T =
  BEGIN
    EVAL ButtonVBT.T.init (v, f, Action);
    MultiClass.Be (v, NEW(MC));
    MultiClass.BeChild (v, MultiFilter.Child (f));
    RETURN v
  END Init;

PROCEDURE Callback (<* UNUSED *>          v : T;
                    <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END Callback;

PROCEDURE Action (         selfAsButtonVBT: ButtonVBT.T;
                  READONLY cd             : VBT.MouseRec ) =
  BEGIN
    NARROW (selfAsButtonVBT, T).callback (cd)
  END Action;

PROCEDURE Pre (v: ButtonVBT.T) =
  BEGIN
    FeedbackVBT.Excited (Filter.Child (v));
  END Pre;

PROCEDURE Post (v: ButtonVBT.T) =
  BEGIN
    FeedbackVBT.Normal (Filter.Child (v));
  END Post;

PROCEDURE Cancel (v: ButtonVBT.T) =
  BEGIN
    FeedbackVBT.Normal (Filter.Child(v));
  END Cancel;

PROCEDURE Replace (m: MC; <* UNUSED *> ch: VBT.T; new: VBT.T) =
  BEGIN
    WITH switch = m.vbt,
         feedback = Filter.Child (switch) DO
      EVAL MultiFilter.Replace (feedback, new);
    END
  END Replace;

PROCEDURE Succ (m: MC; ch: VBT.T): VBT.T =
  BEGIN
    WITH switch = m.vbt,
         feedback = Filter.Child (switch) DO
      IF ch = NIL THEN
        RETURN MultiFilter.Child (feedback);
      ELSE
        RETURN NIL
      END;
    END
  END Succ;

BEGIN
END SwitchVBT.
