(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Feb 15 16:40:07 PST 1993 by mhb        *)
(*      modified on Fri Feb 12 15:55:38 PST 1993 by meehan     *)
(*      modified on Tue Jun 16 13:08:08 PDT 1992 by muller     *)
(*      modified on Fri Jul 26 15:36:28 PDT 1991 by steveg     *)

MODULE TrillSwitchVBT;

IMPORT AutoRepeat, ButtonVBT, FeedbackVBT, Filter, MultiClass,
       MultiFilter, Point, Rect, SwitchVBT, Trestle, TrestleComm, VBT;

REVEAL
  T = Public BRANDED OBJECT
        firstDown: VBT.MouseRec;
        repeater : Repeater;
        trsl     : Trestle.T := NIL
      OVERRIDES
        init     := Init;
        callback := Callback;
        pre      := Pre;
        post     := Post;
        cancel   := Cancel;
        mouse    := Mouse;
        reshape  := Reshape;
        rescreen := Rescreen
      END;

TYPE
  MC = SwitchVBT.MC OBJECT END;

TYPE
  Repeater = AutoRepeat.T OBJECT
               v: T
             OVERRIDES
               repeat    := Repeat;
               canRepeat := CanRepeat
             END;

PROCEDURE Init (v: T; f: FeedbackVBT.T): T =
  BEGIN
    EVAL ButtonVBT.T.init(v, f, Action);
    MultiClass.Be(v, NEW(MC));
    WITH ch = MultiFilter.Child(f) DO
      IF ch # NIL THEN MultiClass.BeChild(v, ch) END;
    END;
    v.repeater := NEW(Repeater, v := v).init();
    RETURN v
  END Init;

PROCEDURE Callback (<* UNUSED *>          v : T;
                    <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END Callback;

PROCEDURE Action(
    <* UNUSED *> selfAsButtonVBT: ButtonVBT.T; 
    <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END Action;

PROCEDURE Repeat (r: Repeater) =
  BEGIN
    LOCK VBT.mu DO
      WITH v = r.v DO v.firstDown.time := 0; v.callback (v.firstDown) END
    END
  END Repeat;

PROCEDURE Rescreen (v: T; READONLY cd: VBT.RescreenRec) =
  BEGIN
    v.trsl := Trestle.ScreenOf(v, Point.Origin).trsl;
    Public.rescreen(v, cd)
  END Rescreen;

PROCEDURE Reshape (v: T; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    IF v.trsl = NIL AND NOT Rect.IsEmpty(cd.new) THEN
      v.trsl := Trestle.ScreenOf(v, Point.Origin).trsl
    END;
    Public.reshape(v, cd)
  END Reshape;

PROCEDURE CanRepeat (r: Repeater): BOOLEAN =
  <* FATAL TrestleComm.Failure *>
  BEGIN
    LOCK VBT.mu DO
      WITH trsl = r.v.trsl DO
        IF trsl = NIL THEN
          (* somehow uninstalled; stop trilling *)
          RETURN FALSE
        ELSE
         (* an installed vbt; continue trilling as long
           as there are no pending mouse clicks or keystrokes
           on their way to any top-level window installed by 
           the trsl in wich v is installed. *)
          RETURN Trestle.AllCeded(trsl)
        END
      END
    END
  END CanRepeat;

PROCEDURE Pre (v: T) =
  BEGIN
    FeedbackVBT.Excited(Filter.Child(v));
    v.callback(v.firstDown);
    AutoRepeat.Start(v.repeater)
  END Pre;

PROCEDURE Post (v: T) =
  BEGIN
    AutoRepeat.Stop(v.repeater);
    FeedbackVBT.Normal (Filter.Child (v));
  END Post;

PROCEDURE Cancel (v: T) =
  BEGIN
    AutoRepeat.Stop(v.repeater);
    FeedbackVBT.Normal(Filter.Child(v));
  END Cancel;

PROCEDURE Mouse (v: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN v.firstDown := cd END;
    ButtonVBT.T.mouse(v, cd)
  END Mouse;

BEGIN
END TrillSwitchVBT.
