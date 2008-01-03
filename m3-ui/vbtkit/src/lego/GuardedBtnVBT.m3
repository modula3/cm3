(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Nov 11 23:51:43 PST 1993 by mhb    *)
(*      modified on Thu Jul 30 23:26:11 PDT 1992 by meehan *)
(*      modified on Tue Jun 16 13:08:48 PDT 1992 by muller *)

MODULE GuardedBtnVBT;

IMPORT BtnVBTClass, ButtonVBT, Cursor, Filter, MultiClass, PaintOp,
       Pixmap, Point, ReactivityVBT, Rect, SwitchVBT, VBT,
       VBTClass, VBTKitResources;
         
VAR
  guardTexture           : Pixmap.T;
  guardTextureInitialized           := FALSE;

REVEAL
  T = Public BRANDED OBJECT
        guarded     : BOOLEAN   := TRUE;
        guardedOnPre: BOOLEAN;
      OVERRIDES
        init     := Init;
        pre      := Pre;
        post     := Post;
        cancel   := Cancel;
        callback := Callback;
        position := Position;
      END;

PROCEDURE Init (v         : T;
                ch        : VBT.T;
                colors    : PaintOp.ColorScheme := NIL): T= 
  VAR
    feedback := NEW (ReactivityVBT.T,
                     paintDormant := PaintDormant).init (
                  ch, colors);
  BEGIN
    EVAL ButtonVBT.T.init (v, feedback, Action);
    MultiClass.Be (v, NEW(SwitchVBT.MC));
    IF ch # NIL THEN MultiClass.BeChild(v, ch) END;
    SetGuard (v, TRUE);
    RETURN v
  END Init;

PROCEDURE Callback ( <* UNUSED *> v: T;  
                     <* UNUSED *> READONLY cd: VBT.MouseRec) =
  BEGIN
  END Callback;

PROCEDURE Action (selfAsButtonVBT: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    NARROW (selfAsButtonVBT, T).callback (cd)
  END Action;

PROCEDURE Pre (v: T) =
  BEGIN
    v.guardedOnPre := v.guarded;
    SetGuard (v, FALSE); 
  END Pre;

PROCEDURE Post (v: T) =
  BEGIN
    IF NOT v.guardedOnPre THEN
      (* Restore guard on second LastUp *)
      SetGuard (v, TRUE)
    END
  END Post;

PROCEDURE Cancel (v: T) =
  BEGIN
   (* Restore guard on chord-cancel *)
    SetGuard(v, TRUE);
  END Cancel;

PROCEDURE Position (v: T; READONLY cd: VBT.PositionRec) =
  BEGIN
    IF NOT v.guarded AND NOT v.armed AND cd.cp.gone THEN
      (* Restore guard when moving out of v before next FirstDown *)
      SetGuard(v, TRUE)
    END;
    ButtonVBT.T.position(v, cd);
  END Position;

PROCEDURE SetGuard (v: T; fg: BOOLEAN) =
  VAR newState: ReactivityVBT.State;
  BEGIN
    v.guarded := fg;
    IF fg THEN 
      newState := ReactivityVBT.State.Dormant
    ELSE
      newState := ReactivityVBT.State.Active
   END;
   ReactivityVBT.Set (Filter.Child(v), newState, Cursor.DontCare);
  END SetGuard;

PROCEDURE PaintDormant (self  : ReactivityVBT.T;
                        r     : Rect.T;
                        colors: PaintOp.ColorScheme) =
  BEGIN
    IF NOT guardTextureInitialized THEN
      guardTexture := VBTKitResources.GetPixmap ("NEDiagonal");
      guardTextureInitialized := TRUE
    END;
    VBT.PaintTexture (
      self, r, colors.transparentFg, guardTexture, Point.Origin)
  END PaintDormant;

BEGIN
END GuardedBtnVBT.











