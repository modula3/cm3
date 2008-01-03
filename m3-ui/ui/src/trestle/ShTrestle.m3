(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sat Feb 27 00:17:40 PST 1993 by msm      *)
<*PRAGMA LL*>

MODULE ShTrestle;

             screenOf  := ScreenOf;
             (* Trestle stuff *)
             attach    := Attach;
             delete    := Delete;
             decorate  := Decorate;
             iconize   := Iconize;
             overlap   := Overlap;
             moveNear  := MoveNear;
             installOffscreen := InstallOffscreen;
(*           setColorMap := SetColorMap; *)
             getScreens := GetScreens;
             captureScreen := CaptureScreen;
             allCeded := AllCeded;
             tickTime := TickTime;
             (* some day:
             swap := Swap;
             getName := GetName;
             setScreens := SetScreens;
             nameList := NameList;
             moveNearByName := MoveNearByName;
             swapByName := SwapByName;
             deleteByName := DeleteByName;
             takeOverMouse := TakeOverMouse;
             releaseMouse := ReleaseMouse;
             setHighlight := SetHighlight;
             addParent := AddParent;
             remParent := RemParent;
             warpCursor := WarpCursor; *)
             lastCeded := LastCeded;
             trestleId := TrestleID;
             windowId := WindowID;
             updateBuddies := UpdateBuddies;

PROCEDURE ScreenOf (v: ChildT; ch: VBT.T; READONLY pt: Point.T):
  Trestle.ScreenOfRec =
  VAR
    res    := Trestle.ScreenOf(v, pt);
    par: T;
  BEGIN
    TYPECASE ch OF
      JoinedVBT.T (cld) =>
        par := cld.parents;
        IF par # NIL AND par.link # NIL THEN res.trsl := v END;
    ELSE                         (*skip*)
    END;
    RETURN res
  END ScreenOf;

PROCEDURE Attach(t: ChildT; v: VBT.T) RAISES {Failure} =
  VAR
    res    := Trestle.ScreenOf(v, pt);
    par: T;
  BEGIN
    TYPECASE ch OF
      JoinedVBT.T (cld) =>
        par := cld.parents;
        IF par # NIL AND par.link # NIL THEN res.trsl := v END;
    ELSE                         (*skip*)
    END;
    RETURN res
  END Attach;

PROCEDURE Delete(t: ChildT; v: VBT.T) RAISES {Failure} =
  BEGIN
  END Delete;

PROCEDURE Overlap (         t : ChildT;
                            v : VBT.T;
                            id: Trestle.ScreenID;
                   READONLY nw: Point.T           ) RAISES {Failure} =
  BEGIN
    
  END Overlap;

PROCEDURE MoveNear (tr: ChildT; v, y: VBT.T) RAISES {Failure} =
  VAR
    par, zpar, zzpar: T         := NIL;
    trsl, ztr       : Trestle.T;
    w, z, zz        : VBT.T     := NIL;
  BEGIN
    TYPECASE v OF
      NULL =>                    (*skip*)
    | JoinedVBT.T (ch) =>
        par := ch.parents;
        IF y # NIL AND TrestleImpl.RootChild(y, ztr, z) THEN
          TYPECASE ztr OF
            ChildT (zch) => zpar := zch.ref.parents
          ELSE                   (* skip *)
          END
        END;
        zzpar := zpar;
        WHILE zzpar # NIL DO zzpar.used := FALSE; zzpar := zzpar.link END;
        WHILE par # NIL DO
          TRY
            IF TrestleImpl.RootChild(par, trsl, w) THEN
              IF zpar # NIL THEN
                zzpar := zpar;
                z := NIL;
                LOOP
                  IF zzpar = NIL THEN EXIT END;
                  IF TrestleImpl.RootChild(zzpar, ztr, zz) AND ztr = trsl THEN
                    z := zzpar;
                    IF NOT zzpar.used THEN zzpar.used := TRUE; EXIT END
                  END;
                  zzpar := zzpar.link
                END
              END;
              trsl.moveNear(w, z)
            END
          EXCEPT
            Failure =>           (* skip *)
          END;
          par := par.link
        END
    ELSE
      IF TrestleImpl.RootChild(tr, trsl, w) THEN trsl.moveNear(w, y) END
    END
  END MoveNear;

PROCEDURE InstallOffscreen (tr           : ChildT;
                            v            : VBT.T;
                            width, height: CARDINAL;
                            st           : VBT.ScreenType)
  RAISES {Failure} =
  VAR
    par : T;
    trsl: Trestle.T;
    w   : VBT.T;
  BEGIN
    TYPECASE v OF
      NULL =>                    (*skip*)
    | JoinedVBT.T (ch) =>
        par := ch.parents;
        WHILE par # NIL DO
          TRY
            IF TrestleImpl.RootChild(par, trsl, w) THEN
              pst := st;
              IF pst = par.joinST THEN
                pst := par.st
              ELSIF pst = par.joinST.bits THEN
                pst := par.st.bits
              END;
              trsl.InstallOffscreen(w, width, height, pst)
            END
          EXCEPT
            Failure =>           (* skip *)
          END;
          par := par.link
        END
    ELSE
      IF TrestleImpl.RootChild(tr, trsl, w) THEN
        trsl.installOffscreen(w, width, height, st)
      END
    END
  END InstallOffscreen;

PROCEDURE Decorate (tr: ChildT; v: VBT.T; old, new: TrestleClass.Decoration)
  RAISES {Failure} =
  VAR
    par : T;
    trsl: Trestle.T;
    w   : VBT.T;
  BEGIN
    TYPECASE v OF
      NULL =>                    (*skip*)
    | JoinedVBT.T (ch) =>
        par := ch.parents;
        WHILE par # NIL DO
          TRY
            IF TrestleImpl.RootChild(par, trsl, w) THEN
              TrestleImpl.InnerDecorate(trsl, w, new)
            END
          EXCEPT
            Failure =>           (* skip *)
          END;
          par := par.link
        END
    ELSE
      IF TrestleImpl.RootChild(tr, trsl, w) THEN
        TrestleImpl.InnerDecorate(trsl, w, new)
      END
    END
  END Decorate;

PROCEDURE Iconize (tr: ChildT; v: VBT.T) RAISES {Failure} =
  VAR
    par : T;
    trsl: Trestle.T;
    w   : VBT.T;
  BEGIN
    TYPECASE v OF
      NULL =>                    (*skip*)
    | JoinedVBT.T (ch) =>
        par := ch.parents;
        WHILE par # NIL DO
          TRY
            IF TrestleImpl.RootChild(par, trsl, w) THEN trsl.iconize(w) END
          EXCEPT
            Failure =>           (* skip *)
          END;
          par := par.link
        END
    ELSE
      IF TrestleImpl.RootChild(tr, trsl, w) THEN trsl.iconize(w) END
    END
  END Iconize;

PROCEDURE AllCeded (tr: ChildT): BOOLEAN RAISES {Failure} =
  VAR
    trsl: Trestle.T;
    v   : VBT.T;
  BEGIN
    IF TrestleImpl.RootChild(tr, trsl, v) THEN
      RETURN Trestle.AllCeded(trsl)
    ELSE
      RETURN TRUE
    END
  END AllCeded;

PROCEDURE LastCeded (tr: ChildT): VBT.TimeStamp
  RAISES {Failure, Unimplemented} =
  VAR
    trsl: Trestle.T;
    v   : VBT.T;
  BEGIN
    IF TrestleImpl.RootChild(tr, trsl, v) THEN
      RETURN Trestle.LastCeded(trsl)
    ELSE
      RETURN 0
    END
  END LastCeded;

PROCEDURE TickTime (tr: ChildT): BOOLEAN =
  VAR
    trsl: Trestle.T;
    v   : VBT.T;
  BEGIN
    IF TrestleImpl.RootChild(tr, trsl, v) THEN
      RETURN Trestle.TickTime(trsl)
    ELSE
      RETURN 1000p
    END
  END TickTime;

PROCEDURE TrestleID (tr: ChildT): TEXT =
  VAR
    trsl: Trestle.T;
    v   : VBT.T;
  BEGIN
    IF TrestleImpl.RootChild(tr, trsl, v) THEN
      RETURN trsl.trestleId()
    ELSE
      RETURN "Unattached!"
    END
  END TickTime;

PROCEDURE WindowID (tr:ChildT; <* UNUSED *> w: VBT.T): TEXT =
  VAR
    trsl: Trestle.T;
    v   : VBT.T;
  BEGIN
    IF TrestleImpl.RootChild(tr, trsl, v) THEN
      RETURN trsl.windowID(v)
    ELSE
      RETURN "0"
    END
  END WindowID;

PROCEDURE GetScreens (tr: ChildT): Trestle.ScreenArray RAISES {Failure} =
  VAR
    trsl: Trestle.T;
    v   : VBT.T;
  BEGIN
    IF TrestleImpl.RootChild(tr, trsl, v) THEN
      RETURN trsl.getScreens()
    ELSE
      RETURN NIL
    END
  END GetScreens;

PROCEDURE CaptureScreen (         tr  : ChildT;
                                  id  : Trestle.ScreenID;
                         READONLY clip: Rect.T;
                         VAR      br  : Region.T          ): ScrnPixmap.T
  RAISES {Failure} =
  VAR
    trsl: Trestle.T;
    v   : VBT.T;
  BEGIN
    IF TrestleImpl.RootChild(tr, trsl, v) THEN
      RETURN trsl.captureScreen(id, clip, br)
    ELSE
      br := Region.FromRect(clip);
      RETURN NIL
    END
  END CaptureScreen;

PROCEDURE UpdateBuddies (ch: VBT.T) =
  BEGIN
    <* ASSERT FALSE *>
  END UpdateBuddies;
  
BEGIN
END ShTrestle.
