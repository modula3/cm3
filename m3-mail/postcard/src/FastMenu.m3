(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Tue Jul 27 15:24:40 PDT 1993 by birrell*)

(* A FastMenu.T is a subclass of FVTypes.FVMenu, the FormsVBT anchor class,
   that allows middle or right click on the anchor to cause specified button
   actions.  The buttons to be activated are given by "middle" and "right".
   They are typically in the menu popped up by the anchor, but that's
   not actually required.

   When the anchor receives a left-click firstDown, it behaves normally.  If
   it receives a middle or right firstDown, it calls the given VBT's "pre"
   method.  If you drag outside the anchor or you chord, the given VBT's
   "cancel" is called, otherwise lastUp calls the given VBT's action
   procedure. *)

MODULE FastMenu;

IMPORT ButtonVBT, FVTypes, Rect, Thread, VBT;

REVEAL
  T = Public BRANDED "FastMenu.T" OBJECT
      armed: ButtonVBT.T := NIL;
      downClick: VBT.MouseRec;
      sentDownClick: BOOLEAN;
      calledPre: BOOLEAN;
    OVERRIDES
      mouse := MyMouse;
      position := MyPosition;
    END;

(* Unfortunately, we can't call the armed child's "pre" method when the
   menu is poped up, because the child's domain is empty until a subsequent
   "redisplay" invocation.  I can't see an easy fix, so instead we fork a
   thread that calls "pre" when the domain becomes non-empty, as observed by
   polling *)

TYPE
  CallPre = Thread.Closure OBJECT
      anchor: T;
    OVERRIDES
      apply := DoCallPre;
    END;

PROCEDURE DoCallPre(self: CallPre): REFANY =
  BEGIN
    Thread.Pause(0.25d0);
    LOCK VBT.mu DO
      IF self.anchor.armed # NIL THEN
        (* put the menu up *)
        self.anchor.cancel();
        FVTypes.FVMenu.mouse(self.anchor, self.anchor.downClick);
        self.anchor.sentDownClick := TRUE;
      END;
    END;
    LOOP
      LOCK VBT.mu DO
        IF self.anchor.armed = NIL THEN EXIT END;
        IF NOT Rect.IsEmpty(VBT.Domain(self.anchor.armed)) THEN
          self.anchor.armed.pre();
          self.anchor.calledPre := TRUE;
          EXIT
        END;
      END;
      Thread.Pause(0.005d0);
    END;
    RETURN NIL
  END DoCallPre;

PROCEDURE Arm(self: T; what: ButtonVBT.T; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF what # NIL THEN
      self.armed := what;
      self.calledPre := FALSE;
      self.downClick := cd;
      self.sentDownClick := FALSE;
      self.pre();
      EVAL Thread.Fork(NEW(CallPre, anchor := self));
    END;
  END Arm;

PROCEDURE Cancel(self: T) =
  BEGIN
    IF self.armed # NIL THEN
      IF NOT self.sentDownClick THEN self.cancel() END;
      IF self.calledPre THEN self.armed.cancel() END;
      self.armed := NIL;
    END;
  END Cancel;

PROCEDURE Act(self: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF self.armed # NIL THEN
      IF NOT self.sentDownClick THEN self.cancel() END;
      IF NOT self.calledPre THEN self.armed.pre() END;
      self.armed.action(self.armed, cd);
      self.armed.post();
      self.armed := NIL;
    END;
  END Act;

PROCEDURE MyMouse(self: T; READONLY cd: VBT.MouseRec) =
  (* LL.sup = VBT.mu *)
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      IF cd.whatChanged = VBT.Modifier.MouseM THEN
        Arm(self, self.middle, cd);
      ELSIF cd.whatChanged = VBT.Modifier.MouseR THEN
        Arm(self, self.right, cd);
      ELSE
        FVTypes.FVMenu.mouse(self, cd);
        self.sentDownClick := TRUE;
      END;
    ELSE
      IF self.sentDownClick THEN
        FVTypes.FVMenu.mouse(self, cd); (* cancel the menu if its up *)
      END;
      IF cd.clickType = VBT.ClickType.OtherDown THEN
        Cancel(self);
      ELSIF cd.clickType = VBT.ClickType.LastUp THEN
        Act(self, cd);
      END;
    END;
  END MyMouse;

PROCEDURE MyPosition(self: T; READONLY cd: VBT.PositionRec) =
  (* LL.sup = VBT.mu *)
  BEGIN
    FVTypes.FVMenu.position(self, cd);
    IF cd.cp.gone THEN Cancel(self) END;
  END MyPosition;


BEGIN
END FastMenu.
