(* Copyright 1989 Digital Equipment Corporation.               *)
(* Distributed only by permission.                             *)
(* Last modified on Wed Feb  1 12:18:32 PST 1995 by kalsow     *)
(*      modified on Tue Mar 30 12:58:13 PST 1993 by birrell    *)
(*      modified on Thu Dec 24 13:36:05 PST 1992 by meehan     *)

(* This module provides a user interface for moving a Trestle window
   from one screen to another. *)

MODULE Rescreen;

IMPORT FormsVBT, Point, Rd, Rect, Rsrc, Thread, Trestle, TrestleComm,
       VBT;

<* FATAL FormsVBT.Error *>
<* FATAL Rd.Failure *>
<* FATAL Thread.Alerted *>
<* FATAL TrestleComm.Failure *>

TYPE
  FormArray = REF ARRAY OF FormsVBT.T;
  T = REF RECORD
    screens: Trestle.ScreenArray;
    forms: FormArray;
    window: VBT.T;
  END;

PROCEDURE OnCancel(<*UNUSED*>fv: FormsVBT.T;
                   <*UNUSED*> name: TEXT;
                   arg: REFANY;
                   <*UNUSED*> ticks: VBT.TimeStamp) =
    (* LL.sup = VBT.mu *)
  VAR self: T := arg;
  BEGIN
    FOR i := 0 TO NUMBER(self.forms^)-1 DO
      Trestle.Delete(self.forms[i]);
    END;
  END OnCancel;

PROCEDURE OnConfirm(fv: FormsVBT.T;
                    name: TEXT;
                    arg: REFANY;
                    ticks: VBT.TimeStamp) =
    (* LL.sup = VBT.mu *)
  VAR self: T := arg;
  VAR myScreen := Trestle.ScreenOf(fv, Rect.NorthWest(VBT.Domain(fv)));
  BEGIN
    Trestle.Overlap(self.window, myScreen.id, myScreen.q);
    OnCancel(fv, name, arg, ticks);
  END OnConfirm;

PROCEDURE DoIt(form, confirm, cancel: TEXT; path: Rsrc.Path; window: VBT.T) =
    (* LL.sup =VBT.mu *)
  <* FATAL Rsrc.NotFound *>
  VAR self := NEW(T);
  VAR wScreen := Trestle.ScreenOf(window, Point.Origin);
  VAR instance, windowTitle, iconTitle, applName: TEXT;
  VAR bgColorR, bgColorG, bgColorB: REAL;
  VAR iconWindow: VBT.T;
  BEGIN
    IF NOT Trestle.GetDecoration(window,
               instance, windowTitle, iconTitle, applName,
               bgColorR, bgColorG, bgColorB, iconWindow) THEN
      windowTitle := "untitled";
    END;
    self.window := window;
    self.screens := Trestle.GetScreens(wScreen.trsl);
    IF self.screens # NIL THEN
      self.forms := NEW(FormArray, NUMBER(self.screens^));
      FOR i := 0 TO NUMBER(self.screens^)-1 DO
        self.forms[i] := NEW(FormsVBT.T).initFromRsrc(form, path);
        FormsVBT.AttachProc(self.forms[i], confirm, OnConfirm, self);
        FormsVBT.AttachProc(self.forms[i], cancel, OnCancel, self);
        Trestle.Attach(self.forms[i], wScreen.trsl);
        Trestle.Decorate(v := self.forms[i],
            windowTitle := "Rescreen \"" & windowTitle & "\"");
        Trestle.Overlap(self.forms[i], self.screens[i].id,
                        Point.Add(Rect.NorthWest(self.screens[i].dom),
                                  Point.T{100,100}));
      END;
    END;
  END DoIt;

PROCEDURE Screens(window: VBT.T): INTEGER =
  (* LL.sup = VBT.mu *)
  VAR wScreen := Trestle.ScreenOf(window, Point.Origin);
  VAR scr := Trestle.GetScreens(wScreen.trsl);
  BEGIN
    IF scr = NIL THEN RETURN 0 ELSE RETURN NUMBER(scr^) END;
  END Screens;

BEGIN
END Rescreen.
