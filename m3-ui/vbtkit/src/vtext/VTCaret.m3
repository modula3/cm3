(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified On Fri May 14 15:41:57 PDT 1993 by meehan *)
(*      modified On Tue Jun 16 13:12:44 PDT 1992 by muller *)
(* modified On Thu Jul 11 16:05:07 PDT 1991 by mhb *)
(* Modified On Tue Dec 18 15:59:48 1990 by jdd *)
(* Modified On Tue May 15 17:04:13 PDT 1990 by mcjones *)

(* This module contains caret support for VTs. There is currently one
   caret, used for the insertion point; it is planned to extend the
   interface to multiple carets with various behaviors, so the current
   implementation is a little overdone! *)

MODULE VTCaret;

IMPORT Point, Rd, Rect, Thread, VBT;
IMPORT VTBase, VTReal, VTTexture;

(* The caret in the VT can be either On or Off; the client calls Switch to
   set the state. The caret in a view can be temporarily deactivated and
   later reactivated; VTReal and others use this facility to turn off the
   caret when redrawing. When the caret is On and active, it is blinked on
   and off at 1 Hz.

   The caret state is held in vt^.caret.state. The deactivation count is
   held in vt^.caret.deactivationCount; deactivations can nest. A separate
   thread blinks the cursor. When the caret is On, it flashes the cursor;
   when the caret turns Off, it will soon exit. *)

PROCEDURE Init (vt: T) RAISES {} =
  (* Init initializes a vt's caret, Off. *)
  BEGIN
    vt.caret.index := 0;
    vt.caret.state := OnOffState.Off;
    vt.caret.mutex := NEW (MUTEX);
    vt.caret.black := FALSE;
    vt.caret.blinker := NIL
  END Init;

PROCEDURE InitInView (view: View)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  (* InitInView initializes a view's caret. *)
  BEGIN
    view.caret.deactivationCount := 0;
    view.caret.black := FALSE;
    IF view.vt.caret.state = OnOffState.On THEN BlinkerOn (view) END
  END InitInView;

(* Exported operations *)

PROCEDURE Switch (vt: T; state: OnOffState)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    LOCK vt.caret.mutex DO
      IF vt.caret.state # state THEN
        vt.caret.state := state;
        IF state = OnOffState.On THEN
          BlinkersOn (vt)
        ELSE
          VTReal.Change (
            vt, vt.caret.index, vt.caret.index + 1, vt.caret.index + 1);
          BlinkersOff (vt)
        END
      END
    END
  END Switch;

PROCEDURE Move (vt: T; place: CARDINAL)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    LOCK vt.caret.mutex DO
      IF vt.caret.state = OnOffState.On THEN
        VTReal.Change (
          vt, vt.caret.index, vt.caret.index + 1, vt.caret.index + 1);
        BlinkersOff (vt)
      END;
      vt.caret.index := place;
      IF vt.caret.state = OnOffState.On THEN BlinkersOn (vt) END
    END
  END Move;

PROCEDURE Deactivate (view: View) RAISES {} =
  BEGIN
    LOCK view.vt.caret.mutex DO INC (view.caret.deactivationCount) END
  END Deactivate;

PROCEDURE Reactivate (view: View)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  BEGIN
    LOCK view.vt.caret.mutex DO
      DEC (view.caret.deactivationCount);
      IF view.vt.caret.state = OnOffState.On
           AND view.caret.deactivationCount = 0 THEN
        BlinkerOn (view)
      END
    END
  END Reactivate;

PROCEDURE Close (vt: T) RAISES {} =
  (* Close closes a caret. We just turn it off and it dies. *)
  BEGIN
    LOCK vt.caret.mutex DO
      vt.caret.state := OnOffState.Off;
      BlinkersOff (vt)
    END
  END Close;

TYPE
  BlinkerClosure =
    Thread.Closure OBJECT vt: T OVERRIDES apply := Blinker END;

PROCEDURE BlinkersOn (vt: T)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  (* BlinkersOn starts the caret in all views. The mutex is locked. *)
  BEGIN
    Find (vt);
    Paint (vt, TRUE);
    IF vt.caret.blinker = NIL THEN
      vt.caret.blinker := Thread.Fork (NEW (BlinkerClosure, vt := vt))
    END
  END BlinkersOn;

PROCEDURE BlinkersOff (vt: T) RAISES {} =
  (* BlinkersOff stops the caret in all views. The mutex is locked. *)
  BEGIN
    Paint (vt, FALSE)
  END BlinkersOff;

PROCEDURE BlinkerOn (view: View)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  (* BlinkerOn starts the caret in one view. The mutex is locked. *)
  BEGIN
    FindInView (view);
    PaintInView (view, view.vt.caret.black)
  END BlinkerOn;

PROCEDURE Blinker (arg: BlinkerClosure): REFANY RAISES {} =
  (* The caret-blinker thread. Sleep for half a second. Then if state
   = On, invert the caret, paint it, and go back to sleep.  If state
   is Off, see how long it's been since it was On.  If it's been Off
   for 10 seconds, kill the thread.  Otherwise, keep the thread but go
   back to sleep; we're probably just typing or moving the cursor. *)
  CONST GRACEPERIOD = 20;
  VAR offCount := 0;
  BEGIN
    LOOP
      Thread.Pause (0.5D0);
      LOCK arg.vt.caret.mutex DO
        IF arg.vt.caret.state = OnOffState.On THEN
          arg.vt.caret.black := NOT arg.vt.caret.black;
          Paint (arg.vt, arg.vt.caret.black);
          offCount := 0
        ELSIF offCount = GRACEPERIOD THEN
          arg.vt.caret.blinker := NIL;
          RETURN NIL
        ELSE
          INC (offCount)
        END
      END
    END
  END Blinker;

PROCEDURE Find (vt: T)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  (* Find finds the caret in the views. The mutex is locked. *)
  VAR view := vt.views;
  BEGIN
    WHILE view # NIL DO FindInView (view); view := view.next END
  END Find;

PROCEDURE FindInView (view: View)
  RAISES {Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  (* FindInView finds the caret in a view. The mutex is locked. *)
  VAR nw: Point.T;
  BEGIN
    IF NOT (view.real.dirty OR view.virtual.dirty) THEN
      VTBase.UnsafeLocatePoint (view, view.vt.caret.index, nw);
      IF nw.v >= 0 THEN
        view.caret.rect :=
          Rect.Meet (Rect.FromCorner (
                       nw, 1, view.vScreenFont.vScreenFont.box.south
                                - view.vScreenFont.vScreenFont.box.north),
                     view.rect.clip);
        view.caret.lineNo :=
          (nw.v - view.rect.text.north) DIV view.lineSpacing;
        view.real.line [view.caret.lineNo].realLine.width :=
          MAX (view.real.line [view.caret.lineNo].realLine.width,
               view.caret.rect.east - view.rect.text.west)
      ELSE
        view.caret.rect := Rect.Empty
      END
    ELSE
      view.caret.rect := Rect.Empty
    END
  END FindInView;

PROCEDURE Paint (vt: T; on: BOOLEAN) RAISES {} =
  (* Paint paints the caret black or white in all views. The lock is
     set. *)
  VAR view := vt.views;
  BEGIN
    vt.caret.black := on;
    WHILE view # NIL DO PaintInView (view, on); view := view.next END
  END Paint;

PROCEDURE PaintInView (view: View; on: BOOLEAN) RAISES {} =
  (* PaintInView paints the caret black or white in one view. The lock is
     set. *)
  BEGIN
    IF view.caret.deactivationCount = 0
         AND NOT Rect.IsEmpty (view.caret.rect) THEN
      view.caret.black := on;
      IF on THEN
        VBT.PaintTint (
          view.vbt, view.caret.rect, view.vOptions.whiteStroke.fg)
      ELSE
        VBT.PaintTexture (
          view.vbt, view.caret.rect, view.vOptions.whiteStroke.bgFg,
          VTTexture.gray, Point.Origin)
      END
    END
  END PaintInView;

BEGIN
END VTCaret.
