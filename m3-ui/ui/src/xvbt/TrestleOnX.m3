(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* by Steve Glassman, Mark Manasse and Greg Nelson *)
(* Last modified on Mon Mar 21 16:10:17 PST 1994 by msm *)
(* modified on Mon Feb 24 13:59:41 PST 1992 by muller *)
(* modified on Wed Nov 20 16:41:02 PST 1991 by gnelson *)
<*PRAGMA LL*>

UNSAFE MODULE TrestleOnX EXPORTS TrestleOnX, TrslOnXF;

IMPORT X, TrestleComm, VBT, XClientF, Thread, VBTClass, TrestleClass;

PROCEDURE Dpy (t: Display): X.DisplayStar =
  BEGIN
    RETURN t.dpy
  END Dpy;

PROCEDURE Drawable (v: VBT.T): X.Drawable =
  BEGIN
    LOOP
      IF v = NIL THEN RETURN X.None END;
      TYPECASE v.upRef OF
        XClientF.Child (ch) => IF ch # NIL THEN RETURN ch.w END
      ELSE                      (* skip *)
      END;
      v := v.parent
    END
  END Drawable;

PROCEDURE Cage (v: VBT.T): X.Drawable =
  BEGIN
    LOOP
      IF v = NIL THEN RETURN X.None END;
      TYPECASE v.upRef OF
        XClientF.Child (ch) => IF ch # NIL THEN RETURN ch.xcage END
      ELSE                      (* skip *)
      END;
      v := v.parent
    END
  END Cage;

PROCEDURE EventHook (t: Display; p: EventProc): EventProc =
  VAR res := t.eventHook;
  BEGIN
    t.eventHook := p;
    RETURN res
  END EventHook;

<*INLINE*> PROCEDURE Enter (t: Display) RAISES {TrestleComm.Failure} =
  BEGIN
    Thread.Acquire(t);
    IF t.dead THEN Thread.Release(t); RAISE TrestleComm.Failure END
  END Enter;

<*INLINE*> PROCEDURE Exit (t: Display; deltaCoverage: [-1 .. 1] := 0)
  RAISES {TrestleComm.Failure} =
  BEGIN
    TRY
      IF t.dead THEN RAISE TrestleComm.Failure END;
      XClientF.AdjustCoverage(t, deltaCoverage)
    FINALLY
      Thread.Release(t)
    END
  END Exit;

PROCEDURE Init() =
  BEGIN
    Visibility := VBT.GetMiscCodeType("Visibility");
  END Init;

BEGIN
END TrestleOnX.
