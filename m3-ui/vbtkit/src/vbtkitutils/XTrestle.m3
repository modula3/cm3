(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* *)
(* Last modified on Fri Jul 30 11:45:09 PDT 1993 by steveg *)
(*      modified on Fri May 14 16:22:19 PDT 1993 by meehan *)
(*      modified on Sun Dec 13 21:49:27 PST 1992 by mhb    *)
<* PRAGMA LL *>

MODULE XTrestle;

IMPORT Axis, Params, Point, StableVBT, Text, Trestle, TrestleComm,
       TrestleImpl, VBT, VBTClass, XParam;

PROCEDURE Install (v          : VBT.T;
                   applName   : TEXT    := NIL;
                   inst       : TEXT    := NIL;
                   windowTitle: TEXT    := NIL;
                   iconTitle  : TEXT    := NIL;
                   bgColorR   : REAL    := -1.0;
                   bgColorG   : REAL    := -1.0;
                   bgColorB   : REAL    := -1.0;
                   iconWindow : VBT.T   := NIL   )
  RAISES {TrestleComm.Failure, Error} =
  VAR
    display   : TEXT            := NIL;
    geometry  : TEXT            := NIL;
    screen    : INTEGER;
    g         : XParam.Geometry;
    trsl      : Trestle.T;
    needScreen                  := TRUE;
    i                           := 1;
  BEGIN
    TRY
      LOOP
        IF i >= Params.Count - 1 THEN
          EXIT
        ELSIF Text.Equal(Params.Get(i), "-display") THEN
          display := Params.Get(i + 1);
          screen := XParam.ParseDisplay(display).screen;
          needScreen := FALSE;
          INC(i, 2)
        ELSIF Text.Equal(Params.Get(i), "-geometry") THEN
          geometry := Params.Get(i + 1);
          g := XParam.ParseGeometry(geometry);
          IF g.size = XParam.Missing THEN
            WITH shapes = VBTClass.GetShapes(v, FALSE) DO
              g.size.h := shapes[Axis.T.Hor].pref;
              g.size.v := shapes[Axis.T.Ver].pref;
            END
          END;
          INC(i, 2)
        ELSE
          INC(i)
        END
      END                        (* LOOP *)
    EXCEPT
      XParam.Error => RAISE Error
    END;

    trsl := Trestle.Connect(display);
    TrestleImpl.SetDefault(trsl);
    Trestle.Attach(v, trsl);
    Trestle.Decorate(v, inst, windowTitle, iconTitle, bgColorR, bgColorG,
                     bgColorB, applName, iconWindow);

    IF geometry = NIL THEN
      Trestle.MoveNear(v, NIL)
    ELSE
      StableVBT.SetShape(v, g.size.h, g.size.v);
      IF needScreen THEN
        screen := Trestle.ScreenOf(v, Point.Origin).id;
      END;
      WITH pos = XParam.Position(trsl, screen, g) DO
        Trestle.Overlap(v, screen, pos);
        (* XParam.Position can't always do the right thing if screen = NoScreen,
           so it does the best it can and then we check to see if that is right
           and do it again if it isn't.  NOTE: the second Trestle.Overlap
           call can be off by the height and width of the X border/title for
           the window *)
        IF screen = Trestle.NoScreen
             AND pos # XParam.Position(
                         trsl, Trestle.ScreenOf(v, Point.Origin).id, g) THEN
          Trestle.Overlap(
            v, screen,
            XParam.Position(trsl, Trestle.ScreenOf(v, Point.Origin).id, g));
        END;
      END;
    END;
  END Install;

BEGIN
END XTrestle.

