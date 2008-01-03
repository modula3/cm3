(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 10:58:39 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:28 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE DragButtonDLE;

IMPORT ButtonDLE, DPS, DPSWindow;

PROCEDURE Mouse (e: E; t: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN = 
 VAR in: BOOLEAN;
  BEGIN
  Init (e, t); 
  in := DPS.PlaceIsInBox (event.place, e.box);
  IF e.hot THEN (* We handle everything! *)
    IF event.clickType = DPS.ClickType.LastUp THEN
      e.hot := FALSE;
      e.Dirty (e.box, e);
      e.Up (t, event);
      END;
    RETURN TRUE; (* We handled it! *)
    END;
  IF in THEN
    IF event.clickType = DPS.ClickType.FirstDown THEN
      e.hot := TRUE;
      EVAL e.MoveToLast();
      e.Dirty (e.box, e);
      e.Down (t, event);
      END;
    END;
  RETURN in;
  END Mouse;

PROCEDURE Init (e: E; t: DPSWindow.T) =
  BEGIN
  ButtonDLE.Init (e, t);
  END Init;

PROCEDURE Down (<*UNUSED*> e: E;
                <*UNUSED*> t: DPSWindow.T;
                <*UNUSED*> event: DPS.MouseEvent) =
  BEGIN
  END Down;

PROCEDURE Move (<*UNUSED*> e: E;
                <*UNUSED*> t: DPSWindow.T;
                <*UNUSED*> event: DPS.MouseEvent) =
  BEGIN
  END Move;

PROCEDURE Up (<*UNUSED*> e: E;
              <*UNUSED*> t: DPSWindow.T;
              <*UNUSED*> event: DPS.MouseEvent) =
  BEGIN
  END Up;

  BEGIN
  END DragButtonDLE.

