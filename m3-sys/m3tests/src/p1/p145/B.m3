(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 14:00:11 PST 1992 by kalsow *)
(*      modified on Tue Apr  3 01:51:15 1990 by saxe       *)

MODULE B;

IMPORT C;

PROCEDURE MakeMoose(w, h, a: REAL; l: INTEGER): Moose =
  BEGIN
    RETURN NEW (Moose, weight := w * 2.0, height := h * 2.0,
                       age := a * 2.0, lodgeNumber := l);
  END MakeMoose;

BEGIN
  WITH m = C.bullwinkle(*moose*)^ DO
    m.height := 78.5;
    m.weight := 1250.2;
    m.age := 14.0;
    m.lodgeNumber := 7;
  END;
END B.
