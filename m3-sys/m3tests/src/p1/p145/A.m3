(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 13:59:43 PST 1992 by kalsow *)
(*      modified on Tue Apr  3 01:44:24 1990 by saxe       *)

MODULE A;

IMPORT C;

PROCEDURE MakeMoose(w, h, a: REAL; l: INTEGER): Moose =
  BEGIN
    RETURN NEW (Moose, weight:=w, height:=h, age:=a, lodgeNumber:=l);
  END MakeMoose;

BEGIN
  WITH m = C.thidwick(*moose*)^ DO
    m.height := 84.0;
    m.weight := 900.2;
    m.age := 12.0;
    m.lodgeNumber := 12;
  END;
END A.
