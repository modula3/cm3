(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Line.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE Line;
IMPORT Rect, Recte;
IMPORT Pointe;
IMPORT Fmt;
PROCEDURE GetBoundRect(l: T): Rect.T =
  BEGIN
    RETURN Recte.Inset(Recte.FromCorners(l.a, l.b), -l.s.thick);
    (* if we knew how lines were drawn we could make this a little tighter *)
  END GetBoundRect;
PROCEDURE Equal(a,b:T): BOOLEAN = BEGIN RETURN a=b; END Equal;
PROCEDURE Format(a: T): TEXT =
  BEGIN
    RETURN Pointe.Format(a.a) & Pointe.Format(a.b) &
           ":" & Fmt.Int(a.s.thick);
  END Format;
BEGIN
END Line.
