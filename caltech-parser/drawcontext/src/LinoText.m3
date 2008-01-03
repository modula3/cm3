(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: LinoText.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE LinoText;
IMPORT Point;
IMPORT Pointe;
IMPORT Rect;
IMPORT Fmt;

PROCEDURE AttachPoint(r: Rect.T; baseV: INTEGER; a: Attach): Point.T =
  VAR
    midH := (r.west + r.east) DIV 2;
  BEGIN
    CASE a OF
    | Attach.West => RETURN Point.T{r.west, baseV};
    | Attach.East => RETURN Point.T{r.east, baseV};
    | Attach.CenterBase => RETURN Point.T{midH, baseV};
    | Attach.North => RETURN Point.T{midH, r.north};
    | Attach.South => RETURN Point.T{midH, r.south};
    | Attach.Center => RETURN Point.T{midH, (r.north + r.south) DIV 2};
    END;
  END AttachPoint;

PROCEDURE Format(a: T): TEXT =
  BEGIN
    RETURN "[\"" & a.t & "\":" & Fmt.Int(a.size) &
           "@" & Pointe.Format(a.a) & FormatAttach[a.attach] & "]";
  END Format;

PROCEDURE Equal(<*UNUSED*>a,b:T): BOOLEAN = BEGIN <*ASSERT FALSE*> END Equal;
BEGIN
END LinoText.
