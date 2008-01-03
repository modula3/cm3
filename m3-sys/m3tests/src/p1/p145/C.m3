(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Last modified on Tue Oct 27 13:48:25 PST 1992 by kalsow *)
(*      modified on Wed Jan  9 18:40:06 1991 by saxe       *)

MODULE C;

PROCEDURE Elder(m1, m2: Moose): Moose =
  BEGIN
    IF m1^.age > m2^.age THEN RETURN m1 ELSE RETURN m2; END
  END Elder;

PROCEDURE Younger(m1, m2: Moose): Moose =
  BEGIN
    IF m1^.age <= m2^.age THEN RETURN m1 ELSE RETURN m2; END
  END Younger;

PROCEDURE Taller(m1, m2: Moose): Moose =
  BEGIN
    IF m1^.height > m2^.height THEN RETURN m1 ELSE RETURN m2; END
  END Taller;

PROCEDURE Shorter(m1, m2: Moose): Moose =
  BEGIN
    IF m1^.height <= m2^.height THEN RETURN m1 ELSE RETURN m2; END
  END Shorter;

PROCEDURE Heavier(m1, m2: Moose): Moose =
  BEGIN
    IF m1^.weight > m2^.weight THEN RETURN m1 ELSE RETURN m2; END
  END Heavier;

PROCEDURE Lighter(m1, m2: Moose): Moose =
  BEGIN
    IF m1^.weight <= m2^.weight THEN RETURN m1 ELSE RETURN m2; END
  END Lighter;

BEGIN
  thidwick   := NEW(Moose);
  bullwinkle := NEW(Moose);
END C.
