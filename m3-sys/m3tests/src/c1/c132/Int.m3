(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Int;

PROCEDURE Equal (t, u: T): BOOLEAN = 
  BEGIN
    RETURN t = u;
  END Equal;

PROCEDURE Copy (t: T): T =
  BEGIN
    RETURN t;
  END Copy;

BEGIN
END Int.
