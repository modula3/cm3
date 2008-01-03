(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE Int;

TYPE
  T = INTEGER;

PROCEDURE Equal (t, u: T): BOOLEAN;

PROCEDURE Copy (t: T): T;

END Int.
