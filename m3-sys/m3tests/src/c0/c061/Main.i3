(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE Main;

TYPE
  E = {a, b, c};

PROCEDURE p (e := E.b) RAISES ANY;
PROCEDURE q (e := E.b) RAISES ANY;
PROCEDURE r (e : E := E.c) RAISES ANY;
PROCEDURE s (e : E := E.c) RAISES ANY;

END Main.

