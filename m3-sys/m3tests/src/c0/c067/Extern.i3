(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE Extern;

VAR
  inside: INTEGER;

<*EXTERNAL*> VAR
  outside: INTEGER;

<*EXTERNAL*> PROCEDURE Outside (x: INTEGER): BOOLEAN;

PROCEDURE Inside (x: INTEGER): BOOLEAN;

END Extern.
