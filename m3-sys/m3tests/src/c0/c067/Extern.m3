(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Extern;

PROCEDURE Inside (x: INTEGER): BOOLEAN =
  BEGIN
    RETURN (x = 17);
  END Inside;

BEGIN
EVAL Outside (inside);
EVAL Outside (outside);
END Extern.
