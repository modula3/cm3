(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Fib;

PROCEDURE Fib (i: INTEGER) : INTEGER =
  BEGIN
    IF i <= 1 THEN RETURN 1
    ELSE RETURN Fib (i-1) + Fib (i-2); END;
  END Fib;

BEGIN
END Fib.

    
