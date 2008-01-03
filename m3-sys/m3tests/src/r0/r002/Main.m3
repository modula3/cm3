(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: stack overflow via fib *)

MODULE Main;

IMPORT Wr, Stdio, Fmt;
<*FATAL ANY*>

PROCEDURE Fib (n: INTEGER): INTEGER =
  BEGIN
    IF n <= 1 THEN 
      RETURN 1;
    ELSE
      RETURN Fib (n-2) + Fib (n-1); END;
  END Fib;

BEGIN

Wr.PutText (Stdio.stdout, "Fib (LAST (INTEGER)) = " &
	    Fmt.Int (Fib (LAST (INTEGER))) & "\n");
Wr.Close (Stdio.stdout);

END Main.
