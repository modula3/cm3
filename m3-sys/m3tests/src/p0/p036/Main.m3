(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: fib *)

MODULE Main;

FROM Test IMPORT checkI, done;

PROCEDURE Fib (n: INTEGER): INTEGER =
  BEGIN
    IF n <= 1 THEN 
      RETURN 1;
    ELSE
      RETURN Fib (n-2) + Fib (n-1); END;
  END Fib;

BEGIN

checkI (Fib (20), 10946);
done ();

END Main.
