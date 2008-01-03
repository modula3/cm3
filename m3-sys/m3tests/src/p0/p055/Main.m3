(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

FROM Fib IMPORT Fib;
FROM Test IMPORT msgI, done;

BEGIN
  msgI (Fib (10));
  done ();
END Main.
