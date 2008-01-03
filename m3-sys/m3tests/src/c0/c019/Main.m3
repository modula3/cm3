(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: WITH statements *)

MODULE Main;

VAR
  i: INTEGER;

BEGIN

   WITH j = i DO
     j := 3;
   END;

   WITH j = i DO
     j := j;
   END;

   WITH i = i DO
     i := i;
   END;

   WITH j = ARRAY [0..2] OF INTEGER {0, 1, 2} DO
     i := j [1];
   END;

END Main.
