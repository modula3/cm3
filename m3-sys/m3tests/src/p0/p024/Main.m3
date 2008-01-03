(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: WITH statements *)

MODULE Main;

FROM Test IMPORT checkI, done;

VAR
  i: INTEGER;

BEGIN

   i := 4;
   WITH j = i DO
     j := 3;
   END;
   checkI (i, 3);

   i := 7;
   WITH j = i DO
     j := j * 2;
   END;
   checkI (i, 14);

   i := 12;
   WITH i = i DO
     i := i * 2;
   END;
   checkI (i, 24);

  done ();

END Main.
