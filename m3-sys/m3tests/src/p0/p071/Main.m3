(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Tue Oct 16 00:42:40 1990 by muller        *)


MODULE Main;

FROM Test IMPORT checkI, done;

TYPE T = ARRAY [0..1] OF INTEGER;
     U = ARRAY [1..100] OF T;

VAR u : U;

BEGIN
  FOR i := 1 TO 100 DO
    u[i] := T {1, 1}; END;
  FOR i := 20 TO 29 DO
    u[i] := T {2, 3}; END;

  FOR i := 20 TO 29 DO
   checkI (u[i][0], 2);
   checkI (u[i][1], 3); END;


  SUBARRAY (u, 9, 10) := SUBARRAY (u, 19, 10);

  FOR i := 10 TO 19 DO
   checkI (u[i][0], 2);
   checkI (u[i][1], 3); END;

  done ();
END Main.