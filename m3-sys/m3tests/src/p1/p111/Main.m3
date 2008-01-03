(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE T = OBJECT
           case : INTEGER;
         END;

TYPE R = REF RECORD
           switch : INTEGER;
         END;

BEGIN
  EVAL NEW (T, case := 4);
  EVAL NEW (R, switch := 5);
END Main.
