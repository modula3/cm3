(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

TYPE
  A = RECORD b: B END;
  B = ARRAY [1..BYTESIZE (A)] OF CHAR;			

BEGIN
END Main.
