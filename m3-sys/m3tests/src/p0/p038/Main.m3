(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Mon Apr 23 23:44:43 1990 by muller        *)

MODULE Main;

FROM Test IMPORT checkI, done;

TYPE
  Short = [-16_8000 .. 16_7FFF];

VAR
  s: Short;

BEGIN
  checkI (BYTESIZE (s), 2);
  checkI (BYTESIZE (Short), 2);

  done ();
END Main.
