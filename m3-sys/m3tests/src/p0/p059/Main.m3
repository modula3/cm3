(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

(***************************************************************************)
(*   Author:  Stephen Harrison						   *)
(***************************************************************************)

(* $Revision: 1.1 $ *)

FROM Test IMPORT checkI, done;

PROCEDURE F (i, j: INTEGER): INTEGER =
  BEGIN
  RETURN MIN (i - 2 , j);
  END F;

BEGIN
  checkI (F (4, LAST (INTEGER)), 2);
  done ();
  
END Main.
