(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT A, AF;

FROM Test IMPORT checkB, done;

BEGIN
checkB (AF.p # NIL, TRUE);
EVAL BITSIZE (A.T); (* to avoid an unused symbol warning *)
done ();
END Main.
