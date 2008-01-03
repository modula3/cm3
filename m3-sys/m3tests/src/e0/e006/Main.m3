(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Mon Nov  4 10:51:45 PST 1991 by kalsow    *)
(*      modified on Wed Apr 25 01:47:39 1990 by muller        *)

MODULE Main;
(*IMPORT Wr, Rd;*)

PROCEDURE foo (x: REFANY) = BEGIN EVAL x END foo;

BEGIN
TRY
EXCEPT 
| Rd.Failure (reason) => foo (reason);
| Wr.Failure (reason) => foo (reason);
END;
END Main.

