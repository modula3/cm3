(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;

IMPORT Extern;

BEGIN
EVAL Extern.Inside (Extern.inside);
EVAL Extern.Outside (Extern.outside);
END Main.

