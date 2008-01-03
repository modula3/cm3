(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE Test;

TYPE T = OBJECT METHODS m (); n() := N; END;

PROCEDURE N (x: T);

END Test.
