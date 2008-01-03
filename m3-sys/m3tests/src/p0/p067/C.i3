(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE C;

TYPE T = OBJECT METHODS m() := Make; END;
     U = OBJECT METHODS m(); END;

PROCEDURE Make (self: T);

END C.
