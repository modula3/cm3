(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE bar;

IMPORT foo;

REVEAL foo.u <: foo.t OBJECT a: INTEGER; METHODS END;

END bar.
