(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: type identification *)

MODULE Test EXPORTS Foo, Bar;

REVEAL Private = BRANDED "Bar" OBJECT
                   y: INTEGER;
                 END;	

BEGIN
END Test. 
