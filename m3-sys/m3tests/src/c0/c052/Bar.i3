(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: revealed object type *)

INTERFACE Bar;

IMPORT Foo;

TYPE Private <: ROOT;

REVEAL Foo.T = Private BRANDED "Foo" OBJECT
                 x : INTEGER;
               END;

END Bar.
