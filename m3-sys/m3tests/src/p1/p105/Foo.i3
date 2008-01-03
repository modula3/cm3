(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
INTERFACE Foo;

TYPE A = OBJECT
            a: INTEGER := 1;
         END;

TYPE B <: A;
REVEAL B = A BRANDED "Foo.B" OBJECT
             a: INTEGER := 2;
           END;

TYPE C = B OBJECT
           x: INTEGER := 3;
         END;

(* C.a will be 2 if the revelation of B is visible, otherwise it's 1. *)

END Foo.
