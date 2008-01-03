(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
(* Test: revealing objects *)

INTERFACE Test;

IMPORT Foo;

TYPE 
  Private <: ROOT;

REVEAL 
  Foo.T = BRANDED "foo" OBJECT
            x : INTEGER;
          METHODS
            f () := f_default;
          END;

  Foo.U = Private BRANDED "bar" OBJECT
            y : INTEGER;
          END;

PROCEDURE f_default (o: Foo.T); 

END Test.
