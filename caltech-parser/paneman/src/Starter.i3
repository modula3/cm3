(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE Starter;
IMPORT Pane;
CONST
  Brand = "Starter";
TYPE
  T <: Public;
  Public = OBJECT
    name, ext: TEXT;
    key: CHAR;
  METHODS
    init(name: TEXT := "Document";
         ext: TEXT := "";
         key: CHAR := 'n'): T;
    new(): Pane.T;
  END;

PROCEDURE Equal(a, b: T): BOOLEAN;
END Starter.
