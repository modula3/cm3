(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Starter.i3,v 1.2 2001-09-19 14:22:13 wagner Exp $ *)

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
