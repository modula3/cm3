(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Starter.mg,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

GENERIC MODULE Starter(Elem);
IMPORT Starter;
IMPORT PaneFrame;
REVEAL
  T = Starter.T BRANDED OBJECT
  OVERRIDES
    new := ElemNew;
  END;

PROCEDURE ElemNew(self: T): PaneFrame.T =
  BEGIN
    EVAL self; (* suppress the warning *)
    RETURN NEW(Elem.T);
  END ElemNew;

PROCEDURE New(name: TEXT := Elem.Name;
              ext: TEXT := Elem.Ext;
              key: CHAR := Elem.StartKey): T =
  BEGIN
    RETURN(NEW(T).init(name, ext, key));
  END New;

BEGIN
  S := New();
END Starter.
