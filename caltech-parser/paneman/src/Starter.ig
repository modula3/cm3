(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Starter.ig,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

GENERIC INTERFACE Starter(Elem);
IMPORT Starter;
TYPE
  T <: Starter.T;
PROCEDURE New(name: TEXT := Elem.Name;
              ext: TEXT := Elem.Ext;
              key: CHAR := Elem.StartKey): T;
VAR
  S: T;
END Starter.
