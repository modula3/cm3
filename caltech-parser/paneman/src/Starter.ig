(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

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
