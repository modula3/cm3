(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

INTERFACE IntInt;
TYPE
  T = RECORD
    key: INTEGER;
    value: INTEGER;
  END;
PROCEDURE Compare(a, b: T): [-1..1];
END IntInt.
