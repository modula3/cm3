(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: IntInt.i3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

INTERFACE IntInt;
TYPE
  T = RECORD
    key: INTEGER;
    value: INTEGER;
  END;
PROCEDURE Compare(a, b: T): [-1..1];
END IntInt.
