(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE IntInt;
IMPORT Integer;
PROCEDURE Compare(a, b: T): [-1..1] = 
  BEGIN
    RETURN Integer.Compare(a.key, b.key);
  END Compare;
BEGIN
END IntInt.
