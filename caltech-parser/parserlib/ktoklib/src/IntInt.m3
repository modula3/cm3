(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: IntInt.m3,v 1.2 2001-09-19 15:03:34 wagner Exp $ *)

MODULE IntInt;
IMPORT Integer;
PROCEDURE Compare(a, b: T): [-1..1] = 
  BEGIN
    RETURN Integer.Compare(a.key, b.key);
  END Compare;
BEGIN
END IntInt.
