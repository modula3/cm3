(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Pointe.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE Pointe;
IMPORT Fmt;

PROCEDURE Format(a: T): TEXT =
  BEGIN
    RETURN "(" & Fmt.Int(a.h) & "," & Fmt.Int(a.v) & ")";
  END Format;

BEGIN
END Pointe.
