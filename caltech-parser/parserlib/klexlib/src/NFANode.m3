(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id$ *)

MODULE NFANode;
IMPORT Integer;
IMPORT Fmt;

PROCEDURE Compare(a, b: T): [-1 .. 1] =
  BEGIN
    RETURN Integer.Compare(a.ID, b.ID);
  END Compare;

PROCEDURE Equal(a,b:T):BOOLEAN=BEGIN RETURN Compare(a,b)=0;END Equal;

PROCEDURE Hash(a: T): INTEGER = BEGIN RETURN a.ID; END Hash;

PROCEDURE Format(a: T): TEXT =
  BEGIN
    RETURN Fmt.Int(a.ID);
  END Format;

BEGIN
END NFANode.
