(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Mark.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE Mark;
IMPORT Pos;
IMPORT Rule;
IMPORT Fmt;
IMPORT Integer;

PROCEDURE Compare(a, b: T): [-1 .. 1] =
  VAR
    result := Pos.Compare(a.current, b.current);
  BEGIN
    IF result # 0 THEN RETURN result END;
    result := Pos.Compare(a.return, b.return);
    IF result # 0 THEN RETURN result END;
    RETURN Integer.Compare(Rule.Number(a.first), Rule.Number(b.first));
  END Compare;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    RETURN Compare(a, b) = 0;
  END Equal;

PROCEDURE Hash(a: T): INTEGER =
  BEGIN
    RETURN Pos.Hash(a.current) + Pos.Hash(a.return)*17;
  END Hash;

PROCEDURE Format(a: T): TEXT =
  BEGIN
    RETURN Pos.Format(a.current) & "[" &
           Pos.Format(a.return) & "]" &
           Fmt.Int(Rule.Number(a.first));
  END Format; 

PROCEDURE Advance(a: T): T =
  BEGIN
    RETURN T{current := Pos.Advance(a.current),
             return := a.return,
             first := NIL,
             baseRule := a.current.rule};
  END Advance;

BEGIN
END Mark.
