(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: Pos.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE Pos;
IMPORT Rule;
IMPORT Fmt;

PROCEDURE Compare(a, b: T): [-1 .. 1] =
  VAR
    arnum := Rule.Number(a.rule);
    brnum := Rule.Number(b.rule);
  BEGIN
    IF arnum > brnum THEN
      RETURN 1;
    ELSIF arnum < brnum THEN
      RETURN -1;
    ELSIF a.index > b.index THEN
      RETURN 1;
    ELSIF a.index < b.index THEN
      RETURN -1;
    ELSE
      RETURN 0;
    END;
  END Compare;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    RETURN Compare(a, b) = 0;
  END Equal;

PROCEDURE Hash(a: T): INTEGER =
  BEGIN
    RETURN Rule.Number(a.rule) + a.index*23;
  END Hash;

PROCEDURE Format(a: T): TEXT =
  BEGIN
    IF a.rule # NIL THEN
      RETURN Fmt.Int(a.rule.number) & ":" &
             Fmt.Int(a.index);
    ELSIF a.index = 0 THEN
      RETURN "N";
    ELSE
      <* ASSERT a.index = -1 *>
      RETURN "E";
    END;
  END Format; 

PROCEDURE Zero(rule: Rule.T): T =
  BEGIN
    RETURN T{rule := rule,
             cell := rule.syms,
             index := 0};
  END Zero;

PROCEDURE Advance(a: T): T =
  BEGIN
    RETURN T{rule := a.rule,
             cell := a.cell.tail,
             index := a.index + 1};
  END Advance;

BEGIN
END Pos. 
