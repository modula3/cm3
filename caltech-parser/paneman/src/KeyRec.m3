(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: KeyRec.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE KeyRec;
IMPORT KeyTrans;
PROCEDURE LowerChar(a: T): CHAR =
  VAR
    c: CHAR;
  BEGIN
    c := KeyTrans.Latin1(a.whatChanged);
    IF c >= 'A' AND c <= 'Z' THEN
      c := VAL(ORD(c) - ORD('A') + ORD('a'), CHAR);
    END;
    RETURN c;
  END LowerChar;

PROCEDURE Equal(a,b: T): BOOLEAN =
  BEGIN
    RETURN LowerChar(a) = LowerChar(b);
  END Equal;
BEGIN
END KeyRec.
