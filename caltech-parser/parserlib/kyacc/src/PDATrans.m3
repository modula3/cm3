(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: PDATrans.m3,v 1.2 2001-09-19 15:13:58 wagner Exp $ *)

MODULE PDATrans;
IMPORT Integer;
IMPORT Fmt;

PROCEDURE Compare(a, b: T): [-1..1] =
  VAR
    result := Integer.Compare(a.code, b.code);
  BEGIN
    IF result # 0 THEN RETURN result; END;
    result := Integer.Compare(ORD(a.kind), ORD(b.kind));
    IF result # 0 THEN RETURN result; END;
    RETURN Integer.Compare(a.target, b.target);
  END Compare;

PROCEDURE Equal(a, b: T): BOOLEAN =
  BEGIN
    RETURN a.code = b.code AND a.kind = b.kind AND a.target = b.target;
  END Equal;

PROCEDURE Hash(a: T): INTEGER = 
  BEGIN
    RETURN a.code + ORD(a.kind)*1000 + a.target*33;
  END Hash;

PROCEDURE FmtKind(a: ActKind): TEXT =
  BEGIN
    CASE a OF
    | ActKind.Shift => RETURN "s";
    | ActKind.Reduce => RETURN "r";
    | ActKind.Error => RETURN "e";
    | ActKind.Accept => RETURN "a";
    | ActKind.ShiftReduce => RETURN "sr";
    | ActKind.ShiftAccept => RETURN "sa";
    | ActKind.Jump => RETURN "j";
    END;
  END FmtKind;

PROCEDURE Format(a: T): TEXT =
  BEGIN
    RETURN Fmt.Int(a.code) & ":" & FmtKind(a.kind) & Fmt.Int(a.target);
  END Format;

PROCEDURE PreShift(a: T; code: INTEGER): T =
  VAR
    newKind: ActKind;
  BEGIN
    CASE a.kind OF
    | ActKind.Reduce => newKind := ActKind.ShiftReduce;
    | ActKind.Accept => newKind := ActKind.ShiftAccept;
    ELSE
      <* ASSERT FALSE *>
    END;
    RETURN T{code := code,
             kind := newKind,
             target := a.target};
  END PreShift;

BEGIN
END PDATrans.
