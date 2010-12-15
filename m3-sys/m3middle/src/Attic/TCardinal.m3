(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TCardinal.m3                                              *)

MODULE TCardinal;

IMPORT TInt;

(*------------------------------------------- unsigned integer operations ---*)

PROCEDURE FromCardinal (x: CARDINAL;  VAR r: T): BOOLEAN =
  BEGIN
    RETURN TInt.FromInt (x, r) AND TInt.GE (r, Zero);
  END FromCardinal;

PROCEDURE Add (READONLY a, b: T;  VAR r: T): BOOLEAN =
  BEGIN
    <* ASSERT GE (a, Zero) *>
    <* ASSERT GE (b, Zero) *>
    RETURN TInt.Add (a, b, r) AND TInt.GE (r, Zero);
  END Add;

PROCEDURE Subtract (READONLY a, b: T;  VAR r: T): BOOLEAN =
  BEGIN
    <* ASSERT GE (a, Zero) *>
    <* ASSERT GE (b, Zero) *>
    RETURN TInt.Subtract (a, b, r) AND TInt.GE (r, Zero);
  END Subtract;

PROCEDURE Multiply (READONLY a, b: T;  VAR r: T): BOOLEAN =
  BEGIN
    <* ASSERT GE (a, Zero) *>
    <* ASSERT GE (b, Zero) *>
    RETURN TInt.Multiply (a, b, r) AND TInt.GE (r, Zero);
  END Multiply;

PROCEDURE Div (READONLY num, den: T;  VAR q: T): BOOLEAN =
  BEGIN
    <* ASSERT GE (num, Zero) *>
    <* ASSERT GE (den, Zero) *>
    RETURN TInt.Div (num, den, q) AND GE (q, Zero);
  END Div;

PROCEDURE Mod (READONLY num, den: T;  VAR r: T): BOOLEAN =
  BEGIN
    <* ASSERT GE (num, Zero) *>
    <* ASSERT GE (den, Zero) *>
    RETURN TInt.Mod (num, den, r) AND TInt.GE (r, Zero);
  END Mod;

PROCEDURE LT (READONLY a, b: T): BOOLEAN =
  BEGIN
    <* ASSERT GE (a, Zero) *>
    <* ASSERT GE (b, Zero) *>
    RETURN TInt.LT (a, b);
  END LT;

PROCEDURE LE (READONLY a, b: T): BOOLEAN =
  BEGIN
    <* ASSERT GE (a, Zero) *>
    <* ASSERT GE (b, Zero) *>
    RETURN TInt.LE (a, b);
  END LE;

PROCEDURE GE (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN LE(b, a);
  END GE;

PROCEDURE GT (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN LT(b, a);
  END GT;

BEGIN
END TCardinal.
