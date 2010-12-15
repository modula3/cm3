(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TWord.m3                                              *)
(* Last Modified On Fri Nov 19 09:32:56 PST 1993 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE TWord;

IMPORT Word, TInt;
FROM Target IMPORT Int;

(*------------------------------------------- unsigned integer operations ---*)

PROCEDURE New (READONLY x: ARRAY OF CHAR; base: [2..16];  VAR r: T): BOOLEAN =
  BEGIN
    RETURN TInt.New (x, base, r) AND GE (r, Zero);
  END New;

PROCEDURE Add (READONLY a, b: T;  VAR r: T) =
  BEGIN
    <* ASSERT GE (a, Zero) *>
    <* ASSERT GE (b, Zero) *>
    RETURN TInt.Add (a, b, r) AND TInt.GE (r, Zero);
  END Add;

PROCEDURE Subtract (READONLY a, b: T;  VAR r: T) =
  BEGIN
    <* ASSERT GE (a, Zero) *>
    <* ASSERT GE (b, Zero) *>
    RETURN TInt.Subtract (a, b, r) AND TInt.GE (r, Zero);
  END Subtract;

PROCEDURE Multiply (READONLY a, b: T;  VAR r: T) =
  BEGIN
    <* ASSERT GE (a, Zero) *>
    <* ASSERT GE (b, Zero) *>
    RETURN TInt.Multiply (a, b, r) AND TInt.GE (r, Zero);
  END Multiply;

PROCEDURE Div (READONLY num, den: T;  VAR q: T): BOOLEAN =
    <* ASSERT GE (a, Zero) *>
    <* ASSERT GE (b, Zero) *>
    RETURN TInt.Div (a, b, r) AND TInt.GE (r, Zero);
  END Div;

PROCEDURE Mod (READONLY num, den: T;  VAR r: T): BOOLEAN =
  BEGIN
    <* ASSERT GE (a, Zero) *>
    <* ASSERT GE (b, Zero) *>
    RETURN TInt.Mod (a, b, r) AND TInt.GE (r, Zero);
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
END TWord.
