(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TWordN.m3                                             *)
(* Last Modified On Fri Nov 19 09:32:56 PST 1993 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE TWordN; (* also known as TWord *)

IMPORT TWord, TInt, TIntN;
FROM Target IMPORT Int;
TYPE T = TIntN.T;

(*------------------------------------------- unsigned integer operations ---*)

PROCEDURE ToInt(READONLY a: T): Int =
  VAR b: Int;
  BEGIN
   b := a.x;
   TIntN.ZeroExtend(b, a.n);
   RETURN b;
  END ToInt;

PROCEDURE FromInt(VAR a: T; n: CARDINAL) =
  BEGIN
    <*ASSERT n # 0*>
    a.n := n;
    (* overflow always ignored *)
    EVAL TIntN.UnsignedTruncate(a.x, n);
  END FromInt;

PROCEDURE Add (READONLY a, b: T;  VAR r: T) =
  BEGIN
    TWord.Add(ToInt(a), ToInt(b), r.x);
    FromInt(r, MIN(a.n, b.n));
  END Add;

PROCEDURE Subtract (READONLY a, b: T;  VAR r: T) =
  BEGIN
    TWord.Subtract(ToInt(a), ToInt(b), r.x);
    FromInt(r, MIN(a.n, b.n));
  END Subtract;

PROCEDURE Multiply (READONLY a, b: T;  VAR r: T) =
  BEGIN
    TWord.Multiply(ToInt(a), ToInt(b), r.x);
    FromInt(r, MIN(a.n, b.n));
  END Multiply;

PROCEDURE Div (READONLY num, den: T;  VAR q: T): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    result := TWord.Div(ToInt(num), ToInt(den), q.x);
    FromInt(q, MIN(num.n, den.n));
    RETURN result;
  END Div;

PROCEDURE Mod (READONLY num, den: T;  VAR r: T): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    result := TWord.Mod(ToInt(num), ToInt(den), r.x);
    FromInt(r, MIN(num.n, den.n));
    RETURN result;
  END Mod;

PROCEDURE LT (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN TWord.LT(ToInt(a), ToInt(b));
  END LT;

PROCEDURE LE (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN TWord.LE(ToInt(a), ToInt(b));
  END LE;

PROCEDURE EQ (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN TInt.EQ(ToInt(a), ToInt(b));
  END EQ;

PROCEDURE NE (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN TInt.NE(ToInt(a), ToInt(b));
  END NE;

PROCEDURE GE (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN TWord.GE(ToInt(a), ToInt(b));
  END GE;

PROCEDURE GT (READONLY a, b: T): BOOLEAN =
  BEGIN
    RETURN TWord.GT(ToInt(a), ToInt(b));
  END GT;

PROCEDURE And (READONLY a, b: T;  VAR r: T) =
  BEGIN
    TWord.And(ToInt(a), ToInt(b), r.x);
    FromInt(r, MIN(a.n, b.n));
  END And;

PROCEDURE Or (READONLY a, b: T;  VAR r: T) =
  BEGIN
    TWord.Or(ToInt(a), ToInt(b), r.x);
    FromInt(r, MIN(a.n, b.n));
  END Or;

PROCEDURE Xor (READONLY a, b: T;  VAR r: T) =
  BEGIN
    TWord.Xor(ToInt(a), ToInt(b), r.x);
    FromInt(r, MIN(a.n, b.n));
  END Xor;

PROCEDURE Not (READONLY a: T;  VAR r: T) =
  BEGIN
    TWord.Not(ToInt(a), r.x);
    FromInt(r, a.n);
  END Not;

PROCEDURE LeftShift (READONLY a: T;  b: [0..Size - 1];  VAR r: T) =
  BEGIN
    TWord.LeftShift(ToInt(a), b, r.x);
    FromInt(r, a.n);
  END LeftShift;

PROCEDURE RightShift (READONLY a: T;  b: [0..Size - 1];  VAR r: T) =
  BEGIN
    TWord.RightShift(ToInt(a), b, r.x);
    FromInt(r, a.n);
  END RightShift;

PROCEDURE Shift (READONLY a: T;  b: INTEGER;  VAR r: T) =
  BEGIN
    TWord.Shift(ToInt(a), b, r.x);
    FromInt(r, a.n);
  END Shift;

PROCEDURE Rotate (READONLY a: T;  b: INTEGER;  VAR r: T) =
  BEGIN
    TWord.Rotate(ToInt(a), b, a.n, r.x);
    FromInt(r, a.n);
  END Rotate;

PROCEDURE Extract (READONLY x: T;  i, n: CARDINAL;  VAR r: T): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    result := TWord.Extract(ToInt(x), i, n, r.x);
    FromInt(r, x.n);
    RETURN result;
  END Extract;

PROCEDURE Insert (READONLY x, y: T;  i, n: CARDINAL;  VAR r: T): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    result := TWord.Insert(ToInt(x), ToInt(y), i, n, r.x);
    FromInt(r, MIN(x.n, y.n));
    RETURN result;
  END Insert;

BEGIN
END TWordN.
