(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3BackWord.m3                                         *)
(* Last Modified On Fri Nov 19 09:32:56 PST 1993 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE M3BackWord; (* also known as TWord *)

IMPORT M3BackInt, TWord, TInt, Target;
FROM M3BackInt IMPORT Int;

(*------------------------------------------- unsigned integer operations ---*)

PROCEDURE ZeroExtend(READONLY a: Int): Target.Int =
(*
    zero extend to the precision of Target.Int
*)
  VAR b: Target.Int;
  BEGIN
   FOR i := 0 TO a.n - 1 DO
     b[i] := a.x[i];
   END;
   FOR i := a.n TO LAST(b) DO
     b[i] := 0;
   END;
   RETURN b;
  END ZeroExtend;

PROCEDURE Chop(VAR a: Int; n: CARDINAL) =
  BEGIN
    <*ASSERT n # 0*>
    a.n := n;
    FOR i := n TO LAST(a.x) DO
      a.x[i] := 0;
    END;
  END Chop;

PROCEDURE Add (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    TWord.Add(ZeroExtend(a), ZeroExtend(b), r.x);
    Chop(r, MIN(a.n, b.n));
  END Add;

PROCEDURE Subtract (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    TWord.Subtract(ZeroExtend(a), ZeroExtend(b), r.x);
    Chop(r, MIN(a.n, b.n));
  END Subtract;

PROCEDURE Multiply (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    TWord.Multiply(ZeroExtend(a), ZeroExtend(b), r.x);
    Chop(r, MIN(a.n, b.n));
  END Multiply;

PROCEDURE Div (READONLY num, den: Int;  VAR q: Int): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    result := TWord.Div(ZeroExtend(num), ZeroExtend(den), q.x);
    Chop(q, MIN(num.n, den.n));
    RETURN result;
  END Div;

PROCEDURE Mod (READONLY num, den: Int;  VAR r: Int): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    result := TWord.Mod(ZeroExtend(num), ZeroExtend(den), r.x);
    Chop(r, MIN(num.n, den.n));
    RETURN result;
  END Mod;

PROCEDURE DivMod (READONLY x, y: Int;  VAR q, r: Int) =
  BEGIN
    TWord.DivMod(ZeroExtend(x), ZeroExtend(y), q.x, r.x);
    Chop(r, MIN(x.n, y.n));
    Chop(q, MIN(x.n, y.n));
  END DivMod;

PROCEDURE LT (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN TWord.LT(ZeroExtend(a), ZeroExtend(b));
  END LT;

PROCEDURE LE (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN TWord.LE(ZeroExtend(a), ZeroExtend(b));
  END LE;

PROCEDURE EQ (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN TInt.EQ(ZeroExtend(a), ZeroExtend(b));
  END EQ;

PROCEDURE NE (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN NOT EQ(a, b);
  END NE;

PROCEDURE GE (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN LE(b, a);
  END GE;

PROCEDURE GT (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN LT(b, a);
  END GT;

PROCEDURE And (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    TWord.And(ZeroExtend(a), ZeroExtend(b), r.x);
    Chop(r, MIN(a.n, b.n));
  END And;

PROCEDURE Or (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    TWord.Or(ZeroExtend(a), ZeroExtend(b), r.x);
    Chop(r, MIN(a.n, b.n));
  END Or;

PROCEDURE Xor (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    TWord.Xor(ZeroExtend(a), ZeroExtend(b), r.x);
    Chop(r, MIN(a.n, b.n));
  END Xor;

PROCEDURE Not (READONLY a: Int;  VAR r: Int) =
  BEGIN
    TWord.Not(ZeroExtend(a), r.x);
    Chop(r, a.n);
  END Not;

PROCEDURE LeftShift (READONLY a: Int;  b: CARDINAL;  VAR r: Int) =
  BEGIN
    TWord.LeftShift(ZeroExtend(a), b, r.x);
    Chop(r, a.n);
  END LeftShift;

PROCEDURE RightShift (READONLY a: Int;  b: CARDINAL;  VAR r: Int) =
  BEGIN
    TWord.RightShift(ZeroExtend(a), b, r.x);
    Chop(r, a.n);
  END RightShift;

PROCEDURE Shift (READONLY a: Int;  b: INTEGER;  VAR r: Int) =
  BEGIN
    TWord.Shift(ZeroExtend(a), b, r.x);
    Chop(r, a.n);
  END Shift;

PROCEDURE Rotate (READONLY a: Int;  b: INTEGER;  VAR r: Int) =
  BEGIN
    TWord.Rotate(ZeroExtend(a), b, a.n, r.x);
    Chop(r, a.n);
  END Rotate;

PROCEDURE Extract (READONLY x: Int;  i, n: CARDINAL;  VAR r: Int): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    result := TWord.Extract(ZeroExtend(x), i, n, r.x);
    Chop(r, x.n);
    RETURN result;
  END Extract;

PROCEDURE Insert (READONLY x, y: Int;  i, n: CARDINAL;  VAR r: Int): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    result := TWord.Insert(ZeroExtend(x), ZeroExtend(y), i, n, r.x);
    Chop(r, MIN(x.n, y.n));
    RETURN result;
  END Insert;

BEGIN
END M3BackWord.
