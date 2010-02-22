(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3BackWord.m3                                         *)
(* Last Modified On Fri Nov 19 09:32:56 PST 1993 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE M3BackWord; (* also known as TWord *)

IMPORT M3BackInt, TInt, TWord, Target;
FROM M3BackInt IMPORT Int;

(*------------------------------------------- unsigned integer operations ---*)

PROCEDURE ToTargetInt(READONLY a: Int): Target.Int =
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
  END ToTargetInt;

PROCEDURE Chop(VAR a: Int; n: CARDINAL) =
  BEGIN
    <*ASSERT n # 0*>
    a.n := n;
    TInt.Chop(a.x, n);
  END Chop;

PROCEDURE Add (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    TWord.Add(ToTargetInt(a), ToTargetInt(b), r.x);
    Chop(r, MIN (a.n, b.n));
  END Add;

PROCEDURE Subtract (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    TWord.Subtract(ToTargetInt(a), ToTargetInt(b), r.x);
    Chop(r, MIN (a.n, b.n));
  END Subtract;

PROCEDURE Multiply (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    TWord.Multiply(ToTargetInt(a), ToTargetInt(b), r.x);
    Chop(r, MIN (a.n, b.n));
  END Multiply;

PROCEDURE Div (READONLY num, den: Int;  VAR q: Int): BOOLEAN =
  VAR success: BOOLEAN;
  BEGIN
    success := TWord.Div(ToTargetInt(num), ToTargetInt(den), q.x);
    Chop(q, MIN (num.n, den.n));
    RETURN success;
  END Div;

PROCEDURE Mod (READONLY num, den: Int;  VAR r: Int): BOOLEAN =
  VAR success: BOOLEAN;
  BEGIN
    success := TWord.Mod(ToTargetInt(num), ToTargetInt(den), r.x);
    Chop(r, MIN (num.n, den.n));
    RETURN success;
  END Mod;

PROCEDURE DivMod (READONLY num, den: Int;  VAR q, r: Int) =
  VAR n := MIN (num.n, den.n);
  BEGIN
    TWord.DivMod(ToTargetInt(num), ToTargetInt(den), q.x, r.x);
    Chop(q, n);
    Chop(r, n);
  END DivMod;

PROCEDURE LT (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN TWord.LT(ToTargetInt(a), ToTargetInt(b));
  END LT;

PROCEDURE LE (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN TWord.LE(ToTargetInt(a), ToTargetInt(b));
  END LE;

PROCEDURE EQ (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN TInt.EQ(ToTargetInt(a), ToTargetInt(b));
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
    TWord.And(ToTargetInt(a), ToTargetInt(b), r.x);
    Chop(r, MIN (a.n, b.n));
  END And;

PROCEDURE Or (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    TWord.Or(ToTargetInt(a), ToTargetInt(b), r.x);
    Chop(r, MIN (* MAX? *) (a.n, b.n));
  END Or;

PROCEDURE Xor (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    TWord.Xor(ToTargetInt(a), ToTargetInt(b), r.x);
    Chop(r, MIN (* MAX? *) (a.n, b.n));
  END Xor;

PROCEDURE Not (READONLY a: Int;  VAR r: Int) =
  BEGIN
    TWord.Not(ToTargetInt(a), r.x);
    Chop(r, a.n);
  END Not;

PROCEDURE LeftShift (READONLY a: Int;  b: CARDINAL;  VAR r: Int) =
  BEGIN
    TWord.LeftShift(ToTargetInt(a), b, r.x);
    Chop(r, a.n);
  END LeftShift;

PROCEDURE RightShift (READONLY a: Int;  b: CARDINAL;  VAR r: Int) =
  BEGIN
    TWord.RightShift(ToTargetInt(a), b, r.x);
    Chop(r, a.n);
  END RightShift;

PROCEDURE Shift (READONLY a: Int;  b: INTEGER;  VAR r: Int) =
  BEGIN
    TWord.Shift(ToTargetInt(a), b, r.x);
    Chop(r, a.n);
  END Shift;

PROCEDURE Rotate (READONLY a: Int;  b: INTEGER;  VAR r: Int) =
  BEGIN
    TWord.Rotate(ToTargetInt(a), b, a.n, r.x);
    Chop(r, a.n);
  END Rotate;

PROCEDURE Extract (READONLY x: Int;  i, n: CARDINAL;  VAR r: Int): BOOLEAN =
  VAR success: BOOLEAN;
      size := x.n * BITSIZE (Target.IByte);
  BEGIN
    IF i + n > size THEN RETURN FALSE; END;
    success := TWord.Extract(ToTargetInt(x), i, n, r.x);
    Chop(r, size);
    RETURN success;
  END Extract;

PROCEDURE Insert (READONLY x, y: Int;  i, n: CARDINAL;  VAR r: Int): BOOLEAN =
  VAR success: BOOLEAN;
      size := x.n * BITSIZE (Target.IByte);
  BEGIN
    IF i + n > size THEN RETURN FALSE; END;
    success := TWord.Insert(ToTargetInt(x), ToTargetInt(y), i, n, r.x);
    Chop(r, size);
    RETURN success;
  END Insert;

BEGIN
END M3BackWord.
