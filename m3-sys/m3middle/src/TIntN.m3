(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TIntN.m3                                              *)
(* Last Modified On Tue Jul 12 08:31:56 PDT 1994 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE TIntN; (* TInt but with specified precision, in bytes *)

IMPORT Fmt, TInt;
FROM Target IMPORT Int, IntN;

PROCEDURE ToInt(READONLY a: IntN): Int =
  VAR b: Int;
  BEGIN
    b := a.x;
    TInt.SignExtend(b, a.n);
    RETURN b;
  END ToInt;

PROCEDURE FromInt(VAR a: IntN; n: CARDINAL): BOOLEAN =
  BEGIN
    <* ASSERT n # 0 *>
    <* ASSERT n <= NUMBER(Int) *>
    a.n := n;
    RETURN TInt.SignedTruncate(a.x, n);
  END FromInt;

PROCEDURE FromHostInteger (x: INTEGER;  n: CARDINAL;  VAR r: IntN): BOOLEAN =
  BEGIN
    RETURN TInt.FromInt(x, r.x) AND FromInt(r, n);
  END FromHostInteger;

PROCEDURE ToHostInteger (READONLY r: IntN;  VAR x: INTEGER): BOOLEAN =
  VAR i := r;
  BEGIN
    i.n := BYTESIZE(INTEGER);
    RETURN TInt.ToInt(ToInt(i), x);
  END ToHostInteger;

PROCEDURE Add (READONLY a, b: IntN;  VAR r: IntN): BOOLEAN =
  BEGIN
    RETURN TInt.Add(ToInt(a), ToInt(b), r.x) AND FromInt(r, MIN(a.n, b.n));
  END Add;

PROCEDURE Subtract (READONLY a, b: IntN;  VAR r: IntN): BOOLEAN =
  BEGIN
    RETURN TInt.Subtract(ToInt(a), ToInt(b), r.x) AND FromInt(r, MIN(a.n, b.n));
  END Subtract;

PROCEDURE Negate (READONLY a: IntN;  VAR r: IntN): BOOLEAN =
  BEGIN
    RETURN Subtract(Zero, a, r);
  END Negate;
  
PROCEDURE Abs (READONLY a: IntN;  VAR r: IntN): BOOLEAN =
  BEGIN
    IF GE(a, Zero) THEN
      r := a;
      RETURN TRUE;
    END;
    RETURN Negate(a, r);
  END Abs;

PROCEDURE Multiply (READONLY a, b: IntN;  VAR r: IntN): BOOLEAN =
  BEGIN
    RETURN TInt.Multiply(ToInt(a), ToInt(b), r.x) AND FromInt(r, MIN(a.n, b.n));
  END Multiply;

PROCEDURE Div (READONLY num, den: IntN;  VAR q: IntN): BOOLEAN =
  BEGIN
    RETURN TInt.Div(ToInt(num), ToInt(den), q.x) AND FromInt(q, MIN(num.n, den.n));
  END Div;

PROCEDURE Mod (READONLY num, den: IntN;  VAR r: IntN): BOOLEAN =
  BEGIN
    RETURN TInt.Mod(ToInt(num), ToInt(den), r.x) AND FromInt(r, MIN(num.n, den.n));
  END Mod;

PROCEDURE EQ (READONLY a, b: IntN): BOOLEAN =
  BEGIN
    RETURN TInt.EQ(ToInt(a), ToInt(b));
  END EQ;

PROCEDURE LT (READONLY a, b: IntN): BOOLEAN =
  BEGIN
    RETURN TInt.LT(ToInt(a), ToInt(b));
  END LT;

PROCEDURE LE (READONLY a, b: IntN): BOOLEAN =
  BEGIN
    RETURN TInt.LE(ToInt(a), ToInt(b));
  END LE;

PROCEDURE NE (READONLY a, b: IntN): BOOLEAN =
  BEGIN
    RETURN TInt.NE(ToInt(a), ToInt(b));
  END NE;

PROCEDURE GT (READONLY a, b: IntN): BOOLEAN =
  BEGIN
    RETURN TInt.GT(ToInt(a), ToInt(b));
  END GT;

PROCEDURE GE (READONLY a, b: IntN): BOOLEAN = 
  BEGIN
    RETURN TInt.GE(ToInt(a), ToInt(b));
  END GE;

PROCEDURE ToText (READONLY r: IntN): TEXT =
  BEGIN
    RETURN TInt.ToText(ToInt(r));
  END ToText;

PROCEDURE ToChars (READONLY r: IntN;  VAR buf: ARRAY OF CHAR): INTEGER =
  BEGIN
    RETURN TInt.ToChars(ToInt(r), buf);
  END ToChars;

PROCEDURE FromTargetInt (READONLY i: Int; byteSize: CARDINAL): IntN =
  BEGIN
    RETURN IntN{n := byteSize, x := i};
  END FromTargetInt;

PROCEDURE ToDiagnosticText(a: IntN): TEXT =
  BEGIN
    RETURN "n:" & Fmt.Unsigned(a.n) & ",x:" & TargetIntToDiagnosticText(a.x);
  END ToDiagnosticText;

PROCEDURE TargetIntToDiagnosticText(a: Int): TEXT =
  VAR t := "";
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      t := t & Fmt.Unsigned(a[i]);
      IF i # LAST(a) THEN
        t := t & ",";
      END;
    END;
    RETURN t;
  END TargetIntToDiagnosticText;

BEGIN
END TIntN.
