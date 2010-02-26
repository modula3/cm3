(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3BackInt.m3                                          *)
(* Last Modified On Tue Jul 12 08:31:56 PDT 1994 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE M3BackInt; (* also known as TInt *)

IMPORT Target, Word, Text, Fmt, TInt;

CONST
  Mask     = 16_FF;
  SignMask = 16_80;

PROCEDURE SignExtend(READONLY a: Int): Target.Int =
(*
    sign extend to the precision of Target.Int
*)
  VAR b: Target.Int;
      extend := 0;
  BEGIN

    <* ASSERT LAST(a.x) = LAST(b) *>

    FOR i := 0 TO a.n - 1 DO
      b[i] := a.x[i];
    END;
    IF Word.And(a.x[a.n - 1], SignMask) # 0 THEN
      extend := Mask;
    END;
    FOR i := a.n TO LAST(b) DO
      b[i] := extend;
    END;
    RETURN b;
  END SignExtend;

PROCEDURE SignedTruncate(VAR a: Int; n: CARDINAL): BOOLEAN =
(*
    check that it fits, by seeing if the sign extension is correct
*)
  VAR extend := 0;
  BEGIN
    <* ASSERT n # 0 *>
    <* ASSERT n <= NUMBER(a.x) *>

    IF Word.And(a.x[LAST(a.x)], SignMask) # 0 THEN
      extend := Mask;
    END;
    FOR i := n TO LAST(a.x) DO
      IF a.x[i] # extend THEN
        RETURN FALSE;
      END;
    END;
    a.n := n;

    RETURN TRUE;

  END SignedTruncate;

PROCEDURE FromInt (x: INTEGER;  n: CARDINAL;  VAR r: Int): BOOLEAN =
  BEGIN
    RETURN TInt.FromInt(x, r.x) AND SignedTruncate(r, n);
  END FromInt;

PROCEDURE ToInt (READONLY r: Int;  VAR x: INTEGER): BOOLEAN =
  VAR r4 := r;
  BEGIN
    r4.n := 4;
    RETURN TInt.ToInt(SignExtend(r4), x);
  END ToInt;

PROCEDURE Add (READONLY a, b: Int;  VAR r: Int): BOOLEAN =
  BEGIN
    RETURN TInt.Add(SignExtend(a), SignExtend(b), r.x) AND SignedTruncate(r, MIN(a.n, b.n));
  END Add;

PROCEDURE Subtract (READONLY a, b: Int;  VAR r: Int): BOOLEAN =
  BEGIN
    RETURN TInt.Subtract(SignExtend(a), SignExtend(b), r.x) AND SignedTruncate(r, MIN(a.n, b.n));
  END Subtract;

PROCEDURE Negate (READONLY a: Int;  VAR r: Int): BOOLEAN =
  BEGIN
    RETURN Subtract(Zero, a, r);
  END Negate;
  
PROCEDURE Abs (READONLY a: Int;  VAR r: Int): BOOLEAN =
  BEGIN
    IF GE(a, Zero) THEN
      r := a;
      RETURN TRUE;
    END;
    RETURN Negate(a, r);
  END Abs;

PROCEDURE Multiply (READONLY a, b: Int;  VAR r: Int): BOOLEAN =
  BEGIN
    RETURN TInt.Multiply(SignExtend(a), SignExtend(b), r.x) AND SignedTruncate(r, MIN(a.n, b.n));
  END Multiply;

PROCEDURE Div (READONLY num, den: Int;  VAR q: Int): BOOLEAN =
  BEGIN
    RETURN TInt.Div(SignExtend(num), SignExtend(den), q.x) AND SignedTruncate(q, MIN(num.n, den.n));
  END Div;

PROCEDURE Mod (READONLY num, den: Int;  VAR r: Int): BOOLEAN =
  BEGIN
    RETURN TInt.Mod(SignExtend(num), SignExtend(den), r.x) AND SignedTruncate(r, MIN(num.n, den.n));
  END Mod;

PROCEDURE EQ (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN TInt.EQ(SignExtend(a), SignExtend(b));
  END EQ;

PROCEDURE LT (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN TInt.LT(SignExtend(a), SignExtend(b));
  END LT;

PROCEDURE LE (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN EQ (a, b) OR LT (a, b);
  END LE;

PROCEDURE NE (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN NOT EQ (a, b);
  END NE;

PROCEDURE GT (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN LT (b, a);
  END GT;

PROCEDURE GE (READONLY a, b: Int): BOOLEAN = 
  BEGIN
    RETURN LE(b, a);
  END GE;

PROCEDURE ToText (READONLY r: Int): TEXT =
  VAR result  : ARRAY [0..BITSIZE (IByte) * NUMBER (IBytes)] OF CHAR;
  BEGIN
    RETURN Text.FromChars(SUBARRAY(result, 0, ToChars(r, result)));
  END ToText;

PROCEDURE ToChars (READONLY r: Int;  VAR buf: ARRAY OF CHAR): INTEGER =
  BEGIN
    RETURN TInt.ToChars(SignExtend(r), buf);
  END ToChars;

PROCEDURE FromTargetInt (READONLY i: Target.Int; byteSize: CARDINAL): Int =
  BEGIN
    RETURN Int{n := byteSize, x := i};
  END FromTargetInt;

PROCEDURE InitInt(VAR a: Int_type; READONLY b: Target.Int_type) =
  BEGIN
    a.size := b.size;
    a.bytes := b.bytes;
    a.min := FromTargetInt(b.min, 8);
    a.max := FromTargetInt(b.max, 8);
  END InitInt;

PROCEDURE Init() =
  BEGIN
    InitInt(Int8, Target.Int8);
    InitInt(Int16, Target.Int16);
    InitInt(Int32, Target.Int32);
    InitInt(Int64, Target.Int64);
    InitInt(Word8, Target.Word8);
    InitInt(Word16, Target.Word16);
    InitInt(Word32, Target.Word32);
    InitInt(Word64, Target.Word64);
    InitInt(Integer, Target.Integer);
  END Init;

PROCEDURE ToDiagnosticText(a: Int): TEXT =
  BEGIN
    RETURN "n:" & Fmt.Unsigned(a.n) & ",x:" & TargetIntToDiagnosticText(a.x);
  END ToDiagnosticText;

PROCEDURE TargetIntToDiagnosticText(a: Target.Int): TEXT =
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
END M3BackInt.
