(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3BackInt.m3                                          *)
(* Last Modified On Tue Jul 12 08:31:56 PDT 1994 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE M3BackInt; (* also known as TInt *)

IMPORT Target, Word, Text, Fmt, TInt, RTIO;

CONST (* IMPORTS *)
  LShift = Word.LeftShift;
  And    = Word.And;

CONST
  Mask     = 16_FF;
  SignMask = 16_80;
  Base     = 16_100;

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

PROCEDURE xFromInt (x: INTEGER;  n: CARDINAL;  VAR r: Int): BOOLEAN =
  BEGIN
    <*ASSERT n # 0*>
    r.n := n;
    FOR i := 0 TO n-1 DO
      r.x[i] := And (x, Mask);
      x := x DIV Base;
    END;
    RETURN (n > 0) AND (x = 0 OR x = -1);
  END xFromInt;

PROCEDURE FromInt (x: INTEGER;  n: CARDINAL;  VAR r: Int): BOOLEAN =
  VAR result1, result2: BOOLEAN;
      r2: Int;
  BEGIN
    result1 :=     xFromInt(x, n, r) AND SignedTruncate(r, n);
    result2 := TInt.FromInt(x, r2.x) AND SignedTruncate(r2, n);
    IF result1 # result2 OR NE(r, r2) THEN
      RTIO.PutText("FromInt error\n ");
      RTIO.PutHex(ORD(result1));
      RTIO.PutText(" ");
      RTIO.PutHex(ORD(result2));
      RTIO.PutText("\n ");
      RTIO.PutText(ToDiagnosticText(r));
      RTIO.PutText("\n ");
      RTIO.PutText(ToDiagnosticText(r2));
      RTIO.Flush();
    END;
    <* ASSERT result1 = result2 *>
    IF result1 AND result2 THEN
      <* ASSERT EQ(r, r2) *>
    END;
    RETURN result1;
  END FromInt;

TYPE Sign = {Bad, Neg, Pos};

PROCEDURE CheckSign (READONLY r: Int;  n: CARDINAL): Sign =
  BEGIN
    <*ASSERT n # 0*>
    IF And (r.x[r.n-1], SignMask) = 0 THEN
      IF n < r.n THEN
        IF And (r.x[n-1], SignMask) # 0 THEN
          RETURN Sign.Bad;
        END;
        FOR i := n TO r.n-1 DO
          IF r.x[i] # 0 THEN
            RETURN Sign.Bad;
          END;
        END;
      END;
      RETURN Sign.Pos;
    ELSE
      IF n < r.n THEN
        IF And (r.x[n-1], SignMask) = 0 THEN
          RETURN Sign.Bad;
        END;
        FOR i := n TO r.n-1 DO
          IF r.x[i] # Mask THEN RETURN
            Sign.Bad;
          END;
        END;
      END;
      RETURN Sign.Neg;
    END;
  END CheckSign;

PROCEDURE xToInt (READONLY r: Int;  VAR x: INTEGER): BOOLEAN =
  VAR sign := CheckSign (r, BITSIZE (INTEGER) DIV BITSIZE (IByte));
      result := TRUE;
  BEGIN
    (* ensure the result has the right sign extension *)
    CASE sign OF
    | Sign.Bad => result := FALSE;
    | Sign.Pos => x := 0;
    | Sign.Neg => x := Word.Not (0);
    END;

    (* finally, pack the bits *)
    FOR i := r.n-1 TO 0 BY -1  DO
      x := Word.Or (LShift (x, BITSIZE (IByte)), r.x[i]);
    END;

    RETURN result;
  END xToInt;

PROCEDURE ToInt (READONLY r: Int;  VAR x: INTEGER): BOOLEAN =
  VAR y: INTEGER;
      result1, result2: BOOLEAN;
  BEGIN
    result1 := xToInt(r, x);
    result2 := TInt.ToInt(SignExtend(r), y);
    <* ASSERT result1 = result2 *>
    IF result1 AND result2 THEN
      <* ASSERT x = y *>
    END;
    RETURN result1;
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

PROCEDURE ToBytes (READONLY r: Int;  VAR buf: ARRAY OF [0..255]): INTEGER =
  VAR n := r.n;
      j := 0;
      k := n;
  BEGIN

    (* strip the sign extension *)

    IF And (r.x[n - 1], SignMask) # 0 THEN
      j := Mask;
    END;

    FOR i := n-1 TO 0 BY -1 DO
      IF r.x[i] # j THEN
        EXIT;
      END;
      DEC (k);
    END;

    (* 0 and -1 appear to require 0 bytes per above code, bump up to 1 *)

    <* ASSERT (k = 0) = (EQ(r, Zero) OR EQ(r, MOne)) *>
    INC(k, ORD(k = 0)); (* increment if 0 *)

    (* Check if it fits. *)

    IF k > NUMBER (buf) THEN
      RETURN -1;
    END;

    (* unpack the bytes *)

    FOR i := 0 TO k-1 DO
      buf[i] := r.x[i];
    END;

    RETURN k;
  END ToBytes;

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
    FOR i := FIRST(a) TO LAST(a) TO  DO
      t := t & Fmt.Unsigned(a[i]);
      IF i # LAST(a) THEN
        t := t & ",";
      END;
    END;
    RETURN t;
  END TargetIntToDiagnosticText;

BEGIN
END M3BackInt.
