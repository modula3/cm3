(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TInt.m3                                               *)
(* Last Modified On Tue Jul 12 08:31:56 PDT 1994 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE TInt;

IMPORT Word, TWord, Text;

CONST (* IMPORTS *)
  RShift = Word.RightShift;
  LShift = Word.LeftShift;
  And    = Word.And;

CONST
  Mask     = RShift (Word.Not (0), Word.Size - BITSIZE (IByte));
  SignMask = LShift (1, BITSIZE (IByte) - 1);
  Base     = Mask + 1;
  Digits   = ARRAY [0..9] OF CHAR { '0','1','2','3','4','5','6','7','8','9'};
  Ten      = Int{10,0,..};

PROCEDURE FromInt (x: INTEGER;  VAR r: Int): BOOLEAN =
  BEGIN
    FOR i := 0 TO LAST(Int) DO
      r [i] := And (x, Mask);
      x := x DIV Base;
    END;
    RETURN (x = 0 OR x = -1);
  END FromInt;

PROCEDURE ToInt (READONLY r: Int;  VAR x: INTEGER): BOOLEAN =
  CONST Extras = BITSIZE (INTEGER) DIV BITSIZE (IByte);
  VAR j := 0;  sign_chunk := MIN (Extras - 1, LAST(Int));
  BEGIN
    (* check that any extra bits are the same as the sign bit *)
    IF And (r [sign_chunk], SignMask) # 0 THEN j := Mask; END;
    FOR i := Extras TO LAST(Int) DO <*NOWARN*>
      IF r [i] # j THEN RETURN FALSE; END;
    END;

    (* ensure the result has the right sign extension *)
    IF j = 0
      THEN x := 0;
      ELSE x := Word.Not (0);
    END;

    (* finally, pack the bits *)
    FOR i := LAST(Int) TO 0 BY -1  DO
      x := Word.Or (LShift (x, BITSIZE (IByte)), r[i]);
    END;

    RETURN TRUE;
  END ToInt;

PROCEDURE New (READONLY x: ARRAY OF CHAR;  VAR r: Int): BOOLEAN =
  CONST ZERO = ORD ('0');   ZEROZERO = 10 * ZERO + ZERO;
  VAR tmp, digit: Int;
  BEGIN
    r := Zero;
    IF (NUMBER (x) = 1) THEN
      r[0] := ORD (x[0]) - ZERO;
    ELSIF (NUMBER (x) = 2) THEN
      r[0] := 10 * ORD (x[0]) + ORD (x[1]) - ZEROZERO;
    ELSE
      digit := Zero;
      FOR i := FIRST (x) TO LAST (x) DO
        digit [0] := ORD (x[i]) - ZERO;
        IF NOT Multiply (r, Ten, tmp) THEN RETURN FALSE; END;
        IF NOT Add (tmp, digit, r)    THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END New;

PROCEDURE Add (READONLY a, b: Int;  VAR r: Int): BOOLEAN =
  (* It is safe for r to alias a or b *)
  VAR carry := 0;
      a_sign := And (a [LAST(Int)], SignMask);
      b_sign := And (b [LAST(Int)], SignMask);
      r_sign : Word.T;
  BEGIN
    FOR i := 0 TO LAST(Int) DO
      carry := a [i] + b [i] + carry;
      r [i] := And (carry, Mask);
      carry := RShift (carry, BITSIZE (IByte));
    END;
    r_sign := And (r [LAST(Int)], SignMask);
    RETURN (a_sign # b_sign) OR (a_sign = r_sign);
  END Add;

PROCEDURE Inc (VAR a: Int): BOOLEAN =
  VAR b := a;
  BEGIN
    RETURN Add (b, One, a);
  END Inc;

PROCEDURE Dec (VAR a: Int): BOOLEAN =
  VAR b := a;
  BEGIN
    RETURN Subtract (b, One, a);
  END Dec;

PROCEDURE Subtract (READONLY a, b: Int;  VAR r: Int): BOOLEAN =
  (* It is safe for r to alias a or b *)
  VAR borrow := 0;
      a_sign := And (a [LAST(Int)], SignMask);
      b_sign := And (b [LAST(Int)], SignMask);
      r_sign : Word.T;
  BEGIN
    FOR i := 0 TO LAST(Int) DO
      borrow := a [i] - b [i] - borrow;
      r [i] := And (borrow, Mask);
      borrow := And (RShift (borrow, BITSIZE (IByte)), 1);
    END;
    r_sign := And (r [LAST(Int)], SignMask);
    RETURN (a_sign = b_sign) OR (a_sign = r_sign);
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
  VAR
    k, carry: INTEGER;
    q: Int;
    p := ARRAY [0.. 2 * NUMBER(Int) - 1] OF IByte {0, ..};
  BEGIN
    FOR i := 0 TO LAST(Int) DO
      FOR j := 0 TO LAST(Int) DO
        k := i + j;
        carry := Word.Times (a [i], b [j]);
        WHILE carry # 0 DO
          carry := carry + p [k];
          p [k] := And (carry, Mask);
          carry := RShift (carry, BITSIZE (IByte));
          INC (k);
        END;
      END;
    END;

    FOR i := 0 TO LAST(Int) DO r[i] := p[i]; END;
    FOR i := 0 TO LAST(Int) DO q[i] := p[i+NUMBER(Int)]; END;

    (* compute the top half *)
    IF And (a [LAST(Int)], SignMask) # 0 THEN EVAL Subtract (q, b, q); END;
    IF And (b [LAST(Int)], SignMask) # 0 THEN EVAL Subtract (q, a, q); END;

    (* there is overflow if the top half is not equal to
       to the sign bit of the low half *)
    carry := 0;
    IF And (r [LAST(Int)], SignMask) # 0 THEN carry := Mask;  END;
    FOR i := 0 TO LAST(Int) DO
      IF q[i] # carry THEN RETURN FALSE; END;
    END;

    RETURN TRUE;
  END Multiply;

PROCEDURE Div (READONLY num, den: Int;  VAR q: Int): BOOLEAN =
  VAR r: Int;
  BEGIN
    RETURN DivMod (num, den, q, r);
  END Div;

PROCEDURE Mod (READONLY num, den: Int;  VAR r: Int): BOOLEAN =
  VAR q: Int;
  BEGIN
    RETURN DivMod (num, den, q, r);
  END Mod;

PROCEDURE DivMod (READONLY a, b: Int;  VAR q, r: Int): BOOLEAN =
  (*  a DIV b  ==  FLOOR (FLOAT (a) / FLOAT (b)) *)
  (*  a MOD b  ==  a - b * (a DIV b)  *)
  VAR
    num     := a;
    den     := b;
    num_neg : BOOLEAN;
    den_neg : BOOLEAN;
    min     : Int;
  BEGIN
    IF EQ (b, Zero) THEN  RETURN FALSE;  END;
    IF EQ (a, Zero) THEN  q := Zero;  r := Zero;  RETURN TRUE;  END;

    (* grab the signs *)
    num_neg := And (a [LAST(Int)], SignMask) # 0;
    den_neg := And (b [LAST(Int)], SignMask) # 0;

    (* check for the only possible overflow:  FIRST(Int) DIV -1 *)
    IF num_neg AND den_neg THEN
      TWord.Shift (MOne, Size - 1, min);
      IF EQ (a, min) AND EQ (b, MOne) THEN
        RETURN FALSE;
      END;
    END;

    (* convert the operands to unsigned.  *)
    IF num_neg THEN  EVAL Subtract (Zero, a, num);  END;
    IF den_neg THEN  EVAL Subtract (Zero, b, den);  END;

    (* compute the unsigned quotient and remainder *)
    TWord.DivMod (num, den, q, r);

    (* fix-up the results to match the signs (see note below) *)
    IF EQ (r, Zero) THEN
      IF (num_neg # den_neg) THEN EVAL Subtract (Zero, q, q); END;
    ELSIF num_neg AND den_neg THEN
      EVAL Subtract (Zero, r, r);
    ELSIF num_neg THEN
      EVAL Subtract (Zero, q, q);
      EVAL Subtract (q, One, q);
      EVAL Subtract (den, r, r);
    ELSIF den_neg THEN
      EVAL Subtract (Zero, q, q);
      EVAL Subtract (q, One, q);
      EVAL Subtract (r, den, r);
  (*ELSE everything's already ok *)
    END;

    RETURN TRUE;
  END DivMod;

(* DIV and MOD notes:
|     Modula-3 defines  "a DIV b" to be  "FLOOR(a/b)"
|                  and  "a MOD b" to be  "a - b * (a DIV b)".
|
|     Given a >= 0, b > 0, q = a DIV b, and r = a MOD b
|     we get the following conversions:
|
|         x    y     x DIV y     x MOD y
|       ----------------------------------
|         a    b        q           r
|        -a   -b        q          -r
|
|        -a    b       -q           0
|        -a    b      -q-1         b-r  (r # 0)
|
|         a   -b       -q           0
|         a   -b      -q-1         r-b  (r # 0)
*)

PROCEDURE EQ (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN (a = b);
  END EQ;

PROCEDURE LT (READONLY a, b: Int): BOOLEAN =
  VAR a_sign := And (a [LAST(Int)], SignMask);
      b_sign := And (b [LAST(Int)], SignMask);
  BEGIN
    IF (a_sign # b_sign) THEN RETURN (a_sign # 0); END;

    FOR i := LAST(Int) TO 0 BY -1 DO
      IF    a [i] < b [i] THEN  RETURN TRUE;
      ELSIF a [i] > b [i] THEN  RETURN FALSE;
      END;
    END;

    RETURN FALSE;
  END LT;

PROCEDURE LE (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN EQ (a, b) OR LT (a, b);
  END LE;

PROCEDURE NE (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN (a # b);
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
  VAR result: ARRAY [0..BITSIZE (Int)] OF CHAR;
  BEGIN
    RETURN Text.FromChars(SUBARRAY(result, 0, ToChars(r, result)));
  END ToText;

PROCEDURE ToChars (READONLY r: Int;  VAR buf: ARRAY OF CHAR): INTEGER =
  VAR
    nDigits : INTEGER := 0;
    minus   : BOOLEAN := FALSE;
    bump    : BOOLEAN := FALSE;
    i, j    : INTEGER;
    result  : ARRAY [0..Size] OF CHAR;
    rr      := r;
    quo, rem: Int;
    min     : Int;
  BEGIN
    IF EQ (r, Zero) THEN
      result [0] := '0';
      INC (nDigits);

    ELSE (* handle a non-zero number *)

      (* get rid of negative numbers *)
      IF And (r [LAST(Int)], SignMask) # 0 THEN
        TWord.Shift (MOne, Size - 1, min);
        IF EQ (r, min) THEN
          (* 2's complement makes FIRST(Int) a special case *)
          bump := TRUE;
	  EVAL Add (rr, One, rr);
        END;
        minus := TRUE;
        EVAL Subtract (Zero, rr, rr);
      END;

      (* convert the bulk of the digits *)
      WHILE LT (Zero, rr) DO
        TWord.DivMod (rr, Ten, quo, rem);
        result [nDigits] := Digits [rem [0]];
        rr := quo;
        INC (nDigits);
      END;

      (* fixup FIRST (Int) *)
      IF (bump) THEN
        result [nDigits] := '0';
        j := 0;
        LOOP
          i := ORD (result [j]) - ORD ('0');
          INC (i);
	  IF (i < 10) THEN
            result [j] := Digits [i];
            EXIT;
          END;
	  result [j] := '0';
	  INC (j);
        END;
        nDigits := MAX (nDigits, j+1);
      END;
    END;

    (* make sure we've got room for the result *)
    j := nDigits;
    IF minus THEN INC (j); END;
    IF (j > NUMBER (buf)) THEN RETURN -1; END;

    (* build the result buffer *)
    j := 0;
    IF (minus) THEN
      buf [0] := '-';
      j := 1; END;
    FOR k := nDigits-1 TO 0 BY -1 DO
      buf [j] := result [k];  INC (j);
    END;

    RETURN j;
  END ToChars;

PROCEDURE ToBytes (READONLY r: Int;  VAR buf: ARRAY OF [0..255]): CARDINAL =
  VAR j := NUMBER(Int);
  BEGIN
    (* strip the sign extension *)
    DEC (j);
    IF (r[j] = 0) THEN
      WHILE (j > 0) AND (r[j] = 0) AND (r[j-1] < 16_80) DO DEC (j); END;
    ELSIF (r[j] = 16_ff) THEN
      WHILE (j > 0) AND (r[j] = 16_ff) AND (r[j-1] >= 16_80) DO DEC (j); END;
    END;
    INC (j);

    IF j > NUMBER (buf) THEN RETURN 0 END;

    (* unpack the bytes *)
    FOR i := 0 TO j-1 DO buf[i] := r[i] END;

    RETURN j;
  END ToBytes;
  
PROCEDURE Extend (READONLY a: Int;  n: CARDINAL;  VAR r: Int): BOOLEAN =
  VAR result := TRUE;
  BEGIN
    FOR i := 0 TO n-1 DO r[i] := a[i] END;
    IF And (a[n-1], SignMask) = 0 THEN
      FOR i := n TO LAST(Int) DO
        IF a[i] # 0 THEN result := FALSE END;
        r[i] := 0;
      END;
    ELSE
      FOR i := n TO LAST(Int) DO
        IF a[i] # Mask THEN result := FALSE END;
        r[i] := Mask;
      END;
    END;
    RETURN result;
  END Extend;

BEGIN
END TInt.
