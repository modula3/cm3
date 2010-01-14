(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TInt.m3                                               *)
(* Last Modified On Tue Jul 12 08:31:56 PDT 1994 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE TInt;

IMPORT Word, TWord;
FROM Target IMPORT Int, IByte, IBytes;

CONST (* IMPORTS *)
  RShift = Word.RightShift;
  LShift = Word.LeftShift;
  And    = Word.And;

CONST
  Mask     = RShift (Word.Not (0), Word.Size - BITSIZE (IByte));
  SignMask = LShift (1, BITSIZE (IByte) - 1);
  Base     = Mask + 1;
  Digits   = ARRAY [0..9] OF CHAR { '0','1','2','3','4','5','6','7','8','9'};

PROCEDURE FromInt (x: INTEGER;  n: CARDINAL;  VAR r: Int): BOOLEAN =
  BEGIN
    <*ASSERT n # 0*>
    r.n := n;
    FOR i := 0 TO n-1 DO
      r.x[i] := And (x, Mask);
      x := x DIV Base;
    END;
    RETURN (n > 0) AND (x = 0 OR x = -1);
  END FromInt;

TYPE Sign = {Bad, Neg, Pos};

PROCEDURE CheckSign (READONLY r: Int;  n: CARDINAL): Sign =
  BEGIN
    <*ASSERT n # 0*>
    IF And (r.x[r.n-1], SignMask) = 0 THEN
      IF n < r.n THEN
        IF And (r.x[n-1], SignMask) # 0 THEN RETURN Sign.Bad END;
        FOR i := n TO r.n-1 DO
          IF r.x[i] # 0 THEN RETURN Sign.Bad END;
        END;
      END;
      RETURN Sign.Pos;
    ELSE
      IF n < r.n THEN
        IF And (r.x[n-1], SignMask) = 0 THEN RETURN Sign.Bad END;
        FOR i := n TO r.n-1 DO
          IF r.x[i] # Mask THEN RETURN Sign.Bad END;
        END;
      END;
      RETURN Sign.Neg;
    END;
  END CheckSign;

PROCEDURE IntI (READONLY r: Int;  n: CARDINAL;  VAR x: Int): BOOLEAN =
  VAR sign := CheckSign (r, n);  j := 0;
  BEGIN
    CASE sign OF
    | Sign.Bad => RETURN FALSE;
    | Sign.Pos => j := 0;
    | Sign.Neg => j := Mask;
    END;
    x.n := n;
    FOR i := 0 TO r.n-1 DO x.x[i] := r.x[i] END;
    FOR i := r.n TO n-1 DO x.x[i] := j END;
    RETURN TRUE;
  END IntI;

PROCEDURE ToInt (READONLY r: Int;  VAR x: INTEGER): BOOLEAN =
  VAR sign := CheckSign (r, BITSIZE (INTEGER) DIV BITSIZE (IByte));
  BEGIN
    (* ensure the result has the right sign extension *)
    CASE sign OF
    | Sign.Bad => RETURN FALSE;
    | Sign.Pos => x := 0;
    | Sign.Neg => x := Word.Not (0);
    END;

    (* finally, pack the bits *)
    FOR i := r.n-1 TO 0 BY -1  DO
      x := Word.Or (LShift (x, BITSIZE (IByte)), r.x[i]);
    END;

    RETURN TRUE;
  END ToInt;

PROCEDURE New (READONLY x: ARRAY OF CHAR;  n: CARDINAL;  VAR r: Int): BOOLEAN =
  CONST ZERO = ORD ('0');   ZEROZERO = 10 * ZERO + ZERO;
  VAR tmp, digit: Int;
  BEGIN
    <*ASSERT n # 0*>
    r := Int{n};
    IF (NUMBER (x) = 1) THEN
      r.x[0] := ORD (x[0]) - ZERO;
    ELSIF (NUMBER (x) = 2) THEN
      r.x[0] := 10 * ORD (x[0]) + ORD (x[1]) - ZEROZERO;
    ELSE
      digit := Int{n};
      FOR i := FIRST (x) TO LAST (x) DO
        digit.x[0] := ORD (x[i]) - ZERO;
        IF NOT Multiply (r, Ten, tmp) THEN RETURN FALSE; END;
        IF NOT Add (tmp, digit, r)    THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END New;

PROCEDURE Add (READONLY a, b: Int;  VAR r: Int): BOOLEAN =
  (* It is safe for r to alias a or b *)
  VAR n := MIN (a.n, b.n);  carry := 0;  r_sign := Sign.Bad;
      a_sign := CheckSign (a, n);  b_sign := CheckSign (b, n);
  BEGIN
    IF a_sign = Sign.Bad THEN RETURN FALSE END;
    IF b_sign = Sign.Bad THEN RETURN FALSE END;
    r.n := n;
    FOR i := 0 TO n-1 DO
      carry := a.x[i] + b.x[i] + carry;
      r.x[i] := And (carry, Mask);
      carry := RShift (carry, BITSIZE (IByte));
    END;
    r_sign := CheckSign (r, n);  <*ASSERT r_sign # Sign.Bad*>
    RETURN (a_sign # b_sign) OR (a_sign = r_sign);
  END Add;

PROCEDURE Subtract (READONLY a, b: Int;  VAR r: Int): BOOLEAN =
  (* It is safe for r to alias a or b *)
  VAR n := MIN (a.n, b.n);  borrow := 0; r_sign := Sign.Bad;
      a_sign := CheckSign (a, n);  b_sign := CheckSign (b, n);
  BEGIN
    IF a_sign = Sign.Bad THEN RETURN FALSE END;
    IF b_sign = Sign.Bad THEN RETURN FALSE END;
    r.n := n;
    FOR i := 0 TO n-1 DO
      borrow := a.x[i] - b.x[i] - borrow;
      r.x[i] := And (borrow, Mask);
      borrow := And (RShift (borrow, BITSIZE (IByte)), 1);
    END;
    r_sign := CheckSign (r, n);  <*ASSERT r_sign # Sign.Bad*>
    RETURN (a_sign = b_sign) OR (a_sign = r_sign);
  END Subtract;
  
PROCEDURE Multiply (READONLY a, b: Int;  VAR r: Int): BOOLEAN =
  VAR
    n := MIN (a.n, b.n);  k, carry: INTEGER;  q: Int;
    p := ARRAY [0.. 2 * NUMBER (IBytes) - 1] OF IByte {0, ..};
    a_sign := CheckSign (a, n);  b_sign := CheckSign (b, n);
  BEGIN
    IF a_sign = Sign.Bad THEN RETURN FALSE END;
    IF b_sign = Sign.Bad THEN RETURN FALSE END;
    FOR i := 0 TO n-1 DO
      FOR j := 0 TO n-1 DO
        k := i + j;
        carry := Word.Times (a.x[i], b.x[j]);
        WHILE carry # 0 DO
          carry := carry + p[k];
          p[k] := And (carry, Mask);
          carry := RShift (carry, BITSIZE (IByte));
          INC (k);
        END;
      END;
    END;

    r.n := n; FOR i := 0 TO n-1 DO r.x[i] := p[i]; END;
    q.n := n; FOR i := 0 TO n-1 DO q.x[i] := p[i+n]; END;

    (* compute the top half *)
    IF And (a.x[n-1], SignMask) # 0 THEN EVAL Subtract (q, b, q); END;
    IF And (b.x[n-1], SignMask) # 0 THEN EVAL Subtract (q, a, q); END;

    (* there is overflow if the top half is not equal to
       to the sign bit of the low half *)
    CASE CheckSign (r, n) OF
    | Sign.Bad => <*ASSERT FALSE*>
    | Sign.Pos => carry := 0;
    | Sign.Neg => carry := Mask;
    END;
    FOR i := 0 TO n-1 DO
      IF q.x[i] # carry THEN RETURN FALSE; END;
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
    num_neg: BOOLEAN;
    den_neg: BOOLEAN;
    n := MIN (a.n, b.n);
    a_sign := CheckSign (a, n);
    b_sign := CheckSign (b, n);
    min: Int;
  BEGIN
    IF a_sign = Sign.Bad THEN RETURN FALSE END;
    IF b_sign = Sign.Bad THEN RETURN FALSE END;

    IF EQ (b, Zero) THEN  RETURN FALSE;  END;
    IF EQ (a, Zero) THEN  q := Zero;  r := Zero;  RETURN TRUE;  END;

    (* grab the signs *)
    num_neg := a_sign = Sign.Neg;
    den_neg := b_sign = Sign.Neg;

    (* check for the only possible overflow:  FIRST DIV -1 *)
    IF num_neg AND den_neg THEN
      TWord.Shift (MOne, n * BITSIZE (IByte) - 1, min);
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
  VAR n := MIN (a.n, b.n);
  BEGIN
    IF CheckSign (a, n) = Sign.Bad THEN RETURN FALSE END;
    IF CheckSign (b, n) = Sign.Bad THEN RETURN FALSE END;
    FOR i := 0 TO n-1 DO
      IF a.x[i] # b.x[i] THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END EQ;

PROCEDURE LT (READONLY a, b: Int): BOOLEAN =
  VAR a_sign := CheckSign (a, a.n);
      b_sign := CheckSign (b, b.n);
      n := MIN (a.n, b.n);
  BEGIN
    <*ASSERT a_sign # Sign.Bad*>
    <*ASSERT b_sign # Sign.Bad*>
    IF (a_sign # b_sign) THEN RETURN (a_sign = Sign.Neg); END;

    IF CheckSign (a, n) = Sign.Bad THEN RETURN a_sign = Sign.Neg END;
    IF CheckSign (b, n) = Sign.Bad THEN RETURN b_sign = Sign.Pos END;

    FOR i := n-1 TO 0 BY -1 DO
      IF    a.x[i] < b.x[i] THEN  RETURN TRUE;
      ELSIF a.x[i] > b.x[i] THEN  RETURN FALSE;
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

PROCEDURE ToChars (READONLY r: Int;  VAR buf: ARRAY OF CHAR): INTEGER =
  VAR
    nDigits : INTEGER := 0;
    minus   : BOOLEAN := FALSE;
    bump    : BOOLEAN := FALSE;
    i, j    : INTEGER;
    result  : ARRAY [0..BITSIZE (IByte) * NUMBER (IBytes)] OF CHAR;
    rr      := r;
    quo, rem, min: Int;
    n := r.n;
  BEGIN
    IF EQ (r, Zero) THEN
      result [0] := '0';
      INC (nDigits);

    ELSE (* handle a non-zero number *)

      (* get rid of negative numbers *)
      IF And (r.x[n-1], SignMask) # 0 THEN
        TWord.Shift (MOne, n * BITSIZE (IByte) - 1, min);
        IF EQ (r, min) THEN
          (* 2's complement makes FIRST a special case *)
          bump := TRUE;
	  EVAL Add (rr, One, rr);
        END;
        minus := TRUE;
        EVAL Subtract (Zero, rr, rr);
      END;

      (* convert the bulk of the digits *)
      WHILE LT (Zero, rr) DO
        TWord.DivMod (rr, Ten, quo, rem);
        result [nDigits] := Digits [rem.x [0]];
        rr := quo;
        INC (nDigits);
      END;

      (* fixup FIRST *)
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
    IF (minus)  THEN 
      buf [0] := '-';
      j := 1; END;
    FOR k := nDigits-1 TO 0 BY -1 DO
      buf [j] := result [k];  INC (j);
    END;

    RETURN j;
  END ToChars;

PROCEDURE ToBytes (READONLY r: Int;  VAR buf: ARRAY OF [0..255]): INTEGER =
  VAR n := r.n;  j := 0;  k := n;
  BEGIN
    (* strip the sign extension *)
    IF And (r.x[n-1], SignMask) # 0 THEN j := Mask END;
    FOR i := n-1 TO 0 BY -1 DO
      IF r.x[i] # j THEN EXIT END;
      DEC (k);
    END;
    IF k > NUMBER (buf) THEN RETURN -1 END;
    (* unpack the bytes *)
    FOR i := 0 TO k-1 DO buf[i] := r.x[i] END;
    RETURN k;
  END ToBytes;

BEGIN
END TInt.
