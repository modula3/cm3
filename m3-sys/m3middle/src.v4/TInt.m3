(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TInt.m3                                               *)
(* Last Modified On Tue Jul 12 08:31:56 PDT 1994 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE TInt;

IMPORT Word, TWord;
FROM Target IMPORT Int, Integer, IChunk, IChunks, ChunkSize, last_chunk;

CONST (* IMPORTS *)
  RShift = Word.RightShift;
  LShift = Word.LeftShift;
  And    = Word.And;

CONST
  Mask     = RShift (Word.Not (0), Word.Size - ChunkSize);
  SignMask = LShift (1, ChunkSize - 1);
  Base     = Mask + 1;
  Digits   = ARRAY [0..9] OF CHAR { '0','1','2','3','4','5','6','7','8','9'};
  Ten      = Int { IChunks {10, 0, 0, 0}};

PROCEDURE FromInt (x: INTEGER;  VAR r: Int): BOOLEAN =
  BEGIN
    FOR i := 0 TO last_chunk DO
      r.x [i] := And (x, Mask);
      x := x DIV Base;
    END;
    RETURN (x = 0 OR x = -1);
  END FromInt;

PROCEDURE ToInt (READONLY r: Int;  VAR x: INTEGER): BOOLEAN =
  CONST Extras = BITSIZE (INTEGER) DIV ChunkSize;
  VAR j := 0;  sign_chunk := MIN (Extras - 1, last_chunk);
  BEGIN
    (* check that any extra bits are the same as the sign bit *)
    IF And (r.x [sign_chunk], SignMask) # 0 THEN j := Mask; END;
    FOR i := Extras TO last_chunk DO
      IF r.x [i] # j THEN RETURN FALSE; END;
    END;

    (* ensure the result has the right sign extension *)
    IF j = 0
      THEN x := 0;
      ELSE x := Word.Not (0);
    END; 

    (* finally, pack the bits *)
    FOR i := last_chunk TO 0 BY -1  DO
      x := Word.Or (LShift (x, ChunkSize), r.x [i]);
    END;

    RETURN TRUE;
  END ToInt;

PROCEDURE New (READONLY x: ARRAY OF CHAR;  VAR r: Int): BOOLEAN =
  CONST ZERO = ORD ('0');   ZEROZERO = 10 * ZERO + ZERO;
  VAR tmp, digit: Int;
  BEGIN
    r := Zero;
    IF (NUMBER (x) = 1) THEN
      r.x[0] := ORD (x[0]) - ZERO;
    ELSIF (NUMBER (x) = 2) THEN
      r.x[0] := 10 * ORD (x[0]) + ORD (x[1]) - ZEROZERO;
    ELSE
      digit := Zero;
      FOR i := FIRST (x) TO LAST (x) DO
        digit.x [0] := ORD (x[i]) - ZERO;
        IF NOT Multiply (r, Ten, tmp) THEN RETURN FALSE; END;
        IF NOT Add (tmp, digit, r)    THEN RETURN FALSE; END;
      END;
    END;
    RETURN TRUE;
  END New;

PROCEDURE Add (READONLY a, b: Int;  VAR r: Int): BOOLEAN =
  (* It is safe for r to alias a or b *)
  VAR carry := 0;
      a_sign := And (a.x [last_chunk], SignMask);
      b_sign := And (b.x [last_chunk], SignMask);
      r_sign : Word.T;
  BEGIN
    FOR i := 0 TO last_chunk DO
      carry := a.x [i] + b.x [i] + carry;
      r.x [i] := And (carry, Mask);
      carry := RShift (carry, ChunkSize);
    END;
    r_sign := And (r.x [last_chunk], SignMask);
    RETURN (a_sign # b_sign) OR (a_sign = r_sign);
  END Add;

PROCEDURE Subtract (READONLY a, b: Int;  VAR r: Int): BOOLEAN =
  (* It is safe for r to alias a or b *)
  VAR borrow := 0; 
      a_sign := And (a.x [last_chunk], SignMask);
      b_sign := And (b.x [last_chunk], SignMask);
      r_sign : Word.T;
  BEGIN
    FOR i := 0 TO last_chunk DO
      borrow := a.x [i] - b.x [i] - borrow;
      r.x [i] := And (borrow, Mask);
      borrow := And (RShift (borrow, ChunkSize), 1);
    END;
    r_sign := And (r.x [last_chunk], SignMask);
    RETURN (a_sign = b_sign) OR (a_sign = r_sign);
  END Subtract;
  
PROCEDURE Multiply (READONLY a, b: Int;  VAR r: Int): BOOLEAN =
  VAR
    k, carry: INTEGER;
    q: Int;
    p := ARRAY [0.. 2 * NUMBER(IChunks) - 1] OF IChunk {0, ..};
  BEGIN
    FOR i := 0 TO last_chunk DO
      FOR j := 0 TO last_chunk DO
        k := i + j;
        carry := Word.Times (a.x [i], b.x [j]);
        WHILE carry # 0 DO
          carry := carry + p [k];
          p [k] := And (carry, Mask);
          carry := RShift (carry, ChunkSize);
          INC (k);
        END;
      END;
    END;

    FOR i := 0 TO last_chunk DO r.x[i] := p[i]; END;
    FOR i := 0 TO last_chunk DO q.x[i] := p[i+last_chunk+1]; END;

    (* compute the top half *)
    IF And (a.x [last_chunk], SignMask) # 0 THEN EVAL Subtract (q, b, q); END;
    IF And (b.x [last_chunk], SignMask) # 0 THEN EVAL Subtract (q, a, q); END;

    (* there is overflow if the top half is not equal to
       to the sign bit of the low half *)
    carry := 0;
    IF And (r.x [last_chunk], SignMask) # 0 THEN  carry := Mask;  END;
    FOR i := 0 TO last_chunk DO
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
    num_neg : BOOLEAN;
    den_neg : BOOLEAN;
  BEGIN
    IF EQ (b, Zero) THEN  RETURN FALSE;  END;
    IF EQ (a, Zero) THEN  q := Zero;  RETURN TRUE;  END;

    (* grab the signs *)
    num_neg := And (a.x [last_chunk], SignMask) # 0;
    den_neg := And (b.x [last_chunk], SignMask) # 0;

    (* check for the only possible overflow:  FIRST(INTEGER) DIV -1 *)
    IF num_neg AND den_neg
      AND EQ (a, Integer.min)
      AND EQ (b, MOne) THEN
      RETURN FALSE;
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
    FOR i := 0 TO last_chunk DO
      IF a.x[i] # b.x[i] THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END EQ;

PROCEDURE LT (READONLY a, b: Int): BOOLEAN =
  VAR a_sign := And (a.x [last_chunk], SignMask);
      b_sign := And (b.x [last_chunk], SignMask);
  BEGIN
    IF (a_sign # b_sign) THEN RETURN (a_sign # 0); END;

    FOR i := last_chunk TO 0 BY -1 DO
      IF    a.x [i] < b.x [i] THEN  RETURN TRUE;
      ELSIF a.x [i] > b.x [i] THEN  RETURN FALSE;
      END;
    END;

    RETURN FALSE;
  END LT;

PROCEDURE LE (READONLY a, b: Int): BOOLEAN =
  BEGIN
    RETURN EQ (a, b) OR LT (a, b);
  END LE;

PROCEDURE ToChars (READONLY r: Int;  VAR buf: ARRAY OF CHAR): INTEGER =
  VAR
    nDigits : INTEGER := 0;
    minus   : BOOLEAN := FALSE;
    bump    : BOOLEAN := FALSE;
    i, j    : INTEGER;
    result  : ARRAY [0..BITSIZE(Int)] OF CHAR;
    rr      := r;
    quo, rem: Int;
  BEGIN
    IF EQ (r, Zero) THEN
      result [0] := '0';
      INC (nDigits);

    ELSE (* handle a non-zero number *)

      (* get rid of negative numbers *)
      IF And (r.x [last_chunk], SignMask) # 0 THEN
        IF EQ (r, Integer.min) THEN
          (* 2's complement makes FIRST(INTEGER) a special case *)
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

      (* fixup FIRST (INTEGER) *)
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

PROCEDURE ToBytes (READONLY r: Int;  VAR buf: ByteArray): INTEGER =
  VAR j, k: INTEGER := 0;
  BEGIN
    (* unpack the bytes *)
    FOR i := 0 TO last_chunk DO
      k := r.x[i];
      buf[j] := Word.And (k, 16_ff);  INC (j);
      buf[j] := Word.RightShift (k, 8);  INC (j);
    END;

    (* strip the sign extension *)
    DEC (j);
    IF (buf[j] = 0) THEN
      WHILE (j > 0) AND (buf[j] = 0) AND (buf[j-1] < 16_80) DO DEC (j); END;
    ELSIF (buf[j] = 16_ff) THEN
      WHILE (j > 0) AND (buf[j] = 16_ff) AND (buf[j-1] >= 16_80) DO DEC (j); END;
    END;
    INC (j);

    RETURN j;
  END ToBytes;

BEGIN
END TInt.
