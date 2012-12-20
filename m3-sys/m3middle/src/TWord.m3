(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: TWord.m3                                              *)
(* Last Modified On Fri Nov 19 09:32:56 PST 1993 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE TWord;

IMPORT Word, TInt;
FROM TInt IMPORT Int, IByte;

CONST (* IMPORTS *)
  RShift = Word.RightShift;
  LShift = Word.LeftShift;

CONST
  Mask = RShift (Word.Not (0), Word.Size - BITSIZE (IByte));
  Base = Mask + 1;

(*------------------------------------------- unsigned integer operations ---*)

PROCEDURE New (READONLY x: ARRAY OF CHAR; base: [2..16];  VAR r: Int): BOOLEAN =
  VAR rr: Int;  digit: INTEGER;  ch: CHAR;
  BEGIN
    r := TInt.Zero;
    FOR i := FIRST (x) TO LAST (x) DO
      ch := x [i];
      IF    ('0' <= ch) AND (ch <= '9') THEN  digit := ORD (ch) - ORD ('0');
      ELSIF ('A' <= ch) AND (ch <= 'F') THEN  digit := ORD (ch) - ORD ('A')+10;
      ELSIF ('a' <= ch) AND (ch <= 'f') THEN  digit := ORD (ch) - ORD ('a')+10;
      ELSE  RETURN FALSE;
      END;
  
      (* rr := r * base *)
      rr := TInt.Zero;
      FOR i := 0 TO LAST(Int) DO
        VAR carry := Word.Times (r[i], base);
        BEGIN
          FOR j := i TO LAST(Int) DO
            IF carry = 0 THEN EXIT END;
            INC (carry, rr [j]);
            rr [j] := Word.And (carry, Mask);
            carry := RShift (carry, BITSIZE (IByte));
          END;
          IF carry # 0 THEN RETURN FALSE END;
        END;
      END;

      (* r := rr + digit *)
      VAR carry := digit;
      BEGIN
        FOR i := 0 TO LAST(Int) DO
          INC (carry, rr [i]);
          r[i] := Word.And (carry, Mask);
          carry := RShift (carry, BITSIZE (IByte));
        END;
        IF carry # 0 THEN RETURN FALSE END;
      END;
    END;

    RETURN TRUE;
  END New;

PROCEDURE Add (READONLY a, b: Int;  VAR r: Int) =
  VAR carry := 0;
  BEGIN
    FOR i := 0 TO LAST(Int) DO
      carry := a[i] + b[i] + carry;
      r[i] := Word.And (carry, Mask);
      carry := RShift (carry, BITSIZE (IByte));
    END;
  END Add;

PROCEDURE Subtract (READONLY a, b: Int;  VAR r: Int) =
  VAR borrow := 0;
  BEGIN
    FOR i := 0 TO LAST(Int) DO
      borrow := a [i] - b [i] - borrow;
      r [i] := Word.And (borrow, Mask);
      borrow := Word.And (RShift (borrow, BITSIZE (IByte)), 1);
    END;
  END Subtract;

PROCEDURE Multiply (READONLY a, b: Int;  VAR r: Int) =
  VAR carry: INTEGER;
  BEGIN
    r := TInt.Zero;
    FOR i := 0 TO LAST(Int) DO
      FOR j := 0 TO LAST(Int) DO
        carry := Word.Times (a[i], b[j]);
        FOR k := i + j TO LAST(Int) DO
          IF carry = 0 THEN EXIT END;
          carry := carry + r[k];
          r[k] := Word.And (carry, Mask);
          carry := RShift (carry, BITSIZE (IByte));
        END;
      END;
    END;
  END Multiply;

PROCEDURE Div (READONLY num, den: Int;  VAR q: Int): BOOLEAN =
  VAR r: Int;
  BEGIN
    IF TInt.EQ (den, TInt.Zero) THEN  RETURN FALSE;  END;
    IF TInt.EQ (num, TInt.Zero) THEN  q := TInt.Zero;  RETURN TRUE;  END;
    DivMod (num, den, q, r);
    RETURN TRUE;
  END Div;

PROCEDURE Mod (READONLY num, den: Int;  VAR r: Int): BOOLEAN =
  VAR q: Int;
  BEGIN
    IF TInt.EQ (den, TInt.Zero) THEN  RETURN FALSE;  END;
    IF TInt.EQ (num, TInt.Zero) THEN  r := TInt.Zero;  RETURN TRUE;  END;
    DivMod (num, den, q, r);
    RETURN TRUE;
  END Mod;

PROCEDURE DivMod (READONLY x, y: Int;  VAR q, r: Int) =
  VAR
    carry   : INTEGER;
    borrow  : INTEGER;
    tmp     : INTEGER;
    max_den : CARDINAL := 0;
    max_num : CARDINAL := 0;
    scale   : INTEGER;
    quo_est : INTEGER;
    num_hi  : INTEGER;
    x1,x2,x3: INTEGER;
    num, den := ARRAY [0..NUMBER (Int)+1] OF INTEGER {0,..};
  BEGIN
    (* initialize the numerator and denominator,
       and find the highest non-zero digits *)
    FOR i := 0 TO LAST(Int) DO
      num[i] := x[i];  IF num[i] # 0 THEN max_num := i; END;
      den[i] := y[i];  IF den[i] # 0 THEN max_den := i; END;
    END;

    q := TInt.Zero;
    r := TInt.Zero;

    IF max_den = 0 THEN
      (* single digit denominator case *)
      carry := 0;
      FOR j := max_num TO 0 BY -1 DO
        tmp := carry * Base + num [j];
        q [j] := tmp DIV den[0];
        carry := tmp MOD den[0];
      END;
      r[0] := carry;
      RETURN;
    END;
      
    (* Insure that the first digit of the divisor is at least Base/2.
       This is required by the quotient digit estimation algorithm.  *)
      
    scale := Base DIV (den [max_den] + 1);
    IF scale > 1 THEN (* scale divisor and dividend *)
      carry := 0;
      FOR i := FIRST (num) TO LAST (num) DO
        tmp := (num[i] * scale) + carry;
        num [i] := Word.And (tmp, Mask);
        carry := RShift (tmp, BITSIZE (IByte));
        IF num[i] # 0 THEN max_num := i; END;
      END;
      
      carry := 0;
      FOR i := FIRST (den) TO LAST (den) DO
        tmp := (den[i] * scale) + carry;
        den[i] := Word.And (tmp, Mask);
        carry := RShift (tmp, BITSIZE (IByte));
        IF den[i] # 0 THEN max_den := i; END;
      END;
    END;
      
    (* Main loop *)
    FOR i := max_num - max_den + 1 TO 1 BY -1 DO
      (* guess the next quotient digit, quo_est, by dividing the first
         two remaining dividend digits by the high order quotient digit.
         quo_est is never low and is at most 2 high.  *)

      num_hi := i + max_den; (* index of highest remaining dividend digit *)
          
      tmp := (num [num_hi] * Base);
      IF num_hi > 0 THEN tmp := tmp + num [num_hi - 1]; END;
      IF num [num_hi] # den [max_den]
        THEN quo_est := tmp DIV den [max_den];
        ELSE quo_est := Base - 1;
      END;
        
      (* refine quo_est so it's usually correct, and at most one high.   *)
      x3 := 0; IF (num_hi > 1) THEN x3 := num[num_hi - 2] END;
      LOOP
        x1 := den[max_den - 1] * quo_est;
        x2 := (tmp - (quo_est * den[max_den])) * Base;
        IF (x1 <= x2 + x3) THEN EXIT END;
        DEC (quo_est);
      END;
          
      (* Try quo_est as the quotient digit, by multiplying the
         denominator by quo_est and subtracting from the remaining
         numerator. Keep in mind that quo_est is the i-1st digit.
         Because we combine the multiply and subtract, borrow
         can be more than 1. *)
      borrow := 0;
      FOR j := 0 TO max_den DO
        tmp := num[i + j - 1] - (quo_est * den[j]) + borrow;
        num [i + j - 1] := tmp MOD Base;
        borrow := tmp DIV Base;
      END; 

      (* if quo_est was high by one, we need to correct things.  *)
      IF -borrow > num [num_hi] THEN
        DEC (quo_est);
        carry := 0;
        FOR j := 0 TO max_den DO 
          tmp := num [i + j - 1] + den [j] + carry;
          num [i + j - 1] := tmp MOD Base;
          carry := tmp DIV Base;
        END;
        INC (num [i + max_den], borrow + carry);
      END;

      (* store the quotient digit.  *)
      q [i - 1] := quo_est;
    END;

    (* finally, compute the remainder *)
    Multiply (q, y, r);
    Subtract (x, r, r);
  END DivMod;

PROCEDURE LT (READONLY a, b: Int): BOOLEAN =
  BEGIN
    FOR i := LAST(Int) TO 0 BY -1 DO
      IF    a [i] < b [i] THEN RETURN TRUE;
      ELSIF a [i] > b [i] THEN RETURN FALSE;
      END;
    END;
    RETURN FALSE;
  END LT;

PROCEDURE LE (READONLY a, b: Int): BOOLEAN =
  BEGIN
    FOR i := LAST(Int) TO 0 BY -1 DO
      IF    a [i] < b [i] THEN RETURN TRUE;
      ELSIF a [i] > b [i] THEN RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END LE;

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
    FOR i := 0 TO LAST(Int) DO
      r [i] := Word.And (a [i], b[i]);
    END;
  END And;

PROCEDURE Or (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    FOR i := 0 TO LAST(Int) DO
      r [i] := Word.Or (a [i], b[i]);
    END;
  END Or;

PROCEDURE Xor (READONLY a, b: Int;  VAR r: Int) =
  BEGIN
    FOR i := 0 TO LAST(Int) DO
      r [i] := Word.Xor (a [i], b[i]);
    END;
  END Xor;

PROCEDURE Not (READONLY a: Int;  VAR r: Int) =
  BEGIN
    FOR i := 0 TO LAST(Int) DO
      r [i] := Word.And (Word.Not (a [i]), Mask);
    END;
  END Not;

PROCEDURE Shift (READONLY a: Int;  b: INTEGER;  VAR r: Int) =
  BEGIN
    IF ABS (b) >= Size THEN
      r := TInt.Zero;
    ELSIF b > 0
      THEN LeftShift(a, b, r);
      ELSE RightShift(a, -b, r);
    END;
  END Shift;

PROCEDURE LeftShift (READONLY a: Int;  b: [0..Size-1];  VAR r: Int) =
  VAR w, i, j, z, x1, x2: INTEGER;
  BEGIN
    IF b = 0 THEN (* no shift *)
      r := a;
    ELSE
      w := b DIV BITSIZE (IByte);
      i := b MOD BITSIZE (IByte);
      j := BITSIZE (IByte) - i;
      FOR k := LAST(Int) TO 0 BY -1 DO
        z := k - w;  x1 := 0;  x2 := 0;
        IF z   >= 0 THEN  x1 := LShift (a[z], i);   END;
        IF z-1 >= 0 THEN  x2 := RShift (a[z-1], j); END;
        r[k] := Word.And (Word.Or (x1, x2), Mask);
      END;
    END;
  END LeftShift;

PROCEDURE RightShift (READONLY a: Int;  b: [0..Size-1];  VAR r: Int) =
  VAR w, i, j, z, x1, x2: INTEGER;
  BEGIN
    IF b = 0 THEN (* no shift *)
      r := a;
    ELSE
      w := b DIV BITSIZE (IByte);
      i := b MOD BITSIZE (IByte);
      j := BITSIZE (IByte) - i;
      FOR k := 0 TO LAST(Int) DO
        z := k + w;  x1 := 0;  x2 := 0;
        IF z   <= LAST(Int) THEN x1 := RShift (a[z], i);   END;
        IF z+1 <= LAST(Int) THEN x2 := LShift (a[z+1], j); END;
        r[k] := Word.And (Word.Or (x1, x2), Mask);
      END;

    END;
  END RightShift;

PROCEDURE Rotate (READONLY a: Int;  b: INTEGER;  n: CARDINAL;  VAR r: Int) =
  VAR
    w, i, j, z, x1, x2: INTEGER;
    tmp: Int;
    size := n * BITSIZE (IByte);
  BEGIN
    b := b MOD size;

    IF b = 0 THEN
      r := a;

    ELSIF b > 0 THEN (* left rotate *)
      w := b DIV BITSIZE (IByte);
      i := b MOD BITSIZE (IByte);
      j := BITSIZE (IByte) - i;
      FOR k := 0 TO n-1 DO
        z := k - w;  x1 := 0;  x2 := 0;
        x1 := LShift (a[z MOD n], i);
        x2 := RShift (a[(z-1) MOD n], j);
        tmp[k] := Word.And (Word.Or (x1, x2), Mask);
      END;
      r := tmp;

    ELSE (* right rotate *)
      w := (-b) DIV BITSIZE (IByte);
      i := (-b) DIV BITSIZE (IByte);
      j := BITSIZE (IByte) - i;
      FOR k := 0 TO n-1 DO
        z := k + w;  x1 := 0;  x2 := 0;
        x1 := RShift (a[z MOD n], i);
        x2 := LShift (a[(z+1) MOD n], j);
        tmp[k] := Word.And (Word.Or (x1, x2), Mask);
      END;
      r := tmp;

    END;
  END Rotate;

PROCEDURE Extract (READONLY x: Int;  i, n: CARDINAL;  VAR r: Int): BOOLEAN =
  VAR w, b: INTEGER;
  BEGIN
    IF i + n > Size THEN RETURN FALSE; END;

    Shift (x, -i, r);

    w := n DIV BITSIZE (IByte);
    b := n MOD BITSIZE (IByte);
    r[w] := Word.And (r[w], RShift (Mask, BITSIZE (IByte) - b));
    FOR k := w + 1 TO LAST(Int) DO r[k] := 0; END;

    RETURN TRUE;
  END Extract;

PROCEDURE Insert (READONLY x, y: Int;  i, n: CARDINAL;  VAR r: Int): BOOLEAN =
  VAR yy, yyy, yyyy: Int;
  BEGIN
    IF i + n > Size THEN RETURN FALSE; END;

    Shift (x, -(i + n), yy);
    Shift (yy, n, r);

    Shift (y, Size - n, yy);
    Shift (yy, -(Size - n), yyy);
    Or (r, yyy, r);
    Shift (r, i, yyyy);
    r := yyyy;

    Shift (x, Size - i, yy);
    Shift (yy, -(Size - i), yyy);
    Or (r, yyy, r);

    RETURN TRUE;
  END Insert;

PROCEDURE Truncate (READONLY a: Int;  n: CARDINAL;  VAR r: Int): BOOLEAN =
  VAR result := TRUE;
  BEGIN
    FOR i := 0 TO n-1 DO r[i] := a[i] END;
    FOR i := n TO LAST(Int) DO
      IF a[i] # 0 THEN result := FALSE END;
      r[i] := 0;
    END;
    RETURN result;
  END Truncate;

BEGIN
END TWord.
