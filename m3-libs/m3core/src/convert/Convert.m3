(* Copyright (C) 1990, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)

(* Last modified on Fri May  7 14:42:51 PDT 1993 by mjordan    *)
(*      modified on Fri Apr  9 15:25:53 PDT 1993 by muller     *)
(*      modified on Mon Dec 23 11:03:05 PST 1991 by kalsow     *)


UNSAFE MODULE Convert;

IMPORT Word, Ctypes;
FROM CConvert IMPORT strtod, dtoa;

VAR
  Digits := ARRAY [0..15] OF CHAR {
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };

PROCEDURE FromInt (VAR buf    : Buffer;
                       value  : INTEGER;
                       base   : Base := 10;
                       prefix : BOOLEAN := FALSE): INTEGER   RAISES {Failed} =
  VAR
    nDigits : INTEGER := 0;
    minus   : BOOLEAN := FALSE;
    bump    : BOOLEAN := FALSE;
    i, j    : INTEGER;
    c       : CHAR;
    result  : ARRAY [0..BITSIZE (INTEGER)] OF CHAR;

  BEGIN
    IF (value = 0) THEN
      result[0] := '0';
      nDigits := 1;
    ELSE (* handle a non-zero number *)
      (* get rid of negative numbers *)
      IF (value < 0) THEN
        IF (value = FIRST (INTEGER)) THEN
          (* 2's complement makes FIRST(INTEGER) a special case *)
          bump := TRUE;
	  INC (value);
        END;
        minus := TRUE;
        value := -value;
        <* ASSERT value > 0 *>
      END;

      (* convert the bulk of the digits *)
      WHILE (value > 0) DO
        result [nDigits] := Digits [value MOD base];
        value := value DIV base;
        INC (nDigits);
      END;

      (* fixup FIRST (INTEGER) *)
      IF (bump) THEN
        result [nDigits] := '0';
        j := 0;
        LOOP
          c := result [j];
          IF (c <= '9')
            THEN i := ORD (c) - ORD ('0');
            ELSE i := ORD (c) - ORD ('a') + 10;
          END;
          INC (i);
	  IF (i < base) THEN  result [j] := Digits [i];  EXIT END;
	  result [j] := '0';
	  INC (j);
        END;
        nDigits := MAX (nDigits, j+1);
      END;
    END;

    (* make sure we've got room for the result *)
    j := nDigits;
    IF minus THEN INC (j) END;
    IF prefix THEN IF base > 9 THEN INC (j, 3) ELSE INC (j, 2) END END;
    IF (j > NUMBER (buf)) THEN RAISE Failed END;

    (* build the result buffer *)
    j := 0;
    IF (minus)  THEN buf [0] := '-';  j := 1; END;
    IF (prefix) THEN
      IF (base > 9) THEN buf[j] := '1'; INC (j); END;
      buf[j] := Digits [base MOD 10];  INC (j);
      buf[j] := '_';  INC (j);
    END;
    FOR k := nDigits-1 TO 0 BY -1 DO
      buf [j] := result [k];  INC (j);
    END;

    RETURN j;
  END FromInt;


PROCEDURE FromUnsigned (VAR buf    : Buffer;
                            value  : INTEGER;
                            base   : Base := 10;
                            prefix : BOOLEAN := FALSE): INTEGER RAISES{Failed}=
  VAR
    nDigits : INTEGER := 0;
    j       : INTEGER;
    result  : ARRAY [0..BITSIZE (INTEGER)] OF CHAR;

  BEGIN
    IF (value = 0) THEN
      result[0] := '0';
      nDigits := 1;
    ELSE
      (* convert the bulk of the digits *)
      WHILE (value # 0) DO
        result [nDigits] := Digits [Word.Mod (value, base)];
        value := Word.Divide (value, base);
        INC (nDigits);
      END;
    END;

    (* make sure we've got room for the result *)
    j := nDigits;
    IF (prefix) THEN IF base > 9 THEN INC (j, 3) ELSE INC (j, 2) END END;
    IF (j > NUMBER (buf)) THEN RAISE Failed END;

    (* build the result buffer *)
    j := 0;
    IF (prefix) THEN
      IF (base > 9) THEN buf[j] := '1'; INC (j); END;
      buf[j] := Digits [base MOD 10];  INC (j);
      buf[j] := '_';  INC (j);
    END;
    FOR k := nDigits-1 TO 0 BY -1 DO
      buf [j] := result [k];  INC (j);
    END;

    RETURN j;
  END FromUnsigned;


PROCEDURE FromFloat (VAR buf : Buffer;
                     v       : REAL;
                     p       : INTEGER := 6;
                     style   := Style.Mix): INTEGER RAISES {Failed} =
BEGIN
  RETURN InternalFromLongFloat (buf, FLOAT (v, LONGREAL), p, style, 'E');
END FromFloat;

PROCEDURE FromLongFloat (VAR buf : Buffer;
                         v       : LONGREAL;
                         p       : INTEGER := 6;
                         style   := Style.Mix): INTEGER RAISES {Failed} =
BEGIN
  RETURN InternalFromLongFloat (buf, v, p, style, 'D');
END FromLongFloat;

PROCEDURE FromExtended (VAR buf : Buffer;
                        v       : EXTENDED;
                        p       : INTEGER := 6;
                        style   := Style.Mix): INTEGER RAISES {Failed} =
BEGIN
  RETURN InternalFromLongFloat (buf, FLOAT (v, LONGREAL), p, style, 'X');
END FromExtended;

                  
PROCEDURE InternalFromLongFloat (VAR buf : Buffer;
                             v       : LONGREAL;
                             p       : INTEGER := 6;
                             style   := Style.Mix;
                      exponentChar   : CHAR): INTEGER   RAISES {Failed} =

VAR
  len, consumed := 0;
  digits: INTEGER;
  sign: Ctypes.int;

  start, end: Ctypes.char_star;
  mode_i, p_i: Ctypes.int;
  decpt  : Ctypes.int;

  ds, df, d: REF ARRAY OF CHAR;
  Af, Bf, Cf, Df, Ef, Xf: INTEGER;   Pf := TRUE;
  As, Bs, Cs, Ds, Es, Xs: INTEGER;   Ps := TRUE;
  A,  B,  C,  D,  E,  X : INTEGER;   P  := TRUE;


BEGIN
  (* The string will consist of:
      - the first A digits from the conversion
      - B '0's
      - '.' if P is TRUE
      - C '0's
      - the next D digits from the conversion
      - E '0's
      - if X # LAST (INTEGER), " <expchar> <exponent>" *)

  Xf := LAST (INTEGER);
  Xs := LAST (INTEGER);

  IF style = Style.Flo OR style = Style.AltFlo OR style = Style.Mix THEN
    mode_i := 3;
    p_i := p;
    start := dtoa (v, mode_i, p_i, ADR(decpt), ADR(sign), ADR (end));
    digits := (end - start) DIV ADRSIZE (CHAR);
    
    df := NEW (REF ARRAY OF CHAR, digits);
    FOR i := 0 TO digits - 1 DO
      df [i] := LOOPHOLE(start, UNTRACED REF CHAR)^;
      INC (start, ADRSIZE (CHAR)); END;

    IF decpt = 9999 THEN (* Special value returned by dtoa *)
      IF sign # 0 THEN
        buf [0] := '-'; 
        len := 1;
      ELSE
        len := 0; END;
      SUBARRAY (buf, len, digits) := df^;
      RETURN (len + digits); END;

    IF decpt <= 0 THEN
      Af := 0;
      Bf := 1;
      Cf := MIN (-decpt, p); 
      Df := MIN (digits, p - Cf);
      Ef := MAX (0, p - (Cf + Df));
    ELSIF decpt <= digits THEN
      Af := decpt;
      Bf := 0;
      Cf := 0;
      Df := MIN (digits - decpt, p);
      Ef := MAX (0, p - Df);
    ELSE
      Af := digits;
      Bf := decpt - digits;
      Cf := p;
      Df := 0;
      Ef := 0; END; 
    IF style = Style.AltFlo OR style = Style.Mix THEN
      Ef := 0;
      WHILE Df > 0 AND df [Af + Df - 1] = '0' DO
        DEC (Df); END;
      IF Df = 0 THEN
        Cf := 0; END; END; END;

  IF style = Style.Sci OR style = Style.AltSci OR style = Style.Mix THEN
    mode_i := 2;
    p_i := p + 1;
    start := dtoa (v, mode_i, p_i, ADR(decpt), ADR(sign), ADR (end));
    digits := (end - start) DIV ADRSIZE (CHAR);
    
    ds := NEW (REF ARRAY OF CHAR, digits);
    FOR i := 0 TO digits - 1 DO
      ds [i] := LOOPHOLE(start, UNTRACED REF CHAR)^;
      INC (start, ADRSIZE (CHAR)); END;

    IF decpt = 9999 THEN (* Special value returned by dtoa *)
      IF sign # 0 THEN
        buf [0] := '-'; 
        len := 1;
      ELSE
        len := 0; END;
      SUBARRAY (buf, len, digits) := ds^;
      RETURN (len + digits); END;

    As := MIN (1, digits);
    Bs := 0;
    Cs := 0;
    Ds := MIN (digits - 1, p);
    Es := MAX (0, p - digits + 1);
    Xs := decpt - 1;
    IF style = Style.AltSci OR style = Style.Mix THEN
      Es := 0;
      WHILE Ds > 0 AND ds [As + Ds - 1] = '0' DO
        DEC (Ds); END;
      IF Ds = 0 THEN
        Cs := 0; END; END; END;

  IF style = Style.Mix THEN
    VAR  floLength := Af + Bf + 1 + Cf + Df + Ef;
         sciLength := As + Bs + 1 + Cs + Ds + Es; BEGIN
      IF Xs # LAST (INTEGER) THEN
        IF Xs < 0 THEN 
          INC (sciLength, 3);
          VAR x := -X; BEGIN
            WHILE x >= 10 DO 
              INC (sciLength); 
              x := x DIV 10; END; END;
        ELSE
          INC (sciLength, 2);
          VAR x := X; BEGIN
            WHILE x >= 10 DO 
              INC (sciLength); 
              x := x DIV 10; END; END; END; END;
      IF floLength > sciLength THEN
        style := Style.AltSci;
      ELSE
        style := Style.AltFlo;
        IF Df = 0 AND Cf = 0 THEN 
          Pf := FALSE; END; END; END; END;

  IF style = Style.Flo OR style = Style.AltFlo THEN
    A := Af; B := Bf; C := Cf; D := Df; E := Ef; X := Xf; P := Pf; d := df;
  ELSE
    A := As; B := Bs; C := Cs; D := Ds; E := Es; X := Xs; P := Ps; d := ds;END;
  
  (* Set the sign *)
  IF sign # 0 THEN
    buf [len] := '-';
    INC (len); END;

  consumed := 0;
  FOR i := 1 TO A DO 
    buf [len] := d [consumed]; INC (len); INC (consumed); END;
  FOR i := 1 TO B DO
    buf [len] := '0'; INC (len); END;
  IF P THEN
    buf [len] := '.'; INC (len); END;
  FOR i := 1 TO C DO
    buf [len] := '0'; INC (len); END;
  FOR i := 1 TO D DO 
    buf [len] := d [consumed]; INC (len); INC (consumed); END;
  FOR i := 1 TO E DO
    buf [len] := '0'; INC (len); END;
  IF X # LAST (INTEGER) THEN
    buf [len] := exponentChar; INC (len);
    INC (len, FromInt (SUBARRAY (buf, len, NUMBER (buf) - len), X)); END;

  RETURN (len);
END InternalFromLongFloat;


PROCEDURE ToInt (READONLY buf  : Buffer;
                      VAR used : INTEGER;
                          default_base : Base := 10): INTEGER   RAISES {} =
  VAR
    value: Word.T;
    skipped := 0;
  BEGIN
    IF NUMBER (buf) = 0 THEN
      used := 0;
      RETURN 0; END;

    IF buf [0] = '-' THEN
      skipped := 1;
      value := InternalToInt (SUBARRAY (buf, 1, NUMBER (buf) - 1),
                              used, default_base, 
                              Word.LeftShift (1, BITSIZE (INTEGER) - 1));
      value := Word.Plus (1, Word.Not (value));
    ELSE
      IF buf [0] = '+' THEN
        skipped := 1; END;
      value := InternalToInt (SUBARRAY (buf, skipped, NUMBER (buf) - skipped),
                              used, default_base, 
                              Word.RightShift (Word.LeftShift (Word.Not (0),
                                                               1), 1)); END;
    IF used # 0 THEN
      INC (used, skipped); END;
    RETURN value;
  END ToInt;

PROCEDURE ToUnsigned (READONLY buf  : Buffer;
                           VAR used : INTEGER;
                               default_base : Base := 10): INTEGER  RAISES {} =
  VAR
    value: Word.T;
    skipped := 0;
  BEGIN
    IF NUMBER (buf) = 0 THEN
      used := 0;
      RETURN 0; END;

    IF buf [0] = '+' THEN
      skipped := 1; END;
    value := InternalToInt (SUBARRAY (buf, skipped, NUMBER (buf) - skipped),
                            used, default_base,
                            Word.Not (0));
    IF used # 0 THEN
      INC (used, skipped); END;
    RETURN value;
  END ToUnsigned;

PROCEDURE InternalToInt (READONLY buf  : Buffer;
                         VAR used : INTEGER;
                         default_base : Base := 10;
                         limit: Word.T): Word.T  RAISES {} =

  VAR
    value : Word.T := 0;  (* accumulated value *)
    val2  : Word.T;
    n     : INTEGER  := 0;  (* index of current digit *)
    z     : INTEGER  := NUMBER (buf);
    ibase : INTEGER;
    based : BOOLEAN;
    i, j  : INTEGER;
    c     : CHAR;
  BEGIN
    IF z = 0 THEN
      used := 0;
      RETURN 0; END;
    c := buf [0];

    (* peel off any leading zeros *)
    WHILE (c = '0') DO
      INC (n);
      IF (n >= z) THEN used := n;  RETURN 0 END;
      c := buf[n];
    END;

    (* check for an explicit base *)
    IF (c = '1') AND (n+3 < z) AND (buf[n+2] = '_')
      AND ('0' <= buf[n+1]) AND (buf[n+1] <= '6') THEN
      (* an explicit base between 10 and 16 *)
      based := TRUE;
      ibase := 10 + ORD (buf[n+1]) - ORD ('0');
      INC (n, 3);
      c := buf[n];
    ELSIF ('2' <= c) AND (c <= '9') AND (n+2 < z) AND (buf[n+1] = '_') THEN
      (* an explicit base between 2 and 9 *)
      based := TRUE;
      ibase := ORD (c) - ORD ('0');
      INC (n, 2);
      c := buf[n];
    ELSE
      (* no explicit base *)
      based := FALSE;
      ibase := default_base; END;

    (* scan the digits *)
    j := n;  (* remember the first digit *)
    LOOP
      IF    ('0' <= c) AND (c <= '9') THEN  i := ORD (c) - ORD ('0');
      ELSIF ('A' <= c) AND (c <= 'F') THEN  i := ORD (c) - ORD ('A') + 10;
      ELSIF ('a' <= c) AND (c <= 'f') THEN  i := ORD (c) - ORD ('a') + 10;
      ELSE  EXIT;
      END;
      IF (i >= ibase) THEN EXIT END;
      IF Word.LT (Word.Divide (limit, ibase), value) THEN
        EXIT; END;
      val2 := Word.Times (value, ibase);  (* no overflow *)
      IF Word.LT (Word.Minus (limit, i), value) THEN
        EXIT; END;
      value := Word.Plus (val2, i);	 (* no overflow *)
      INC (n);
      IF (n >= z) THEN EXIT END;
      c := buf [n]; END;

    IF (j = n) AND (based) THEN  (* no digits were consumed *)
      (* back up and "rescan" the explicit base *)
      IF (ibase < 10) THEN (* single digit base was specified *)
        DEC (n); (* return the "_" *)
        IF (ibase < default_base) THEN (* digit is legal *)
          value := ibase;
        ELSE (* an illegal digit was specified *)
          DEC (n);  (* return the digit *)
          (* value remains 0 *) END;
      ELSE (* 2-digit base was specified *)
	(* first digit was '1' and is always legal *)
        DEC (n); (* return the "_" *)
        IF (ibase-10 < default_base) THEN (* the second digit was legal *)
          value := default_base + ibase - 10;
	ELSE (* the second digit was illegal *)
	  DEC (n);  (* return the second digit *)
	  value := 1; END; END; END;

    used := n;
    RETURN value;
  END InternalToInt;


(* Note: in ToFloat and friends, buf does not need to be terminated by a
   \0, so we have to copy the characters in a local array.  We also need
   to change the exponent characters to 'e'. *)

TYPE BufPtr = UNTRACED REF Buffer;

PROCEDURE ToFloat (READONLY buf  : Buffer;
                        VAR used : INTEGER): REAL  RAISES {Failed} =
  VAR
    value  : LONGREAL;
    chars  : BufPtr;
    tmp    : ARRAY [0..31] OF CHAR;
    ok     : BOOLEAN;
    nchars := NUMBER (buf);
  BEGIN
    IF (nchars < NUMBER (tmp)) THEN
      ok := ToBinary (buf, 'E', 'E', tmp, used, value);
    ELSE (* we don't have enough space in 'tmp' *)
      chars := NEW (BufPtr, nchars + 1);
      ok := ToBinary (buf, 'E', 'E', chars^, used, value);
      DISPOSE (chars);
    END;
    IF (ok)
      THEN RETURN FLOAT (value);
      ELSE RAISE Failed;
    END;
  END ToFloat;

PROCEDURE ToLongFloat (READONLY buf  : Buffer;
                            VAR used : INTEGER): LONGREAL  RAISES {Failed} =
  VAR
    value  : LONGREAL;
    chars  : BufPtr;
    tmp    : ARRAY [0..31] OF CHAR;
    ok     : BOOLEAN;
    nchars := NUMBER (buf);
  BEGIN
    IF (nchars < NUMBER (tmp)) THEN
      ok := ToBinary (buf, 'D', 'd', tmp, used, value);
    ELSE (* we don't have enough space in 'tmp' *)
      chars := NEW (BufPtr, nchars + 1);
      ok := ToBinary (buf, 'D', 'd', chars^, used, value);
      DISPOSE (chars);
    END;
    IF (ok)
      THEN RETURN value;
      ELSE RAISE Failed;
    END;
  END ToLongFloat;

PROCEDURE ToExtended (READONLY buf  : Buffer;
                           VAR used : INTEGER): EXTENDED  RAISES {Failed} =
  VAR
    value  : LONGREAL;
    chars  : BufPtr;
    tmp    : ARRAY [0..31] OF CHAR;
    ok     : BOOLEAN;
    nchars := NUMBER (buf);
  BEGIN
    IF (nchars < NUMBER (tmp)) THEN
      ok := ToBinary (buf, 'X', 'x', tmp, used, value);
    ELSE (* we don't have enough space in 'tmp' *)
      chars := NEW (BufPtr, nchars + 1);
      ok := ToBinary (buf, 'X', 'x', chars^, used, value);
      DISPOSE (chars);
    END;
    IF (ok)
      THEN RETURN FLOAT (value, EXTENDED);
      ELSE RAISE Failed;
    END;
  END ToExtended;

PROCEDURE ToBinary (READONLY source : Buffer;
                         exp1, exp2 : CHAR;
                         VAR tmp    : Buffer;
                         VAR used   : INTEGER;
                         VAR value  : LONGREAL): BOOLEAN =
  VAR ch: CHAR;  eptr: ADDRESS;  nchars: INTEGER := NUMBER (source);
  BEGIN
    (* copy source to tmp, fix the exponent character and null terminate *)
    FOR i := 0 TO nchars -1 DO
      ch := source [i];
      IF (ch = exp1) OR (ch = exp2) THEN ch := 'e' END;
      tmp [i] := ch;
    END;
    tmp [nchars] := '\000';

    (* finally, do the conversion *)
    value := strtod (ADR (tmp [0]), eptr);
    IF eptr = LOOPHOLE (0, ADDRESS)
      THEN RETURN FALSE;
      ELSE used := eptr - ADR (tmp [0]); RETURN TRUE;
    END;
  END ToBinary;

BEGIN
END Convert.
