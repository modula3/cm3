(* Copyright (C) 1994, Digital Equipment Corporation.       *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Mon Nov 21 08:21:27 PST 1994 by kalsow  *)
(*      modified on Fri Apr  9 09:54:23 PDT 1993 by muller  *)

UNSAFE MODULE RTIO;

IMPORT TextF, Word, RT0, RTOS;

VAR
  len : INTEGER := 0;
  buf : ARRAY [0..1023] OF CHAR;

PROCEDURE PutChar (c: CHAR) =
  BEGIN
    IF (len > LAST (buf)) THEN Flush (); END;
    buf [len] := c;
    INC (len);
  END PutChar;

PROCEDURE PutChars (a: ADDRESS;  n: INTEGER) =
  VAR p : RT0.String := a;
  BEGIN
    WHILE (n > 0) DO
      PutChar (p^);
      INC (p, BYTESIZE (p^));
      DEC (n);
    END;
  END PutChars;

PROCEDURE PutString (s: ADDRESS) =
  VAR p : RT0.String := s;
  BEGIN
    IF (p = NIL) THEN RETURN; END;
    WHILE (p^ # '\000') DO
      PutChar (p^);
      INC (p, BYTESIZE (p^));
    END;
  END PutString;

PROCEDURE PutInt (i: INTEGER;  width: INTEGER) =
  VAR
    num : ARRAY [0..30] OF CHAR;
    len := FromInt (ADR (num[0]), i, 10);
  BEGIN
    FOR i := 1 TO width - len DO PutChar (' ') END;
    PutChars (ADR (num[0]), len);
  END PutInt;

PROCEDURE PutHex (i: INTEGER;  width: INTEGER) = 
  VAR
    num : ARRAY [0..30] OF CHAR;
    len := FromUnsigned (ADR (num[2]), i, 16) + 2;
  BEGIN
    FOR i := 1 TO width - len DO PutChar (' ') END;
    num[0] := '0';
    num[1] := 'x';
    PutChars (ADR (num[0]), len);
  END PutHex;

PROCEDURE PutAddr (a: ADDRESS;  width: INTEGER) =
  BEGIN
    PutHex (LOOPHOLE (a, INTEGER), width);
  END PutAddr;

PROCEDURE PutText (t: TEXT) =
  BEGIN
    PutChars (ADR (t[0]), NUMBER (t^) - 1);
  END PutText;

PROCEDURE Flush () =
  BEGIN
    IF (len > 0) THEN
      RTOS.Write (ADR (buf[0]), len);
      len := 0;
    END;
  END Flush;

(*----------------------------------------------------- internal routines ---*)


TYPE  Base = [2..16];
CONST Digits = ARRAY [0..15] OF CHAR {
                   '0', '1', '2', '3', '4', '5', '6', '7',
                   '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' };

PROCEDURE FromInt (buf    : UNTRACED REF CHAR;
                   value  : INTEGER;
                   base   : Base := 10): INTEGER =
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

    (* build the result buffer *)
    j := 0;
    IF (minus)  THEN buf^ := '-';  j := 1; INC (buf, BYTESIZE (buf^)); END;
    FOR k := nDigits-1 TO 0 BY -1 DO
      buf^ := result [k];  INC (j); INC (buf, BYTESIZE (buf^));
    END;

    RETURN j;
  END FromInt;

PROCEDURE FromUnsigned (buf    : UNTRACED REF CHAR;
                        value  : INTEGER;
                        base   : Base := 10): INTEGER =
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

    (* build the result buffer *)
    j := 0;
    FOR k := nDigits-1 TO 0 BY -1 DO
      buf^ := result [k];  INC (j); INC (buf, BYTESIZE (buf^));
    END;

    RETURN j;
  END FromUnsigned;

BEGIN
END RTIO.
