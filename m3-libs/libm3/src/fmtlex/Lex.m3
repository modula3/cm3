(* Copyright (C) 1990, Digital Equipment Corporation.			     *)
(* All rights reserved.							     *)
(* See the file COPYRIGHT for a full description.			     *)
(*									     *)
(* Last modified on Thu Jan 26 14:06:30 PST 1995 by kalsow       *)
(*      modified on Tue Mar 22 15:25:28 PST 1994 by heydon		     *)
(*      modified on Tue May 11 15:44:41 PDT 1993 by mcjones		     *)
(*      modified on Thu Apr 29 16:40:03 PDT 1993 by muller                   *)

MODULE Lex;
IMPORT IEEESpecial, FloatMode, Rd, Word, Text;
IMPORT RealFloat, LongFloat, ExtendedFloat;
FROM Thread IMPORT Alerted;

PROCEDURE Scan(rd: Rd.T; READONLY cs: SET OF CHAR := NonBlanks): TEXT
    RAISES {Rd.Failure, Alerted} =
  CONST BufSize = 256;
  VAR res := ""; i := 0; c: CHAR; buf: ARRAY [0..BufSize-1] OF CHAR; BEGIN
    TRY
      LOOP
	c := Rd.GetChar(rd);
	IF NOT (c IN cs) THEN
	  Rd.UnGetChar(rd);
	  EXIT
	END;
	IF i = BufSize THEN
	  res := res & Text.FromChars(buf);
	  i := 0
	END;
	buf[i] := c;
	INC(i)
      END
    EXCEPT
      Rd.EndOfFile => (* SKIP *)
    END;
    RETURN res & Text.FromChars(SUBARRAY(buf, 0, i))
  END Scan;

PROCEDURE Skip(rd: Rd.T; READONLY cs: SET OF CHAR := Blanks)
    RAISES {Rd.Failure, Alerted} =
  BEGIN
    TRY
      LOOP
	IF NOT (Rd.GetChar(rd) IN cs) THEN
	  Rd.UnGetChar(rd);
	  RETURN
	END
      END
    EXCEPT Rd.EndOfFile => (* SKIP *)
    END
  END Skip;

PROCEDURE Match(rd: Rd.T; t: TEXT) RAISES {Error, Rd.Failure, Alerted} =
  BEGIN
    TRY
      FOR i := 0 TO Text.Length(t) - 1 DO
	IF Rd.GetChar(rd) # Text.GetChar(t, i) THEN
	  Rd.UnGetChar(rd);
	  RAISE Error
	END
      END
    EXCEPT
      Rd.EndOfFile => RAISE Error
    END
  END Match;

PROCEDURE Bool(rd: Rd.T): BOOLEAN RAISES {Error, Rd.Failure, Alerted} =
  PROCEDURE M(c: ['A'..'Z']) RAISES {Error, Rd.Failure, Rd.EndOfFile, Alerted}=
    CONST UpperToLower = ORD('a') - ORD('A');
    VAR c1: CHAR; BEGIN
      c1 := Rd.GetChar(rd);
      IF c1 # c AND c1 # VAL(ORD(c) + UpperToLower, CHAR) THEN
        Rd.UnGetChar(rd); RAISE Error
      END
    END M;
  BEGIN
    Skip(rd, Blanks);
    TRY
      CASE Rd.GetChar(rd) OF
      | 'F', 'f' => M('A'); M('L'); M('S'); M('E'); RETURN FALSE
      | 'T', 't' => M('R'); M('U'); M('E'); RETURN TRUE
      ELSE Rd.UnGetChar(rd); RAISE Error
      END
    EXCEPT
      Rd.EndOfFile => RAISE Error
    END
  END Bool;

(* Implementation Note:

   When possible, this implementation uses one-character look-ahead and
   exceptions to avoid calls to "Rd.UnGetChar" and unnecessary tests in the
   fast case.

   The following procedures attempt to read characters representing non-
   terminals in the grammar defining "Number"'s and "Float"'s in the "Lex"
   interface. These procedures often take a "Rd.T" parameter "rd" along with a
   "VAR" character "c". The semantics of these procedures is to read from the
   conceptual stream of characters formed by concatenating "c" with the stream
   of characters represented by "rd", and to set "c" on exit from the
   procedure to the character after the last character that was processed.
   Those procedures that take the character "c" as a "VALUE" parameter exit
   with "rd" containing all unprocessed characters (in particular, such
   procedures call "Rd.UnGetChar" if they cannot process the character "c").

   Some of these procedures raise "Rd.EndOfFile" while some do not. The
   general convention is that those procedures representing non-terminals that
   *must* be followed by at least one more character (because of their place
   in the grammar) will raise "Rd.EndOfFile". For example, the "ReadSign"
   procedure represents the optional "Sign" non-terminal, and in expansions of
   both the "Number" and "Float" non-terminal, it must be followed by at least
   one more character. On the other hand, the "ReadUnsigned" procedure, which
   reads an unsigned number represented by a string of digits, need not be
   followed by any characters, so if end-of-file is encountered, it returns
   success so long as at least one digit has been read. *)

PROCEDURE ReadSign(rd: Rd.T; VAR (*INOUT*) c: CHAR): [0..1]
    RAISES {Rd.EndOfFile, Rd.Failure, Alerted} =
(* If "c" is the character "+" or "-", then read the next character from "rd"
   into "c", and return 0 or 1, respectively. Otherwise, return 0. *)
  BEGIN
    CASE c OF
      '+' => c := Rd.GetChar(rd); RETURN 0
    | '-' => c := Rd.GetChar(rd); RETURN 1
    ELSE RETURN 0
    END
  END ReadSign;

PROCEDURE ReadUnsigned(rd: Rd.T; c: CHAR; base: [2..16]; noBasedInt := FALSE):
    Word.T RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted} =
(* Read an unsigned integer of the form "SmallBaseNum" or "BigBaseNum" (see
   the specification for "Lex.Integer") as "base" is at most 10 or exceeds 10,
   respectively. If "noBasedInt", then don't allow an explicit base prefix
   (i.e., read an unsigned integer of the form "DecVal" or "HexVal" as "base"
   is at most 10 or exceeds 10, respectively).

   Raises "Error" if no legal digits were read, or if some digit in the stream
   is not a legal digit in the chosen base. Raises "FloatMode.Trap" with
   argument "FloatMode.Flag.IntOverflow" if the value exceeds the largest
   value representable by a "Word.T". *)
  CONST
    MaxWord = Word.Not(0);
  VAR
    digit: [0..15];
    digitsSeen, specBase: CARDINAL := 0;
    seenUnderscore := noBasedInt;
    badDigit, overflow := FALSE;
    fastUB := Word.Divide(Word.Minus(MaxWord, base-1), base);
    slowUB := Word.Divide(MaxWord, base);
    res: Word.T := 0;
  BEGIN
    LOOP
      CASE c OF
        '0'..'9' =>
          digit := ORD(c) - ORD('0');
          IF digitsSeen < 2 THEN specBase := specBase * 10 + digit END
      | 'a'..'f' =>
          IF base <= 10 THEN Rd.UnGetChar(rd); EXIT END;
          digit := ORD(c) - ORD('a') + 10;
      | 'A'..'F' =>
          IF base <= 10 THEN Rd.UnGetChar(rd); EXIT END;
          digit := ORD(c) - ORD('A') + 10;
      | '_' =>
          (* finish if the preceeding base is not one of "2", "3", ..., "16" *)
          IF seenUnderscore OR digitsSeen > 2
             OR specBase < 2 OR specBase > 16
             OR (digitsSeen > 1 AND specBase < 10) THEN
            Rd.UnGetChar(rd); EXIT
          END;
          seenUnderscore := TRUE;
          (* reinitialize state for new base*)
          base := specBase;
          fastUB := Word.Divide(Word.Minus(MaxWord, base-1), base);
          slowUB := Word.Divide(MaxWord, base);
          digitsSeen := 0;
          badDigit := FALSE;
          overflow := FALSE;
          res := 0;
      ELSE Rd.UnGetChar(rd); EXIT
      END;
      IF c # '_' THEN
	INC(digitsSeen);
	IF digit >= base THEN
	  badDigit := TRUE
	ELSIF Word.LE(res, fastUB) THEN
          (* fast path *)
	  res := Word.Plus(Word.Times(res, base), digit)
        ELSIF Word.LE(res, slowUB) THEN
          (* slow path *)
          res := Word.Times(res, base);
          IF digit <= Word.Minus(MaxWord, res)
            THEN res := Word.Plus(res, digit)
            ELSE overflow := TRUE
          END
        ELSE
	  overflow := TRUE
	END
      END;
      TRY c := Rd.GetChar(rd) EXCEPT Rd.EndOfFile => EXIT END
    END;
    IF digitsSeen = 0 OR badDigit THEN RAISE Error END;
    IF overflow THEN RAISE FloatMode.Trap(FloatMode.Flag.IntOverflow) END;
    RETURN res
  END ReadUnsigned;

PROCEDURE ReadNumber(rd: Rd.T; defaultBase: [2..16]; signed: BOOLEAN): Word.T
    RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted} =
(* Raises "FloatMode.Trap(FloatMode.Flag.IntOverflow)" if the read number
   exceeds a "Word.T" or if "signed = TRUE" and the read number exceeds
   "LAST(INTEGER)" when the sign is positive or exceeds "-FIRST(INTEGER)" when
   the sign is negative. *)
  VAR c: CHAR; sign: [0..1]; res: Word.T; BEGIN
    Skip(rd, Blanks);
    TRY
      c := Rd.GetChar(rd);
      IF signed
        THEN sign := ReadSign(rd, c)
        ELSE sign := 0
      END
    EXCEPT
      Rd.EndOfFile => RAISE Error
    END;
    res := ReadUnsigned(rd, c, defaultBase);
    IF signed AND
       ((sign = 0 AND Word.GT(res, LAST(INTEGER))) OR
        (sign = 1 AND Word.GT(res, -FIRST(INTEGER)))) THEN
      RAISE FloatMode.Trap(FloatMode.Flag.IntOverflow)
    END;
    IF sign = 1 THEN res := - res END;
    RETURN res
  END ReadNumber;

PROCEDURE Int(rd: Rd.T; defaultBase: [2..16] := 10): INTEGER 
    RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted} =
  BEGIN RETURN ReadNumber(rd, defaultBase, signed := TRUE) END Int;

PROCEDURE Unsigned(rd: Rd.T; defaultBase: [2..16] := 16): Word.T
    RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted} =
  BEGIN RETURN ReadNumber(rd, defaultBase, signed := FALSE) END Unsigned;

CONST
  DigitBufSz = 40;

TYPE
  Digits = ARRAY OF [0..9];
  DigitBuf = ARRAY [0..DigitBufSz-1] OF [0..9];

PROCEDURE ReadFloVal(
    rd: Rd.T;
    VAR (*INOUT*) c: CHAR;
    VAR (*OUT*) prefix: REF Digits;
    VAR (*OUT*) digits: DigitBuf;
    VAR (*OUT*) digCnt: CARDINAL;
    VAR (*OUT*) exp: INTEGER): BOOLEAN
    RAISES {Rd.Failure, Rd.EndOfFile, Alerted} =
(* Read a "FloVal" (as defined in the BNF for a "Float") from the character
   stream formed by concatenating "c" with the stream "rd". If the number of
   digits in this "FloVal" is at most "DigitBufSz", then set "prefix" to
   "NIL", and store the digits in "digits" and the number of digits read in
   "digCnt". Otherwise (the slow path), "prefix" is set to a buffer of the
   prefix digits; the remaining digits are in "SUBARRAY(digits, 0, digCnt)".
   The value "exp" is set to the power of 10 to which the read digits
   *with a demimal point inserted just after the first digit* must be raised
   to equal the "FloVal" that was read. For example:

|    FloVal read   digits[]    digCnt    exp
|    -----------   --------    ------    ---
|    "1.234"       1234        4          0
|    "0.1234"      1234        4         -1
|    "12.34"       1234        4          1
|    "0.00012"     12          2         -4
|    "00012.3"     123         3          1

   Notice that this implementation ignores leading zeroes, so if there are any
   non-zero digits, then "digits[0] # 0".

   Returns TRUE iff end-of-file was encountered after first reading a valid
   "FloVal". In this case, the value of the INOUT parameter "c" on return is
   undefined. Otherwise, "c" is the next unprocessed character on "rd".

   Raises "Rd.EndOfFile" if end-of-file was encountered before reading a valid
   "FloVal". *)

  VAR sawZero, foundDecimal := FALSE;

  PROCEDURE SkipZeros() RAISES {Rd.EndOfFile, Rd.Failure, Alerted} =
  (* Skip leading zeros and at most one decimal point in the character stream 
     formed by concatenating "c" with the stream "rd". Set "sawZero" to TRUE
     iff at least one zero digit was skipped. Set "foundDecimal" to TRUE iff a
     decimal point was skipped. If the stream does not contain a decimal
     point, then set "exp" to 0. Otherwise, set "exp" to the negation of the
     number of zeros read after the decimal.

     If this routine reads at least one new character, then "sawZero OR
     foundDecimal". *)
    BEGIN
      LOOP
	CASE c OF
	  '0' =>
	    sawZero := TRUE;
	    IF foundDecimal THEN DEC(exp) END
	| '.' =>
	    IF foundDecimal
	      THEN RETURN
	      ELSE foundDecimal := TRUE
	    END
	ELSE RETURN
	END;
	c := Rd.GetChar(rd)
      END
    END SkipZeros;

  PROCEDURE AppendToPrefix() =
  (* Append the digits in "digits[0..digCnt-1]" to the "prefix" array. *)
    VAR new: REF ARRAY OF [0..9]; BEGIN
      IF prefix = NIL THEN
        new := NEW(REF ARRAY OF [0..9], digCnt);
      ELSE
        new := NEW(REF ARRAY OF [0..9], NUMBER(prefix^) + digCnt);
        SUBARRAY(new^, 0, NUMBER(prefix^)) := prefix^
      END;
      prefix := new;
      SUBARRAY(prefix^, NUMBER(prefix^) - digCnt, digCnt) :=
        SUBARRAY(digits, 0, digCnt)
    END AppendToPrefix;

  PROCEDURE ReadDigits() RAISES {Rd.EndOfFile, Rd.Failure, Alerted} =
  (* Read decimal digits into "digits", setting "foundDecimal" if a decimal
     point is seen, and incrementing "exp" for each digit seen before a
     decimal point.

     Note: If "digCnt = 0 AND prefix = NIL" on entry, then on any sort of
     return, "digCnt = 0 => prefix = NIL". *)
    BEGIN
      <* ASSERT c # '0' *>
      LOOP
	CASE c OF
	  '0'..'9' =>
            IF NOT foundDecimal THEN INC(exp) END;
            IF digCnt = DigitBufSz THEN
              AppendToPrefix();
              digCnt := 0
            END;
            digits[digCnt] := ORD(c) - ORD('0');
            INC(digCnt)
	| '.' =>
	    IF foundDecimal THEN RETURN END;
	    foundDecimal := TRUE
	ELSE RETURN
	END;
	c := Rd.GetChar(rd)
      END
    END ReadDigits;

  (* ReadFloVal *)
  VAR res := FALSE; BEGIN
    TRY
      exp := -1;
      SkipZeros();
      prefix := NIL;
      digCnt := 0;
      ReadDigits()
    EXCEPT
      Rd.EndOfFile =>
        IF digCnt = 0 AND NOT sawZero THEN
          RAISE Rd.EndOfFile
        END;
        res := TRUE
    END;
    IF digCnt = 0 AND sawZero THEN
      digits[0] := 0; INC(digCnt); INC(exp)
    END;
    RETURN res
  END ReadFloVal;

EXCEPTION IntOverflow(BOOLEAN);
(* The exception is raised in the event of an integer overflow; the Boolean
   argument is TRUE iff the preceeding sign was negative. *)

PROCEDURE ReadExponent(rd: Rd.T; c: CHAR): INTEGER
    RAISES {Error, IntOverflow, Rd.Failure, Alerted} =
(* Read an optional "Exp" as defined in the BNF for a "Float". If "c" is not
   one of the valid first characters of an "Exp", return 0. Since "c" is not a
   VAR parameter, this routine must also guarantee than the last read but
   unprocessed character is prepended to "rd". *)
  VAR sign: [0..1]; val: INTEGER; BEGIN
    TRY
      CASE c OF
	'd', 'e', 'x', 'D', 'E', 'X' => c := Rd.GetChar(rd)
      ELSE Rd.UnGetChar(rd); RETURN 0
      END;
      sign := ReadSign(rd, c)
    EXCEPT
      Rd.EndOfFile => RAISE Error
    END;
    (* read the exponent; if it overflows a Word.T or is bigger than
       LAST(INTEGER), then it is obviously too large *)
    TRY
      val := ReadUnsigned(rd, c, base := 10, noBasedInt := TRUE);
      IF Word.GT(val, LAST(INTEGER)) THEN
        RAISE FloatMode.Trap(FloatMode.Flag.IntOverflow)
      END
    EXCEPT
      FloatMode.Trap (flag) =>
        <* ASSERT flag = FloatMode.Flag.IntOverflow *>
        RAISE IntOverflow(sign = 1)
    END;
    IF sign = 1 THEN val := -val END;
    RETURN val
  END ReadExponent;

TYPE SpecialKind = { NaN, PosInf, NegInf };
EXCEPTION SpecialReal(SpecialKind);

PROCEDURE ReadReal(
    rd: Rd.T;
    VAR (*OUT*) sign: [0..1];
    VAR (*OUT*) digits: ARRAY OF [0..9];
    VAR (*OUT*) digCnt: CARDINAL;
    VAR (*OUT*) exp: INTEGER):
    REF ARRAY OF [0..9]
    RAISES {SpecialReal, Error, FloatMode.Trap, Rd.Failure, Alerted} =
(* Common entry point for parsing Real, LongReal, and Extended floating point
   numbers. *)

  PROCEDURE M(c: ['A'..'Z']) RAISES {Error, Rd.Failure, Alerted} =
  (* Read the next character from "rd". If it is "c" or the lower-case
     character corresponding to "c", then return. Raises "Error" on
     end-of-file or if the match failed. In the latter case, the bad character
     is returned to the reader. *)
    CONST UpperToLower = ORD('a') - ORD('A');
    VAR c1: CHAR; BEGIN
      TRY c1 := Rd.GetChar(rd) EXCEPT Rd.EndOfFile => RAISE Error END;
      IF c1 # c AND c1 # VAL(ORD(c) + UpperToLower, CHAR) THEN
        Rd.UnGetChar(rd); RAISE Error
      END
    END M;

  PROCEDURE Inf(s: [0..1]): SpecialKind =
    BEGIN
      CASE s OF
        0 => RETURN SpecialKind.PosInf
      | 1 => RETURN SpecialKind.NegInf
      END
    END Inf;

  (* ReadReal *)
  VAR c: CHAR; prefix: REF ARRAY OF [0..9]; BEGIN
    exp := 0;
    Skip(rd, Blanks);
    TRY
      c := Rd.GetChar(rd);
      sign := ReadSign(rd, c);
      IF ReadFloVal(rd, c, prefix, digits, digCnt, exp) THEN
        RETURN prefix
      END;
    EXCEPT
      Rd.EndOfFile => RAISE Error
    END;
    IF digCnt = 0 THEN
      <* ASSERT prefix = NIL *>
      IF NOT FloatMode.IEEE THEN
        Rd.UnGetChar(rd);
        RAISE Error
      END;
      (* check for NAN, INFINITY here *)
      CASE c OF
        'I', 'i' =>
          M('N'); M('F');
          TRY
            c := Rd.GetChar(rd);
            CASE c OF
              'I', 'i' => M('N'); M('I'); M('T'); M('Y')
            ELSE Rd.UnGetChar(rd)
            END
          EXCEPT Rd.EndOfFile => (* SKIP *)
          END;
          RAISE SpecialReal(Inf(sign))
      | 'N', 'n' =>
          M('A'); M('N');
          RAISE SpecialReal(SpecialKind.NaN)
      ELSE Rd.UnGetChar(rd); RAISE Error
      END
    END;
    TRY exp := exp + ReadExponent(rd, c) EXCEPT IntOverflow (neg) =>
      IF neg
        THEN RAISE FloatMode.Trap(FloatMode.Flag.Underflow)
        ELSE RAISE FloatMode.Trap(FloatMode.Flag.Overflow)
      END
    END;
    RETURN prefix
  END ReadReal;

PROCEDURE ConcatDigits(READONLY d1, d2: ARRAY OF [0..9]): REF ARRAY OF [0..9] =
(* Return a new array of digits containing the concatenation of the digits in
   "d1" and "d2". *)
  VAR
    d1Cnt := NUMBER(d1); d2Cnt := NUMBER(d2);
    res := NEW(REF ARRAY OF [0..9], d1Cnt + d2Cnt);
  BEGIN
    SUBARRAY(res^, 0, d1Cnt) := d1;
    SUBARRAY(res^, d1Cnt, d2Cnt) := d2;
    RETURN res
  END ConcatDigits;

PROCEDURE Real(rd: Rd.T): REAL
    RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted} =
  VAR
    sign: [0..1];
    prefix: REF ARRAY OF [0..9];
    digits: DigitBuf;
    digCnt: CARDINAL;
    exp: INTEGER;
  BEGIN
    TRY
      prefix := ReadReal(rd, sign, digits, digCnt, exp);
      IF prefix = NIL THEN
	RETURN RealFloat.FromDecimal(sign, SUBARRAY(digits, 0, digCnt), exp)
      ELSE
	VAR digs := ConcatDigits(prefix^, SUBARRAY(digits, 0, digCnt)); BEGIN
	  RETURN RealFloat.FromDecimal(sign, digs^, exp)
	END
      END
    EXCEPT SpecialReal(kind) =>
      CASE kind OF
        SpecialKind.NegInf => RETURN IEEESpecial.RealNegInf
      | SpecialKind.PosInf => RETURN IEEESpecial.RealPosInf
      | SpecialKind.NaN    => RETURN IEEESpecial.RealNan
      END
    END
  END Real;

PROCEDURE LongReal(rd: Rd.T): LONGREAL 
    RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted} =
  VAR
    sign: [0..1];
    prefix: REF ARRAY OF [0..9];
    digits: DigitBuf;
    digCnt: CARDINAL;
    exp: INTEGER;
  BEGIN
    TRY
      prefix := ReadReal(rd, sign, digits, digCnt, exp);
      IF prefix = NIL THEN
	RETURN LongFloat.FromDecimal(sign, SUBARRAY(digits, 0, digCnt), exp)
      ELSE
	VAR digs := ConcatDigits(prefix^, SUBARRAY(digits, 0, digCnt)); BEGIN
	  RETURN LongFloat.FromDecimal(sign, digs^, exp)
	END
      END
    EXCEPT SpecialReal(kind) =>
      CASE kind OF
        SpecialKind.NegInf => RETURN IEEESpecial.LongNegInf
      | SpecialKind.PosInf => RETURN IEEESpecial.LongPosInf
      | SpecialKind.NaN    => RETURN IEEESpecial.LongNan
      END
    END
  END LongReal;

PROCEDURE Extended(rd: Rd.T): EXTENDED 
    RAISES {Error, FloatMode.Trap, Rd.Failure, Alerted} =
  VAR
    sign: [0..1];
    prefix: REF ARRAY OF [0..9];
    digits: DigitBuf;
    digCnt: CARDINAL;
    exp: INTEGER;
  BEGIN
    TRY
      prefix := ReadReal(rd, sign, digits, digCnt, exp);
      IF prefix = NIL THEN
	RETURN ExtendedFloat.FromDecimal(sign,SUBARRAY(digits, 0, digCnt),exp)
      ELSE
	VAR digs := ConcatDigits(prefix^, SUBARRAY(digits, 0, digCnt)); BEGIN
	  RETURN ExtendedFloat.FromDecimal(sign, digs^, exp)
	END
      END
    EXCEPT SpecialReal(kind) =>
      CASE kind OF
        SpecialKind.NegInf => RETURN IEEESpecial.ExtdNegInf
      | SpecialKind.PosInf => RETURN IEEESpecial.ExtdPosInf
      | SpecialKind.NaN    => RETURN IEEESpecial.ExtdNan
      END
    END
  END Extended;

BEGIN END Lex.
