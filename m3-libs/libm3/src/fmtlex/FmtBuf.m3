(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Mar 17 12:37:12 PST 1994 by heydon     *)

MODULE FmtBuf EXPORTS FmtBuf, FmtBufF, FmtBufTest;

IMPORT Text, TextF, Word, Convert, FloatMode, Process;
IMPORT Real AS R, LongReal AS LR, Extended AS ER;
IMPORT RealFloat, LongFloat, ExtendedFloat;

PROCEDURE Int(VAR (*INOUT*) b: T; n: INTEGER; base: Base := 10): CARDINAL =
  <* FATAL Convert.Failed *>
  BEGIN
    RETURN Convert.FromInt(b, n, base, prefix := FALSE)
  END Int;

PROCEDURE Unsigned(VAR (*INOUT*) b: T; n: Word.T; base: Base := 16): CARDINAL =
  <* FATAL Convert.Failed *>
  BEGIN
    RETURN Convert.FromUnsigned(b, n, base, prefix := FALSE)
  END Unsigned;

(* Floating-point values --------------------------------------------------- *)

PROCEDURE Real(
    VAR (*INOUT*) b: T;
    x: REAL;
    style := Style.Auto;
    prec: CARDINAL := R.MaxSignifDigits - 1;
    literal := FALSE)
  : CARDINAL =
  VAR da := RealFloat.ToDecimal(x); num: NumAttr; BEGIN
    num.class := ClassMapReal[da.class];
    num.kind := IEEEKind.Single;
    num.maxExpDigits := R.MaxExpDigits;
    num.sign := da.sign;
    IF num.class = Class.Number THEN
      num.len := da.len;
      num.exp := da.exp;
      num.errorSign := da.errorSign
    END;
    RETURN Float(b, num, da.digits, FmtRec{style, prec, literal})
  END Real;

PROCEDURE LongReal(
    VAR (*INOUT*) b: T;
    x: LONGREAL;
    style := Style.Auto;
    prec: CARDINAL := LR.MaxSignifDigits - 1;
    literal := FALSE)
  : CARDINAL =
  VAR da := LongFloat.ToDecimal(x); num: NumAttr; BEGIN
    num.class := ClassMapLong[da.class];
    num.kind := IEEEKind.Double;
    num.maxExpDigits := LR.MaxExpDigits;
    num.sign := da.sign;
    IF num.class = Class.Number THEN
      num.len := da.len;
      num.exp := da.exp;
      num.errorSign := da.errorSign
    END;
    RETURN Float(b, num, da.digits, FmtRec{style, prec, literal})
  END LongReal;

PROCEDURE Extended(
    VAR (*INOUT*) b: T;
    x: EXTENDED;
    style := Style.Auto;
    prec: CARDINAL := ER.MaxSignifDigits - 1;
    literal := FALSE)
  : CARDINAL =
  VAR da := ExtendedFloat.ToDecimal(x); num: NumAttr; BEGIN
    num.class := ClassMapExtd[da.class];
    num.kind := IEEEKind.Extended;
    num.maxExpDigits := ER.MaxExpDigits;
    num.sign := da.sign;
    IF num.class = Class.Number THEN
      num.len := da.len;
      num.exp := da.exp;
      num.errorSign := da.errorSign
    END;
    RETURN Float(b, num, da.digits, FmtRec{style, prec, literal})
  END Extended;

(* The different styles have the following formats, where:

|  "D" represents a digit,
|  "PP***P" represents a total of "prec" digits,
|  "XX***X" represents a total of T.MaxExpDigits for floating-point type "T",
|  "DD***D" represents a total of "whole" digits

|  Style.Sci: (-| )D.PP***Pe(+|-)XX***X   width = prec + T.MaxExpDigits + 5
|  Style.Fix: [-]DD***D.PP***P            width = prec + whole + 1 [ + 1 ]

   "Style.Auto" has either the "Style.Sci" or the "Style.Fix" format,
   depending on the magnitude of the number. In both cases, if "prec = 0", the
   decimal point is dropped (unless the number is being rendered as a Modula-3
   literal, in which case the decimal is not dropped and an extra zero is
   added after the decimal). *)

PROCEDURE Float(
    VAR (*OUT*) b: T;
    READONLY num: NumAttr;
    VAR (*IN*) digits: Digits;
    READONLY fmt: FmtRec)
  : CARDINAL =
  BEGIN
    CASE num.class OF
      Class.NaN => RETURN NaN(b, num, fmt)
    | Class.Inf => RETURN Inf(b, num, fmt)
    | Class.Zero => RETURN Zero(b, num, fmt)
    | Class.Number => RETURN Number(b, num, digits, fmt)
    END
  END Float;

PROCEDURE SciWidth(READONLY fmt: FmtRec; maxExpDigits: CARDINAL): CARDINAL =
(* Return the width of a number formated for "Style.Sci" with "fmt.prec"
   digits of precision and "maxExpDigits" in the exponent. If "fmt.prec = 0",
   the decimal is dropped unless "fmt.literal = TRUE" *)
  VAR res: CARDINAL := fmt.prec + maxExpDigits + 5; BEGIN
    IF fmt.prec = 0 THEN
      IF fmt.literal
        THEN INC(res)			 (* include extra '0' after '.' *)
        ELSE DEC(res)			 (* don't include '.' *)
      END
    END;
    RETURN res
  END SciWidth;

TYPE
  KindValues = ARRAY IEEEKind OF TEXT;
  TwoKindValues = ARRAY [0..1] OF KindValues;

CONST
  LitSpecials = TwoKindValues{
    KindValues{"0.0/0.0", "0.0d0/0.0d0", "0.0x0/0.0x0"},
    KindValues{"1.0/0.0", "1.0d0/0.0d0", "1.0x0/0.0x0"}};

PROCEDURE NaN(VAR (*OUT*) b: T; READONLY num: NumAttr; READONLY fmt: FmtRec)
  : CARDINAL =
  VAR str: TEXT; BEGIN
    IF fmt.literal
      THEN str := LitSpecials[0, num.kind]
      ELSE str := "NaN"
    END;
    RETURN Special(b, num, fmt, str)
  END NaN;

PROCEDURE Inf(VAR (*OUT*) b: T; READONLY num: NumAttr; READONLY fmt: FmtRec)
  : CARDINAL =
  VAR str: TEXT; truncate: BOOLEAN; BEGIN
    IF fmt.literal
      THEN str := LitSpecials[1, num.kind]; truncate := FALSE
      ELSE str := "Infinity"; truncate := TRUE
    END;
    RETURN Special(b, num, fmt, str, truncate := truncate)
  END Inf;

PROCEDURE Special(
    VAR (*INOUT*) b: T;
    READONLY num: NumAttr;
    READONLY fmt: FmtRec;
    str: TEXT;
    truncate := FALSE)
  : CARDINAL =
(* Format the string "str" into the buffer "b", and return the number of
   characters inserted. If "fmt.style" is not "Style.Sci", then the result is
   either "str" or "-str" as "num.sign" is 0 or 1, respectively. If
   "fmt.style" is "Style.Sci", the result is the concatenation of a space or
   minus sign and "str" followed by sufficient space characters to pad the
   result to overall width for scientific notation formatting according to
   "num.maxExpDigits", "fmt.prec", and "fmt.literal". If the leading space or
   minus sign followed by "str" does not fit in this width and "truncate" is
   "TRUE", then "str" is first truncated to 3 characters. *)

  PROCEDURE AppendBlanks(
      VAR (*INOUT*) b: T;
      VAR (*INOUT*) pos: CARDINAL;
      num: CARDINAL) =
  (* Append "num" blanks to "b" starting at "pos". Requires "num > 0". *)
    CONST
      BlanksLen = 20;
      Blanks = ARRAY [0..BlanksLen-1] OF CHAR{' ', ..};
    VAR toGo := num; copyCnt: CARDINAL; BEGIN
      <* ASSERT num > 0 *>
      REPEAT
        copyCnt := MIN(toGo, BlanksLen);
        SUBARRAY(b, pos, copyCnt) := SUBARRAY(Blanks, 0, copyCnt);
        DEC(toGo, copyCnt); INC(pos, copyCnt)
      UNTIL toGo = 0
    END AppendBlanks;

  (* Special *)
  VAR res: CARDINAL; BEGIN
    CASE fmt.style OF
      Style.Fix, Style.Auto =>
        VAR strLen := Text.Length(str); BEGIN
          res := num.sign;
          IF res = 1 THEN b[0] := '-' END;
          SUBARRAY(b, res, strLen) := SUBARRAY(str^, 0, strLen);
          INC(res, strLen)
        END
    | Style.Sci =>
        IF num.sign = 0
          THEN b[0] := ' '
          ELSE b[0] := '-'
        END;
        VAR
          name: TEXT; nmLen: CARDINAL;
          strLen := Text.Length(str);
          sciWidth := SciWidth(fmt, num.maxExpDigits);
        BEGIN
          IF strLen >= sciWidth AND truncate
            THEN name := Text.Sub(str, 0, 3); nmLen := 3
            ELSE name := str; nmLen := strLen
          END;
          SUBARRAY(b, 1, nmLen) := SUBARRAY(name^, 0, nmLen);
          res := 1 + nmLen;
          WITH num = sciWidth - res DO
            IF num > 0 THEN AppendBlanks(b, res, num) END
          END
        END
    END;
    RETURN res
  END Special;

CONST
  SignChar = ARRAY [0..1] OF CHAR{ ' ', '-' };
  ExpChar = ARRAY IEEEKind OF CHAR{ 'e', 'd', 'x' };
  MaxExpDigits = MAX(R.MaxExpDigits, MAX(LR.MaxExpDigits, ER.MaxExpDigits));

PROCEDURE AppendZeros(
    VAR (*INOUT*) b: T;
    VAR (*INOUT*) pos: CARDINAL;
    num: CARDINAL) =
(* Append "num" zero characters ('0') to "b" starting at "pos", and advance
   "pos" by "num". Requires "num > 0". *)
  CONST
    ZerosLen = 20;
    Zeros = ARRAY [0..ZerosLen-1] OF CHAR{'0', ..};
  VAR toGo := num; copyCnt: CARDINAL; BEGIN
    <* ASSERT num > 0 *>
    REPEAT
      copyCnt := MIN(toGo, ZerosLen);
      SUBARRAY(b, pos, copyCnt) := SUBARRAY(Zeros, 0, copyCnt);
      DEC(toGo, copyCnt); INC(pos, copyCnt)
    UNTIL toGo = 0
  END AppendZeros;

PROCEDURE Zero(VAR (*OUT*) b: T; READONLY num: NumAttr; READONLY fmt: FmtRec)
  : CARDINAL =
  VAR res: CARDINAL; fullPrec: CARDINAL; BEGIN
    IF fmt.prec = 0 AND fmt.literal
      THEN fullPrec := 1
      ELSE fullPrec := fmt.prec
    END;
    CASE fmt.style OF
      Style.Sci =>
        b[0] := SignChar[num.sign]; b[1] := '0'; res := 2;
        IF fullPrec > 0 THEN
          b[res] := '.'; INC(res);
          AppendZeros(b, res, fullPrec)
        END;
        IF fmt.literal AND num.kind # IEEEKind.Single THEN
          b[res] := ExpChar[num.kind]; b[res+1] := '0';
          INC(res, 2)
        END;
        CONST Spaces = ARRAY [0..MaxExpDigits+1] OF CHAR{' ', ..};
        VAR cnt := SciWidth(fmt, num.maxExpDigits) - res; BEGIN
          <* ASSERT cnt > 0 *>
          (* "cnt" will be the space for the exponent, which requires 2 +
             "T.MaxExpDigits". The size of the "Spaces" array guarantees that
             "cnt <= NUMBER(spaces)". *)
          SUBARRAY(b, res, cnt) := SUBARRAY(Spaces, 0, cnt);
          INC(res, cnt)
        END
    | Style.Fix =>
        IF num.sign = 1 THEN b[0] := '-' END;
        b[num.sign] := '0'; res := num.sign + 1;
        IF fullPrec > 0 THEN
          b[res] := '.'; INC(res);
          AppendZeros(b, res, fullPrec)
        END;
        IF fmt.literal AND num.kind # IEEEKind.Single THEN
          b[res] := ExpChar[num.kind]; b[res+1] := '0';
          INC(res, 2)
        END
    | Style.Auto =>
	CONST
	  LitZero = TwoKindValues{
	    KindValues{ "0.0",  "0.0d0",  "0.0x0"},
	    KindValues{"-0.0", "-0.0d0", "-0.0x0"}};
	  AutoZero = ARRAY [0..1] OF TEXT{"0", "-0"};
        VAR str: TEXT; BEGIN
          IF fmt.literal
            THEN str := LitZero[num.sign, num.kind]
            ELSE str := AutoZero[num.sign]
          END;
          res := Text.Length(str);
          SUBARRAY(b, 0, res) := SUBARRAY(str^, 0, res)
        END
    END;
    RETURN res
  END Zero;

PROCEDURE Number(
    VAR (*OUT*) b: T;
    READONLY num: NumAttr;
    VAR (*IN*) digits: Digits;
    READONLY fmt: FmtRec)
  : CARDINAL =
  CONST Zero = ORD('0');
  VAR pos: CARDINAL := 0; 

  PROCEDURE FmtNum(len, prec: CARDINAL; shift: INTEGER) =
  (* Write the first "len" digits of "digits" into "b" starting at position
     "pos", inserting a decimal point between digits "digits[shift-1]" and
     "digits[shift]". If "shift <= 0", a suitable number of leading digits are
     written. A total of "prec" digits are written after the decimal point.
     Hence, if "shift <= 0", only "MAX(0, MIN(len, prec + shift))" of the
     "len" digits are written. If "prec > len - shift", then a suitable number
     of trailing zeros are written.

     If "prec = 0", no decimal point is written, unless "fmt.literal" is
     "TRUE", in which case a decimal and a single "0" are written.

     This procedure increments "pos" by the number of characters written.

     There are 3 cases to consider: the decimal is to the left of the "len"
     digits (i.e., "shift <= 0"), the decimal is between some of these digits
     (i.e., "0 < shift < len"), or it is to the right of the digits (i.e.,
     "len <= shift"). *)

    (* FmtNum *)
    VAR toGo := prec; curr: CARDINAL; BEGIN
      (* Write digits before decimal point: *)
      IF shift <= 0 THEN
        curr := 0;
        b[pos] := '0'; INC(pos)
      ELSE
        curr := MIN(shift, len);
        (* write leading digits *)
        FOR i := 0 TO curr - 1 DO
          b[pos] := VAL(digits[i] + Zero, CHAR); INC(pos)
        END;
        (* write zeros down to units place, if necessary *)
      	IF len < shift THEN AppendZeros(b, pos, shift-len) END
      END;

      (* Handle "prec = 0" case *)
      IF prec = 0 THEN
        IF fmt.literal THEN b[pos] := '.'; b[pos+1] := '0'; INC(pos, 2) END;
        RETURN
      END;

      (* Write decimal point and "prec" digits after it: *)
      b[pos] := '.'; INC(pos);
      IF shift <= 0 THEN
        WITH zeroCnt = MIN(toGo, ABS(shift)) DO
          IF zeroCnt > 0 THEN
            AppendZeros(b, pos, zeroCnt);
            DEC(toGo, zeroCnt)
          END
        END
      END;
      IF shift <= 0 OR shift < len THEN
        WITH digCnt = MIN(toGo, len - curr) DO
          FOR i := curr TO curr + digCnt - 1 DO
            b[pos] := VAL(digits[i] + Zero, CHAR); INC(pos)
          END;
          DEC(toGo, digCnt)
        END
      END;
      IF toGo > 0 THEN AppendZeros(b, pos, toGo) END
    END FmtNum;

  PROCEDURE DelTrailingZeros() =
  (* Requires "pos > 0 AND (E i: 0 <= i < pos: b[i] = '.')". *)
    BEGIN
      REPEAT DEC(pos) UNTIL b[pos] # '0';
      IF b[pos] = '.' THEN
        IF fmt.literal
          THEN INC(pos); b[pos] := '0'
          ELSE DEC(pos)
        END
      END;
      INC(pos)
    END DelTrailingZeros;

  PROCEDURE FmtExp(exp: INTEGER; auto: BOOLEAN) =
  (* Write the exponent portion of a number in scientific notation to the
     array "b" starting at position "pos", and increment "pos" by the number
     of characters written. The written characters take the form:
     "e(+|-)XX**X", where the value of "XX**X" is "exp", left-padded with
     zeros to have width "num.maxExpChars". If "fmt.literal" is "TRUE", then
     the leading character is one of "e", "d", or "x" depending on "num.sign".
     If "auto" is "TRUE", then the plus sign and leading exponent zeros, if
     any, are dropped. *)
    CONST ExpSignChar = ARRAY [0..1] OF CHAR{ '+', '-' };
    VAR expLen: CARDINAL; expBuf: ARRAY [0..MaxExpDigits-1] OF CHAR; BEGIN
      IF fmt.literal
        THEN b[pos] := ExpChar[num.kind]
        ELSE b[pos] := 'e'
      END;
      INC(pos);
      expLen := Unsigned(expBuf, ABS(exp), base := 10);
      IF auto THEN
        IF exp < 0 THEN b[pos] := '-'; INC(pos) END
      ELSE
        b[pos] := ExpSignChar[ORD(exp < 0)]; INC(pos);
        CONST Zeros = ARRAY [0..MaxExpDigits-1] OF CHAR{'0', ..};
        VAR zLen: INTEGER := num.maxExpDigits - expLen; BEGIN
          (* The size of the "Zeros" array is chosen so as to
             guarantee that "zLen <= NUMBER(Zeros)". *)
          IF zLen > 0 THEN
            SUBARRAY(b, pos, zLen) := SUBARRAY(Zeros, 0, zLen);
            INC(pos, zLen)
          END
        END
      END;
      SUBARRAY(b, pos, expLen) := SUBARRAY(expBuf, 0, expLen);
      INC(pos, expLen)
    END FmtExp;

  (* Number *)
  VAR carry: BOOLEAN; exp := num.exp; len: INTEGER; BEGIN
    CASE fmt.style OF
      Style.Sci =>
        len := fmt.prec + 1;
        carry := Round(num, digits, len);
        IF carry THEN INC(exp) END;
        b[pos] := SignChar[num.sign]; INC(pos);
        FmtNum(len, prec := fmt.prec, shift := 1);
        FmtExp(exp, auto := FALSE)
    | Style.Fix =>
        len := fmt.prec + 1 + num.exp;
        carry := Round(num, digits, len);
        IF carry THEN INC(exp, 1 + ABS(MIN(0, len))) END;
        IF num.sign = 1 THEN b[pos] := '-'; INC(pos) END;
        FmtNum(len, prec := fmt.prec, shift := 1 + exp);
        IF fmt.literal AND num.kind # IEEEKind.Single THEN
          b[pos] := ExpChar[num.kind]; b[pos+1] := '0'; INC(pos, 2)
        END
    | Style.Auto =>
        len := fmt.prec + 1;
        carry := Round(num, digits, len);
        IF carry THEN INC(exp) END;
        IF num.sign = 1 THEN b[pos] := '-'; INC(pos) END;
        IF ABS(exp) >= 6 THEN
          FmtNum(len, prec := fmt.prec, shift := 1);
          IF fmt.prec > 0 THEN DelTrailingZeros() END;
          FmtExp(exp, auto := TRUE)
        ELSE
          WITH shift = 1 + exp, prec = MAX(fmt.prec, len - shift) DO
            FmtNum(len, prec := prec, shift := shift);
            IF prec > 0 THEN DelTrailingZeros() END
          END;
          IF fmt.literal AND num.kind # IEEEKind.Single THEN
            b[pos] := ExpChar[num.kind]; b[pos+1] := '0'; INC(pos, 2)
          END
        END
    END;
    RETURN pos
  END Number;

PROCEDURE Round(
    READONLY num: NumAttr;
    VAR (*IN*) digits: Digits;
    VAR (*INOUT*) len: INTEGER)
  : BOOLEAN =
(* Round the number "(num, digits)" to "len" digits of precision, modifying
   the digits "digits" in place. Requires "num.len >= 1". On exit from this
   procedure, "len" is set to the (non-negative) number of digits in "digits"
   that are valid, namely:

|    "MIN(MAX(len, ORD(RESULT)), num.len)"

   where "RESULT" denotes the Boolean return value. The final value for "len"
   can also be expressed by considering two cases:

|    Input len   Output len
|    ---------   -----------------
|    len <= 0    ORD(RESULT)           (* since num.len >= 1 *)
|    len => 1    MIN(len, num.len)     (* since ORD(BOOLEAN) <= 1 *)

   If "len >= num.len", then this procedure does not change "digits".
   Otherwise, the values "num.sign", "num.errorSign", and

|    digits[len], ..., digits[num.len-1]

   (where we define "digits[i] = 0" for "i < 0"), along with the current
   rounding mode, are used to decide if the digits 

|    digits[0], ..., digits[len-1]

   taken as an integer, should be incremented. The algorithm used to make this
   determination is described below. If it is decided that the number should
   be incremented and it is all "9"'s (or if it is empty, i.e., "len <= 0"),
   then incrementing it would cause an overflow. In this case, "digits[0]" is
   replaced by a "1". The procedure returns TRUE iff an overflow resulted due
   to incrementing the digits.

   See the "FmtBufTest" interface for a description of how the "current
   rounding mode" is determined from the global variables "useCurrentRounding"
   and "testRoundingMode". *)

  PROCEDURE NearestInc(tieBreak: BOOLEAN): BOOLEAN =
  (* Algorithm to decide whether or not to increment in the "NearestElse..."
     rounding mode cases. Returns TRUE iff an increment should be done. If
     "len < 0", the increment is never done. Otherwise, the value "R" is
     compared to "0.5" as described below. In the event that the decimal
     approximation is exactly half way between the unincremented and
     incremented values (i.e., when "R = 0.5" in the above analysis) and is
     exactly equal to the floating-binary value (i.e., when "num.errorSign =
     0"), the result is given by "tieBreak". *)
    BEGIN
      IF len < 0 THEN RETURN FALSE END;
      <* ASSERT 0 <= len AND len < num.len AND 1 <= num.len *>
      IF digits[len] < 5 THEN
        RETURN FALSE
      ELSIF digits[len] > 5 THEN
        RETURN TRUE
      ELSE
        VAR i := len + 1; BEGIN
          WHILE i < num.len AND digits[i] = 0 DO INC(i) END;
          IF i < num.len THEN RETURN TRUE END;
        END;
        CASE num.errorSign OF
          -1 => RETURN FALSE
        |  0 => RETURN tieBreak
        | +1 => RETURN TRUE
        END
      END
    END NearestInc;

  PROCEDURE Increment(): BOOLEAN =
  (* Increment the value "digits[0] ... digits[len-1]". If "len <= 0", this
     does nothing, but is considered to overflow. In the event of an overflow,
     set "digits[0]" to "1". Return TRUE iff there was an overflow. *) 
    BEGIN
      (* handle empty prefix case *)
      IF len <= 0 THEN digits[0] := 1; RETURN TRUE END;
      (* otherwise, increment from right to left *)
      VAR i := len; BEGIN
      	REPEAT
      	  DEC(i);
      	  digits[i] := (digits[i] + 1) MOD 10
      	UNTIL i = 0 OR digits[i] # 0;
      	IF i = 0 AND digits[i] = 0 THEN digits[0] := 1; RETURN TRUE END
      END;
      RETURN FALSE
    END Increment;

  CONST RoundingName = ARRAY FloatMode.RoundingMode OF TEXT{
    "NearestElseEven", "TowardMinusInfinity", "TowardPlusInfinity",
    "TowardZero", "NearestElseAwayFromZero", "IBM370", "Other"};

  (* Round *)
  VAR inc: BOOLEAN; roundingMode: FloatMode.RoundingMode; BEGIN
    <* ASSERT num.len >= 1 *>
    IF len >= num.len THEN len := num.len; RETURN FALSE END;
    IF useCurrentRounding
      THEN roundingMode := FloatMode.GetRounding()
      ELSE roundingMode := testRoundingMode
    END;
    CASE roundingMode OF
      FloatMode.RoundingMode.TowardZero => inc := FALSE
    | FloatMode.RoundingMode.TowardMinusInfinity => inc := num.sign = 1
    | FloatMode.RoundingMode.TowardPlusInfinity => inc := num.sign = 0
    | FloatMode.RoundingMode.NearestElseEven =>
        inc := NearestInc(tieBreak := (len >= 1 AND digits[len-1] MOD 2 = 1))
    | FloatMode.RoundingMode.NearestElseAwayFromZero =>
        inc := NearestInc(tieBreak := TRUE)
    | FloatMode.RoundingMode.IBM370, FloatMode.RoundingMode.Other =>
        Process.Crash("FmtBuf: Unimplemented rounding mode: \""
          & RoundingName[roundingMode] & "\".")
    END;
    VAR res: BOOLEAN; BEGIN
      IF inc
        THEN res := Increment() 
        ELSE res := FALSE
      END;
      len := MIN(MAX(len, ORD(res)), num.len);
      RETURN res
    END
  END Round;

(* Algorithm for deciding whether or not to increment in the Round procedure:

   Whether or not to increment the digits is determined as follows. Define
   "lo" and "hi" by:

|    lo = (-1)^num.sign * digits[0] . digits[1] ... digits[len-1] * 10^num.exp
|    hi = lo + (-1)^num.sign * 10^(num.exp - (len-1))

   Roughly speaking, at the end of the "Round" procedure, the value denoted by
   "num", "digits", "len", and the Boolean return result is either "lo" or
   "hi".

   On entry to the "Round" procedure, the value represented by the parameters
   "num", "digits", and "len" is a decimal approximation "da" defined by:

|    da = (-1)^num.sign * digits[0] . digits[1] ... digits[num.len-1]
|                       * 10^num.exp

   The value "da" approximates some floating point value "r" according to the
   procedure "Float.ToDecimal". Since "len < num.len", we have:

|    (1) ABS(lo) < ABS(da) < ABS(hi)

   Moreover, by definition of the approximation produced by "ToDecimal", we
   can prove that:

|    (2) ABS(lo) < ABS(r) < ABS(hi)

   Proof: Without loss of generality, suppose all values are non-negative and
   that "lo < hi <= r". By (1), we would then have "lo < da < hi <= r". But
   since "hi" has fewer digits than "da" and is nearer to "r" than "da" in the
   same rounding direction, "ToDecimal" would have produced "hi" instead of
   "da" in this case. Hence, "r < hi". For the same reason, we must also have
   "lo < r".

   The relations (1) and (2) guide the development of the algorithm. The
   algorithm can be divided into cases based on the current rounding mode.

   The following table describes when to increment for the rounding modes
   "TowardZero", "TowardMinusInfinity", and "TowardPlusInfinity":

|    Rounding Mode          Increment when...
|    -------------------    -----------------
|    TowardZero             NEVER
|    TowardMinusInfinity    num.sign = 1
|    TowardPlusInfinity     num.sign = 0

   The determination in the "NearestElseEven" and "NearestElseAwayFromZero"
   cases is made as follows. Let "R" be the nonnegative floating-decimal
   number defined by

|    R = 0 . digits[len] ... digits[num.len-1]
|      = sum(i: len <= i < num.len, digits[i] * 10^(len - i - 1))

   Then the decision whether or not to increment is based to the first order
   on the value of "R" as follows:

|                   R < 0.5    R = 0.5    R > 0.5
|    Increment?       NO        MAYBE       YES

   When "R = 0.5", we use "num.errorSign" to make our decision as follows:

|    num.errorSign =  -1          0          1               
|    Increment?       NO        MAYBE       YES

   When "R = 0.5" and "num.errorSign = 0", the decision to increment or not
   depends on the rounding mode, according to the following table:

|    Rounding Mode              Increment when...
|    -----------------------    -----------------
|    NearestElseEven            digits[len-1] MOD 2 = 1
|    NearestElseAwayFromZero    ALWAYS
*)

BEGIN
END FmtBuf.
