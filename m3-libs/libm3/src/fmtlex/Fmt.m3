(* Copyright (C) 1994, Digital Equipment Corporation                   *)
(* All rights reserved.                                                *)
(* See the file COPYRIGHT for a full description.                      *)
(*                                                                     *)
(* Last modified on Tue Mar  8 13:52:02 PST 1994 by heydon             *)
(*      modified on Fri Feb 25 17:02:28 PST 1994 by kalsow             *)
(*      modified on Fri Apr 30 10:19:41 PDT 1993 by mcjones            *)
(*      modified on Thu Apr 29 16:34:23 PDT 1993 by muller             *)

UNSAFE MODULE Fmt;

IMPORT Text, Text8, Text8Short, Word, Long, Convert, FmtBuf, FmtBufF;
IMPORT Real AS R, LongReal AS LR, Extended AS ER;
IMPORT RealFloat, LongFloat, ExtendedFloat;

(* Boolean, character values ----------------------------------------------- *)

PROCEDURE Bool (b: BOOLEAN): TEXT =
  CONST Map = ARRAY BOOLEAN OF TEXT { "FALSE", "TRUE" };
  BEGIN 
    RETURN Map[b];
  END Bool;

PROCEDURE Char (c: CHAR): TEXT = 
  BEGIN
    RETURN Text.FromChar(c);
  END Char;

(* Integer, unsigned values ------------------------------------------------ *)

CONST
  SmallInts = ARRAY [-50..100] OF TEXT {
    "-50","-49","-48","-47","-46","-45","-44","-43","-42","-41",
    "-40","-39","-38","-37","-36","-35","-34","-33","-32","-31",
    "-30","-29","-28","-27","-26","-25","-24","-23","-22","-21",
    "-20","-19","-18","-17","-16","-15","-14","-13","-12","-11",
    "-10", "-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1",
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",  "8",  "9",
     "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
     "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
     "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
     "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
     "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
     "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
     "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
     "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
     "90", "91", "92", "93", "94", "95", "96", "97", "98", "99",
     "100"
  };

PROCEDURE Int (n: INTEGER; base: Base := 10): TEXT =
  BEGIN
    IF FIRST(SmallInts) <= n AND n <= LAST(SmallInts) AND base = 10
      THEN RETURN SmallInts[n]
      ELSE RETURN AnyInt(n, base)
    END
  END Int;

PROCEDURE AnyInt (n: INTEGER; base: Base := 10): TEXT =
  <* FATAL Convert.Failed *>
  VAR chars: ARRAY [0..BITSIZE(INTEGER)] OF CHAR; used: INTEGER; BEGIN
    used := Convert.FromInt(chars, n, base, prefix := FALSE);
    RETURN Text.FromChars(SUBARRAY(chars, 0, used))
  END AnyInt;

PROCEDURE Unsigned (n: Word.T; base: Base := 10): TEXT =
  BEGIN
    IF 0 <= n AND n <= LAST(SmallInts) AND base = 10
      THEN RETURN SmallInts[n]
      ELSE RETURN AnyUnsigned (n, base)
    END
  END Unsigned;

PROCEDURE AnyUnsigned (n: Word.T; base: Base := 10): TEXT =
  <* FATAL Convert.Failed *>
  VAR chars: ARRAY [0..BITSIZE(INTEGER)-1] OF CHAR; used: INTEGER; BEGIN
    used := Convert.FromUnsigned (chars, n, base, prefix := FALSE);
    RETURN Text.FromChars(SUBARRAY(chars, 0, used))
  END AnyUnsigned;

PROCEDURE LongInt(n: LONGINT; base: Base := 10): TEXT =
  BEGIN
    IF VAL(FIRST(SmallInts), LONGINT) <= n AND n <= VAL(LAST(SmallInts), LONGINT) AND base = 10
      THEN RETURN SmallInts[VAL(n, INTEGER)]
      ELSE RETURN AnyLongInt(n, base)
    END
  END LongInt;

PROCEDURE AnyLongInt (n: LONGINT; base: Base := 10): TEXT =
  <* FATAL Convert.Failed *>
  VAR chars: ARRAY [0..BITSIZE(LONGINT)] OF CHAR; used: INTEGER; BEGIN
    used := Convert.FromLongInt(chars, n, base, prefix := FALSE);
    RETURN Text.FromChars(SUBARRAY(chars, 0, used))
  END AnyLongInt;

PROCEDURE LongUnsigned (n: Long.T; base: Base := 10): TEXT =
  BEGIN
    IF 0L <= n AND n <= VAL(LAST(SmallInts), LONGINT) AND base = 10
      THEN RETURN SmallInts[VAL(n, INTEGER)]
      ELSE RETURN AnyLongUnsigned (n, base)
    END
  END LongUnsigned;

PROCEDURE AnyLongUnsigned (n: Long.T; base: Base := 10): TEXT =
  <* FATAL Convert.Failed *>
  VAR chars: ARRAY [0..BITSIZE(LONGINT)-1] OF CHAR; used: INTEGER; BEGIN
    used := Convert.FromLongUnsigned (chars, n, base, prefix := FALSE);
    RETURN Text.FromChars(SUBARRAY(chars, 0, used))
  END AnyLongUnsigned;

(* Floating-point values --------------------------------------------------- *)

PROCEDURE Real(x: REAL; style := Style.Auto;
    prec: CARDINAL := R.MaxSignifDigits - 1;
    literal := FALSE): TEXT =
  CONST RealMin = MAX(6 + R.MaxExpDigits, 12);
  VAR
    da := RealFloat.ToDecimal(x);
    bufSz := RealMin + prec;
    num: FmtBufF.NumAttr;
  BEGIN
    num.class := FmtBufF.ClassMapReal[da.class];
    num.kind := FmtBufF.IEEEKind.Single;
    num.maxExpDigits := R.MaxExpDigits;
    num.sign := da.sign;
    IF num.class = FmtBufF.Class.Number THEN
      num.len := da.len;
      num.exp := da.exp;
      num.errorSign := da.errorSign;
      INC(bufSz, MAX(1, da.exp))
    END;
    RETURN Float(bufSz, num, da.digits, FmtBufF.FmtRec{style, prec, literal})
  END Real;

PROCEDURE LongReal(x: LONGREAL; style := Style.Auto;
    prec: CARDINAL := LR.MaxSignifDigits - 1;
    literal := FALSE): TEXT =
  CONST LongMin = MAX(6 + LR.MaxExpDigits, 12);
  VAR
    da := LongFloat.ToDecimal(x);
    bufSz := LongMin + prec;
    num: FmtBufF.NumAttr;
  BEGIN
    num.class := FmtBufF.ClassMapLong[da.class];
    num.kind := FmtBufF.IEEEKind.Double;
    num.maxExpDigits := LR.MaxExpDigits;
    num.sign := da.sign;
    IF num.class = FmtBufF.Class.Number THEN
      num.len := da.len;
      num.exp := da.exp;
      num.errorSign := da.errorSign;
      INC(bufSz, MAX(1, da.exp))
    END;
    RETURN Float(bufSz, num, da.digits, FmtBufF.FmtRec{style, prec, literal})
  END LongReal;

PROCEDURE Extended(x: EXTENDED; style := Style.Auto;
    prec: CARDINAL := ER.MaxSignifDigits - 1;
    literal := FALSE): TEXT =
  CONST ExtdMin = MAX(6 + ER.MaxExpDigits, 12);
  VAR
    da := ExtendedFloat.ToDecimal(x);
    bufSz := ExtdMin + prec;
    num: FmtBufF.NumAttr;
  BEGIN
    num.class := FmtBufF.ClassMapExtd[da.class];
    num.kind := FmtBufF.IEEEKind.Extended;
    num.maxExpDigits := ER.MaxExpDigits;
    num.sign := da.sign;
    IF num.class = FmtBufF.Class.Number THEN
      num.len := da.len;
      num.exp := da.exp;
      num.errorSign := da.errorSign;
      INC(bufSz, MAX(1, da.exp))
    END;
    RETURN Float(bufSz, num, da.digits, FmtBufF.FmtRec{style, prec, literal})
  END Extended;

CONST StackBufSz = 100;

(* The following procedure is implemented using the "Float" procedure in the
   "FmtBufF" interface. That interface requires the caller to pass a character
   buffer. To avoid an unnecessary allocation, these routines pass a
   stack-based buffer of size "StackBufSz" in the fast case. Otherwise, they
   allocate a sufficiently large buffer.

   The analysis in the "FmtBufF" interface concludes the the buffer
   requirements are bounded from above as follows:

|    Style.Sci:  width <= MAX(5 + MAX(prec, 1) + T.MaxExpDigits, 12)
|    Style.Fix:  width <= MAX(4 + MAX(prec, 1) + MAX(exp, 1), 12)

   Since "prec" is a cardinal, we have "MAX(prec, 1) <= 1 + prec". Hence, we
   will use the overall conservative bound of:

|    All cases:  width <= MAX(6 + prec + T.MaxExpDigits + MAX(exp, 1), 12)
|                      <= MAX(6 + T.MaxExpDigits, 12) + prec + MAX(exp, 1)

   The first element of this sum can be computed statically. *)

PROCEDURE Float(
    bufSz: CARDINAL;
    READONLY num: FmtBufF.NumAttr;
    VAR (*IN*) digits: FmtBufF.Digits;
    READONLY fmt: FmtBufF.FmtRec)
  : TEXT =
  VAR res: TEXT; BEGIN
    IF bufSz <= StackBufSz THEN
      VAR
        buf: ARRAY [0..StackBufSz-1] OF CHAR;
        cnt := FmtBufF.Float(buf, num, digits, fmt);
      BEGIN
        res := Text.FromChars(SUBARRAY(buf, 0, cnt))
      END
    ELSE
      VAR
        buf := NEW(UNTRACED REF FmtBuf.T, bufSz);
        cnt := FmtBufF.Float(buf^, num, digits, fmt);
      BEGIN
        res := Text.FromChars(SUBARRAY(buf^, 0, cnt));
        DISPOSE(buf)
      END
    END;
    RETURN res
  END Float;

(* Padding routines -------------------------------------------------------- *)

PROCEDURE Pad(
    text: TEXT;
    length: CARDINAL;
    padChar: CHAR := ' ';
    align : Align := Align.Right)
  : TEXT =
  VAR buff: ARRAY [0..99] OF CHAR; len, padLen: INTEGER; pad: TEXT; BEGIN
    len := length - Text.Length(text);
    IF len <= 0 THEN RETURN text END;
    padLen := MIN(NUMBER(buff), len);
    FOR i := 0 TO padLen - 1 DO buff[i] := padChar END;
    pad := Text.FromChars(SUBARRAY(buff, 0, padLen));
    WHILE len >= padLen DO
      IF align = Align.Right
        THEN text := pad & text
        ELSE text := text & pad
      END;
      DEC(len, padLen)
    END;
    IF len > 0 THEN
      IF align = Align.Right
        THEN text := Text.Sub(pad, 0, len) & text
        ELSE text := text & Text.Sub(pad, 0, len)
      END
    END;
    RETURN text
  END Pad;

PROCEDURE F(fmt: TEXT; t1, t2, t3, t4, t5: TEXT := NIL): TEXT =
(* Construct an array of texts not including NIL texts in the suffix, and call
   "FN" with the constructed array. *)
  VAR
    a := ARRAY [0..4] OF TEXT {t1, t2, t3, t4, t5};
    pos: INTEGER := LAST(a);
  BEGIN
    WHILE pos >= 0 AND a[pos] = NIL DO DEC(pos) END;
    RETURN FN(fmt, SUBARRAY(a, 0, pos + 1))
  END F;

PROCEDURE FN(fmt: TEXT; READONLY texts: ARRAY OF TEXT): TEXT =
  VAR len := Text.Length (fmt);
  BEGIN
    TYPECASE fmt OF
    | Text8.T(t) =>
        RETURN FNBuf (fmt, SUBARRAY (t.contents^, 0, len), texts);
    | Text8Short.T(t) =>
        RETURN FNBuf (fmt, SUBARRAY (t.contents,  0, len), texts);
    (******
    | Text8Literal.T(t) =>
        RETURN FNBuf (fmt, SUBARRAY (t.contents,  0, len), texts);
    ******)
    ELSE
        IF (len <= 128) THEN
          VAR chars: ARRAY [0..127] OF CHAR; BEGIN
            Text.SetChars (chars, fmt);
            RETURN FNBuf (fmt, SUBARRAY (chars, 0, len), texts);
          END;
        ELSE
          VAR chars := NEW (REF ARRAY OF CHAR, len); BEGIN
            Text.SetChars (chars^, fmt);
            RETURN FNBuf (fmt, chars^, texts);
          END;
        END;
    END;
  END FN;

TYPE
  FormatSpec = RECORD
    start    : CARDINAL;  (* offset of the specifier in the format string *)
    length   : CARDINAL;  (* length of the specifier in the format string *)
    padWidth : CARDINAL;
    padAlign : Align;
    padChar  : CHAR;
  END;

PROCEDURE FNBuf(fmtTxt : TEXT;
       READONLY fmt    : ARRAY OF CHAR;  (* == contents of fmtTxt *)
       READONLY texts  : ARRAY OF TEXT): TEXT =
  VAR n := NUMBER(texts);  specs: ARRAY [0..19] OF FormatSpec;
  BEGIN
    IF n <= NUMBER (specs)
      THEN RETURN DoFN (fmtTxt, fmt, texts, SUBARRAY (specs, 0, n));
      ELSE RETURN DoFN (fmtTxt, fmt, texts, NEW (REF ARRAY OF FormatSpec, n)^);
    END;
  END FNBuf;

PROCEDURE DoFN (fmtTxt : TEXT;
       READONLY fmt    : ARRAY OF CHAR;
       READONLY texts  : ARRAY OF TEXT;
       VAR      specs  : ARRAY OF FormatSpec): TEXT = 
  <* FATAL Convert.Failed *>
  VAR cnt := ReadSpecs(fmt, specs);
  BEGIN
    IF cnt # NUMBER(texts) THEN  RAISE Convert.Failed;  END;
    IF cnt = 0 THEN RETURN fmtTxt; END;	 (* handle the null case *)
    RETURN ConstructResult (fmt, texts, specs);
  END DoFN;

PROCEDURE ReadSpecs(READONLY fmt   : ARRAY OF CHAR;
                  VAR(*OUT*) specs : ARRAY OF FormatSpec): CARDINAL =
(* Scans "fmt" for format specifiers, sets "specs" to any that
   are found, and returns the number found. *)
  VAR cnt: CARDINAL := 0;   cursor := 0;  ignore: FormatSpec;
  BEGIN
    LOOP
      WHILE (cursor < NUMBER(fmt)) AND (fmt[cursor] # '%') DO INC(cursor); END;
      IF (cursor >= NUMBER(fmt)) THEN RETURN cnt; END;
      IF (cnt < NUMBER(specs)) THEN
        IF ReadSpec(fmt, cursor, specs[cnt]) THEN INC(cnt); END;
      ELSIF ReadSpec(fmt, cursor, ignore) THEN
        RETURN cnt+1;  (* too many *)
      END;
    END;
  END ReadSpecs;

PROCEDURE ReadSpec(READONLY fmt    : ARRAY OF CHAR;
              VAR(*IN/OUT*) cursor : INTEGER;
                 VAR(*OUT*) spec   : FormatSpec): BOOLEAN =
(* Reads a format specifier from "fmt" beginning at "cursor".  Updates
   "cursor" to reflect the characters consumed from "fmt" and
   sets "spec" to the corresponding specifier.  Returns "TRUE"
   if a complete specifier was parsed.  *)
  VAR ch: CHAR;  len := NUMBER(fmt);
  BEGIN
    spec.start    := cursor;
    spec.length   := 0;
    spec.padAlign := Align.Right;
    spec.padWidth := 0;
    spec.padChar  := ' ';

    ch := fmt[cursor];  
    <*ASSERT ch = '%'*>
    INC(cursor);
    IF (cursor >= len) THEN RETURN FALSE; END;
    ch := fmt[cursor]; 

    (* Alignment *)
    IF ch = '-' THEN
      spec.padAlign := Align.Left;
      INC(cursor);
      IF (cursor >= len) THEN RETURN FALSE; END;
      ch := fmt[cursor];
    END;

    (* Pad character *)
    IF ch = '0' THEN
      spec.padChar := '0';
      INC(cursor);
      IF (cursor >= len) THEN RETURN FALSE; END;
      ch := fmt[cursor]; 
    END;

    (* Field width *)
    WHILE '0' <= ch AND ch <= '9' DO
      spec.padWidth := spec.padWidth * 10 + ORD(ch) - ORD('0');
      INC(cursor);
      IF (cursor >= len) THEN RETURN FALSE; END;
      ch := fmt[cursor]; 
    END;

    (* terminating 's' *)
    IF ch # 's' THEN RETURN FALSE; END;
    INC(cursor);

    spec.length := cursor - spec.start;
    RETURN TRUE;
  END ReadSpec;

PROCEDURE ConstructResult(READONLY fmt    : ARRAY OF CHAR;
                          READONLY texts  : ARRAY OF TEXT;
                          VAR      specs  : ARRAY OF FormatSpec): TEXT = 

(* Allocate and return a string formed from "fmt", "texts" and "specs". *)
  VAR
    res: Text8.T;
    buf: REF ARRAY OF CHAR;
    fPos, rPos : INTEGER := 0;
    len, argLen, pad: INTEGER;
    arg: TEXT;
  BEGIN
    <*ASSERT NUMBER(texts) = NUMBER(specs)*>
    (* first, size and allocate the result *)
    len := NUMBER(fmt);
    FOR i := FIRST(specs) TO LAST(specs) DO
      WITH s = specs[i] DO
        argLen := Text.Length(texts[i]);
        INC(len, MAX(argLen, s.padWidth) - s.length);
      END;
    END;
    res := Text8.Create(len);
    buf := res.contents;

    FOR i := FIRST(specs) TO LAST(specs) DO
      WITH s = specs[i] DO
        (* copy section of 'fmt' between this and the last spec *)
        len := s.start - fPos;
        IF (len > 0) THEN
          SUBARRAY(buf^, rPos, len) := SUBARRAY(fmt, fPos, len);
          INC(rPos, len)
        END;
        fPos := s.start + s.length;  (* skip over the specifier *)

        (* copy padded argument *)
        arg := texts[i];
        len := Text.Length (arg);
        pad := s.padWidth - len;
        IF s.padAlign = Align.Right THEN
          WHILE pad > 0 DO buf[rPos] := s.padChar; INC(rPos); DEC(pad); END;
        END;
        IF len > 0 THEN
          Text.SetChars (SUBARRAY(buf^, rPos, len), arg); INC(rPos, len);
        END;
        IF s.padAlign = Align.Left THEN
          WHILE pad > 0 DO buf[rPos] := s.padChar; INC(rPos); DEC(pad); END;
        END;
      END; (*WITH*)
    END; (* FOR *)

    (* copy tail of format string *)
    len := NUMBER(fmt) - fPos;
    IF (len > 0) THEN
      SUBARRAY(buf^, rPos, len) := SUBARRAY(fmt, fPos, len);
      INC(rPos, len)
    END;

    RETURN res
  END ConstructResult;

BEGIN
END Fmt.
