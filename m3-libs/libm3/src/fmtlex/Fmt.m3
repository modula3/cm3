(* Copyright (C) 1994, Digital Equipment Corporation                   *)
(* All rights reserved.                                                *)
(* See the file COPYRIGHT for a full description.                      *)
(*                                                                     *)
(* Last modified on Tue Mar  8 13:52:02 PST 1994 by heydon             *)
(*      modified on Fri Feb 25 17:02:28 PST 1994 by kalsow             *)
(*      modified on Fri Apr 30 10:19:41 PDT 1993 by mcjones            *)
(*      modified on Thu Apr 29 16:34:23 PDT 1993 by muller             *)

UNSAFE MODULE Fmt;

IMPORT Text, TextF, Word, Convert, FmtBuf, FmtBufF;
IMPORT Real AS R, LongReal AS LR, Extended AS ER;
IMPORT RealFloat, LongFloat, ExtendedFloat;

(* Boolean, character values ----------------------------------------------- *)

PROCEDURE Bool (b: BOOLEAN): Text.T =
  CONST Map = ARRAY BOOLEAN OF Text.T { "FALSE", "TRUE" };
  BEGIN 
    RETURN Map[b];
  END Bool;

PROCEDURE Char (c: CHAR): Text.T = 
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

PROCEDURE Int (n: INTEGER; base: Base := 10): Text.T =
  BEGIN
    IF FIRST(SmallInts) <= n AND n <= LAST(SmallInts) AND base = 10
      THEN RETURN SmallInts[n]
      ELSE RETURN AnyInt(n, base)
    END
  END Int;

PROCEDURE AnyInt (n: INTEGER; base: Base := 10): Text.T =
  <* FATAL Convert.Failed *>
  VAR chars: ARRAY [0..BITSIZE(INTEGER)] OF CHAR; used: INTEGER; BEGIN
    used := Convert.FromInt(chars, n, base, prefix := FALSE);
    RETURN Text.FromChars(SUBARRAY(chars, 0, used))
  END AnyInt;

PROCEDURE Unsigned (n: Word.T; base: Base := 10): Text.T =
  BEGIN
    IF 0 <= n AND n <= LAST(SmallInts) AND base = 10
      THEN RETURN SmallInts[n]
      ELSE RETURN AnyUnsigned (n, base)
    END
  END Unsigned;

PROCEDURE AnyUnsigned (n: Word.T; base: Base := 10): Text.T =
  <* FATAL Convert.Failed *>
  VAR chars: ARRAY [0..BITSIZE(INTEGER)-1] OF CHAR; used: INTEGER; BEGIN
    used := Convert.FromUnsigned (chars, n, base, prefix := FALSE);
    RETURN Text.FromChars(SUBARRAY(chars, 0, used))
  END AnyUnsigned;

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
    text: Text.T;
    length: CARDINAL;
    padChar: CHAR := ' ';
    align : Align := Align.Right)
  : Text.T =
  VAR buff: ARRAY [0..99] OF CHAR; len, padLen: INTEGER; pad: Text.T; BEGIN
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

PROCEDURE F(fmt: Text.T; t1, t2, t3, t4, t5: Text.T := NIL): Text.T =
(* Construct an array of texts not including NIL texts in the suffix, and call
   "FN" with the constructed array. *)
  VAR
    a := ARRAY [0..4] OF Text.T {t1, t2, t3, t4, t5};
    pos: INTEGER := LAST(a);
  BEGIN
    WHILE pos >= 0 AND a[pos] = NIL DO DEC(pos) END;
    RETURN FN(fmt, SUBARRAY(a, 0, pos + 1))
  END F;

CONST
  SpecBufferSize = 32;

TYPE
  (* Padding information *)
  FormatSpecPad = RECORD
    align: Align;
    width: CARDINAL;
    padChar: CHAR;
  END;

  FormatSpec = RECORD
    (* Textual position and size of specifier (including % and s) *)
    start, length: CARDINAL;
    (* Corresponding argument and its length *)
    arg: Text.T;
    argLength: CARDINAL;
    (* Padding information extracted from the specification *)
    pad: FormatSpecPad;
  END;

  SpecBuffer = ARRAY [0..SpecBufferSize-1] OF FormatSpec;

  SpecBufferList = REF RECORD
    next: SpecBufferList := NIL;
    buffer: SpecBuffer;
  END;

PROCEDURE ReadSpec(
    fmt: Text.T;
    start: CARDINAL;
    VAR (*OUT*) pad: FormatSpecPad)
    : CARDINAL =
(* Reads a format specifier from the string "Text.Sub(fmt, start)". This
   routine assumes that the leading '%' character has already been processed.
   It writes the "align", "padChar", and "width" fields of "pad", and returns
   the number of characters in the specifier (including the already processed
   '%' character). *)
  VAR
    ch : CHAR    := fmt[start];
    pos: INTEGER := start + 1;
  BEGIN
    (* Alignment *)
    IF ch = '-'
      THEN pad.align := Align.Left; ch := fmt[pos]; INC(pos)
      ELSE pad.align := Align.Right;
    END;

    (* Pad character *)
    IF ch = '0'
      THEN pad.padChar := '0'; ch := fmt[pos]; INC(pos)
      ELSE pad.padChar := ' ';
    END;

    (* Field width *)
    pad.width := 0;
    WHILE '0' <= ch AND ch <= '9' DO
      pad.width := pad.width * 10 + ORD(ch) - ORD('0');
      ch := fmt[pos]; INC(pos)
    END;

    (* terminating 's' *)
    IF ch = 's'
      THEN RETURN pos - start + 1 (* add 1 for '%' *)
      ELSE RETURN 0
    END;
  END ReadSpec;

PROCEDURE PutSpec(
    READONLY spec: FormatSpec;
    pos: CARDINAL;
    VAR (*INOUT*) list: SpecBufferList) =
(* Add the specifier "spec" with index "pos" to the list "list", where the
   first specifier in "list" has index "SpecBufferSize" on the initial,
   non-recursive call. Hence, this procedure requires that "pos >=
   SpecBufferSize" on the initial call. *)
  BEGIN
    DEC(pos, SpecBufferSize);
    IF pos >= SpecBufferSize THEN
      PutSpec(spec, pos, list.next)
    ELSE
      IF pos = 0 THEN list := NEW(SpecBufferList) END;
      list.buffer[pos] := spec;
    END
  END PutSpec;

PROCEDURE GetSpec(pos: CARDINAL; list: SpecBufferList): FormatSpec =
(* Return the specifier with index "i" from "list", where the first specifier
   in "list" has index "SpecBufferSize" on the initial, non-recursive call.
   Hence, this procedure requires that "pos >= SpecBufferSize" on the initial
   call. *) 
  BEGIN
    DEC(pos, SpecBufferSize);
    IF pos >= SpecBufferSize
      THEN RETURN GetSpec(pos, list.next)
      ELSE RETURN list.buffer[pos]
    END
  END GetSpec;

PROCEDURE FN(fmt: Text.T; READONLY texts: ARRAY OF Text.T): Text.T =
  <* FATAL Convert.Failed *>
  VAR
    fmtLen := Text.Length(fmt);
    resLen := fmtLen;			 (* length of final string *)
    buffer: SpecBuffer;
    overflow: SpecBufferList := NIL;

  PROCEDURE ReadSpecs(): CARDINAL =
  (* Scan through "fmt" looking for format specifiers. Information on each
     one found is stored in "buffer" or, if "buffer" overflows, "overflow".
     This implementation requires quadriatic time for specifications inserted
     in "overflow". Returns the number of specifiers found. *)
    VAR spec: FormatSpec; cnt := 0; fPos := 0; BEGIN
      WHILE fPos < fmtLen DO
    	IF fmt[fPos] = '%' THEN
    	  spec.start := fPos; INC(fPos);
    	  spec.length := ReadSpec(fmt, fPos, spec.pad);
    	  IF spec.length # 0 THEN
    	    INC(fPos, spec.length - 1);
    	    spec.arg := texts[cnt];
    	    spec.argLength := Text.Length(spec.arg);
    	    INC(resLen, MAX(spec.argLength, spec.pad.width) - spec.length);
    	    IF cnt < SpecBufferSize
    	      THEN buffer[cnt] := spec;
    	      ELSE PutSpec(spec, cnt, overflow);
    	    END;
    	    INC(cnt)
    	  END
    	ELSE
    	  INC(fPos)
    	END
      END;
      RETURN cnt
    END ReadSpecs;

  PROCEDURE ConstructResult(cnt: CARDINAL): TEXT =
  (* Allocate and return a string formed from "fmt", "buffer", and "overflow"
     by replacing format specifiers in "fmt" by the corresponding padded and
     aligned "cnt" argument values. *)
    VAR res: TEXT; fPos, rPos := 0; spec: FormatSpec; BEGIN
      res := TextF.New(resLen);
      FOR i := 0 TO cnt - 1 DO

        (* get next spec *)
        IF i < SpecBufferSize
          THEN spec := buffer[i];
          ELSE spec := GetSpec(i, overflow);
        END;

        (* copy section of 'fmt' between this and the last spec *)
        VAR fl := spec.start - fPos; BEGIN
          IF fl > 0 THEN
            SUBARRAY(res^, rPos, fl) := SUBARRAY(fmt^, fPos, fl);
            INC(rPos, fl)
          END
        END;
        fPos := spec.start + spec.length;

        (* copy padded argument *)          
        WITH al = spec.argLength, padChar = spec.pad.padChar DO
          VAR padding := spec.pad.width - al; BEGIN
            IF spec.pad.align = Align.Right AND padding > 0 THEN
              WITH limit = rPos + padding DO
          	REPEAT res[rPos] := padChar; INC(rPos) UNTIL rPos = limit
              END
            END;
            IF al > 0 THEN
              SUBARRAY(res^, rPos, al) := SUBARRAY(spec.arg^, 0, al);
              INC(rPos, al);
            END;
            IF spec.pad.align = Align.Left AND padding > 0 THEN
              WITH limit = rPos + padding DO
          	REPEAT res[rPos] := padChar; INC(rPos) UNTIL rPos = limit;
              END
            END
          END
        END

      END; (* FOR *)

      (* copy tail of format string *)
      WITH fl = fmtLen - fPos DO
        IF fl > 0 THEN
          SUBARRAY(res^, rPos, fl) := SUBARRAY(fmt^, fPos, fl)
        END
      END;
      RETURN res
    END ConstructResult;

  VAR specCnt: CARDINAL; BEGIN
    specCnt := ReadSpecs();		 (* read format specifiers *)
    IF specCnt # NUMBER(texts) THEN	 (* check for proper arg count *)
      RAISE Convert.Failed
    END;
    IF specCnt = 0 THEN RETURN fmt END;	 (* handle the null case *)
    RETURN ConstructResult(specCnt)	 (* replace specs by args *)
  END FN;

BEGIN
END Fmt.
