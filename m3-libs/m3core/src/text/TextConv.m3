(* Copyright (C) 1993 Digital Equipment Corporation.         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Wed Nov 24 09:40:32 PST 1993 by kalsow   *)
(*      modified on Fri Jul 16 19:43:17 1993 by luca         *)

MODULE TextConv;
IMPORT Text, Text8;

TYPE
  Chars    = ARRAY OF CHAR;
  RefChars = REF Chars;
  Char4    = ARRAY [0..3] OF CHAR;

CONST
  Octal = CharSet{'0'..'7'};

CONST
  EncodedSize = ARRAY CHAR OF [1..4] {
  (*******   0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  ***)
  (* 00 *)   4, 4, 4, 4, 4, 4, 4, 4, 4, 2, 2, 4, 2, 2, 4, 4,
  (* 10 *)   4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  (* 20 *)   1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1,
  (* 30 *)   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  (* 40 *)   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  (* 50 *)   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1,
  (* 60 *)   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  (* 70 *)   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  (* 80 *)   4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  (* 90 *)   4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  (* A0 *)   4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  (* B0 *)   4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  (* C0 *)   4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  (* D0 *)   4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  (* E0 *)   4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  (* F0 *)   4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
  };


PROCEDURE EncodedCharSize(charIn: CHAR): CARDINAL =
  BEGIN
    RETURN EncodedSize[charIn];
  END EncodedCharSize;

PROCEDURE EncodeChar(charIn: CHAR; VAR (*out*)charsOut: Char4): CARDINAL =
  VAR sz := EncodedSize[charIn];
  BEGIN
    IF sz = 1 THEN
      charsOut[0] := charIn;
    ELSIF sz = 2 THEN
      charsOut[0] := Escape;
      IF    charIn = Escape  THEN charsOut[1] := charIn;
      ELSIF charIn = '\n'    THEN charsOut[1] := 'n';
      ELSIF charIn = '\r'    THEN charsOut[1] := 'r';
      ELSIF charIn = '\t'    THEN charsOut[1] := 't';
      ELSIF charIn = '\f'    THEN charsOut[1] := 'f';
      ELSE  (*quotes*)            charsOut[1] := charIn;
      END;
    ELSE <*ASSERT sz = 4*>
      charsOut[0] := Escape;
      charsOut[1] := VAL((ORD(charIn) DIV 64)+ORD('0'), CHAR);
      charsOut[2] := VAL(((ORD(charIn) MOD 64) DIV 8)+ORD('0'), CHAR);
      charsOut[3] := VAL((ORD(charIn) MOD 8)+ORD('0'), CHAR);
    END;
    RETURN sz;
  END EncodeChar;

PROCEDURE EncodedCharsSize(READONLY charsIn: Chars): CARDINAL =
  VAR out: CARDINAL := 0;
  BEGIN
    FOR in := FIRST(charsIn) TO LAST(charsIn) DO
      INC(out, EncodedSize[charsIn[in]]);
    END;
    RETURN out;
  END EncodedCharsSize;

PROCEDURE EncodedTextSize(txt: TEXT): CARDINAL =
  VAR
    len : CARDINAL := Text.Length(txt);
    in  : CARDINAL := 0;
    out : CARDINAL := 0;
    buf : ARRAY [0..63] OF CHAR;
  BEGIN
    WHILE (in < len - NUMBER(buf)) DO
      Text.SetChars(buf, txt, in);
      INC(out, EncodedCharsSize(buf));
      INC(in, NUMBER(buf));
    END;
    IF (in < len) THEN
      Text.SetChars(buf, txt, in);
      INC(out, EncodedCharsSize(SUBARRAY(buf, 0, len-in)));
    END;
    RETURN out;
  END EncodedTextSize;

PROCEDURE EncodeChars(READONLY charsIn  : Chars; 
                    VAR(*out*) charsOut : Chars): CARDINAL =
  VAR out: CARDINAL := 0;   ch: CHAR;  buf: Char4;
  BEGIN
    FOR in := FIRST(charsIn) TO LAST(charsIn) DO
      ch := charsIn[in];
      IF EncodedSize[ch] = 1 THEN
        charsOut[out] := ch;  INC(out);
      ELSE
        FOR i := 0 TO EncodeChar(ch, (*out*)buf)-1 DO
          charsOut[out] := buf[i];  INC(out);
        END;
      END;
    END;
    RETURN out;
  END EncodeChars;

PROCEDURE Encode(textIn: TEXT; quoted: BOOLEAN:=TRUE): TEXT =
  VAR
    textLen : CARDINAL := Text.Length(textIn);
    len     : CARDINAL := EncodedTextSize(textIn);
    result  : Text8.T  := Text8.Create (len + 2 * ORD(quoted));
    charsOut: RefChars := result.contents;
    out     : CARDINAL := 0;
    in      : CARDINAL := 0;
    buf     : ARRAY [0..63] OF CHAR;
  BEGIN
    IF quoted THEN 
      charsOut[0] := '\"';
      charsOut[len+1] := '\"';
      out := 1;  INC(len);
    END;
    WHILE (in < textLen - NUMBER(buf)) DO
      Text.SetChars(buf, textIn, in);
      INC(out, EncodeChars(buf, SUBARRAY(charsOut^, out, len-out)));
      INC(in, NUMBER(buf));
    END;
    IF (in < textLen) THEN
      Text.SetChars(buf, textIn, in);
      INC(out, EncodeChars(SUBARRAY(buf, 0, textLen-in),
                           SUBARRAY(charsOut^, out, len-out)));
    END;
    RETURN result;
  END Encode;

PROCEDURE DecodeChar(READONLY charsIn: Char4;  availIn: INTEGER;
                   VAR(*out*) charOut: CHAR): CARDINAL
  RAISES {Fail} =
  VAR ord: INTEGER;
  BEGIN
    IF    availIn < 1             THEN  RAISE Fail;
    ELSIF charsIn[0] # Escape     THEN  charOut := charsIn[0]; RETURN 1;
    ELSIF availIn < 2             THEN  RAISE Fail
    ELSIF charsIn[1] = Escape     THEN  charOut := Escape; RETURN 2;
    ELSIF charsIn[1] = 'n'        THEN  charOut := '\n'; RETURN 2;
    ELSIF charsIn[1] = 'r'        THEN  charOut := '\r'; RETURN 2;
    ELSIF charsIn[1] = 't'        THEN  charOut := '\t'; RETURN 2;
    ELSIF charsIn[1] = 'f'        THEN  charOut := '\f'; RETURN 2;
    ELSIF charsIn[1] = '\''       THEN  charOut := '\''; RETURN 2;
    ELSIF charsIn[1] = '\"'       THEN  charOut := '\"'; RETURN 2;
    ELSIF availIn < 4             THEN  RAISE Fail;
    ELSIF NOT charsIn[1] IN Octal THEN  RAISE Fail;
    ELSIF NOT charsIn[2] IN Octal THEN  RAISE Fail;
    ELSIF NOT charsIn[3] IN Octal THEN  RAISE Fail;
    ELSE
      ord := (ORD(charsIn[1])-ORD('0'))*64 +
             (ORD(charsIn[2])-ORD('0'))*8 +
             (ORD(charsIn[3])-ORD('0'));
      IF ord > ORD (LAST(CHAR)) THEN RAISE Fail; END;
      charOut := VAL(ord, CHAR);
      RETURN 4;
    END;
  END DecodeChar;

PROCEDURE DecodedCharsSize(READONLY charsIn: Chars): CARDINAL
  RAISES {Fail} =
  VAR
    in    : CARDINAL := 0;
    out   : CARDINAL := 0;
    avail : INTEGER;
    buf   : Char4;
    charOut: CHAR;
  BEGIN
    LOOP
      avail := MIN(NUMBER(charsIn)-in, NUMBER(buf));
      IF avail=0 THEN RETURN out END;
      FOR i:=0 TO avail-1 DO buf[i]:=charsIn[in+i] END;
      INC(in, DecodeChar(buf, avail, (*out*)charOut));
      INC(out);
    END;
  END DecodedCharsSize;

PROCEDURE DecodeChars(READONLY charsIn  : Chars;
                    VAR(*out*) charsOut : Chars): CARDINAL
  RAISES {Fail} =
  VAR
    in    : CARDINAL := 0;
    out   : CARDINAL := 0;
    avail : INTEGER;
    buf   : Char4;
  BEGIN
    LOOP
      avail := MIN(NUMBER(charsIn)-in, NUMBER(buf));
      IF avail=0 THEN RETURN out END;
      FOR i:=0 TO avail-1 DO buf[i]:=charsIn[in+i] END;
      INC(in, DecodeChar(buf, avail, (*out*)charsOut[out]));
      INC(out);
    END;
  END DecodeChars;

PROCEDURE Decode(textIn: TEXT; quoted: BOOLEAN:=TRUE): TEXT
  RAISES {Fail} =
  VAR len := Text.Length(textIn);
  BEGIN
    IF len <= MaxShortDecode
      THEN RETURN DecodeShort(textIn, quoted, len);
      ELSE RETURN DecodeLong(textIn, quoted, len);
    END;
  END Decode;

CONST MaxShortDecode = 128;

PROCEDURE DecodeShort(textIn: TEXT; quoted: BOOLEAN; len: CARDINAL): TEXT
  RAISES {Fail} =
  VAR
   buf : ARRAY [0..MaxShortDecode-1] OF CHAR;
   tmp : ARRAY [0..MaxShortDecode-1] OF CHAR;
  BEGIN
    Text.SetChars(buf, textIn);
    RETURN DecodeBuf(SUBARRAY(buf, 0, len), tmp, quoted, len);
  END DecodeShort;

PROCEDURE DecodeLong(textIn: TEXT; quoted: BOOLEAN;  len: CARDINAL): TEXT
  RAISES {Fail} =
  VAR
    buf := NEW (RefChars, len);
    tmp := NEW (RefChars, len);
  BEGIN
    Text.SetChars(buf^, textIn);
    RETURN DecodeBuf(buf^, tmp^, quoted, len);
  END DecodeLong;

PROCEDURE DecodeBuf(READONLY buf: Chars;  VAR tmp: Chars; 
                    quoted: BOOLEAN;  len: CARDINAL): TEXT
  RAISES {Fail} =
  VAR start: CARDINAL := 0;
  BEGIN
    IF quoted THEN
      IF (len < 2) OR (buf[0] # '\"') OR (buf[len-1] # '\"') THEN
        RAISE Fail;
      END;
      start := 1;
      len := len-2;
    END;
    len := DecodeChars(SUBARRAY (buf, start, len), tmp);
    RETURN Text.FromChars(SUBARRAY (tmp, 0, len));
  END DecodeBuf;
     
PROCEDURE ImplodedSize(READONLY array: ARRAY OF TEXT): CARDINAL =
  VAR out: CARDINAL := 0;
  BEGIN
    FOR i := FIRST(array) TO LAST(array) DO
      INC(out, Text.Length(array[i]));
    END;
    INC(out, MAX(0,NUMBER(array)-1));
    RETURN out;
  END ImplodedSize;

PROCEDURE Implode(READONLY array: ARRAY OF TEXT; sep: CHAR): TEXT =
  VAR
    outLen := ImplodedSize(array);
    result := Text8.Create (outLen);
    buf    := result.contents;
    out    : CARDINAL := 0;
    len    : CARDINAL;
    text   : TEXT;
  BEGIN
    FOR i := FIRST(array) TO LAST(array) DO
      IF i # FIRST(array) THEN buf[out] := sep;  INC (out); END;
      text := array[i];
      len := Text.Length(text);
      Text.SetChars(SUBARRAY(buf^, out, len), text);
      INC(out, len);
    END;
    RETURN result;
  END Implode;

PROCEDURE ExplodedItemSize(text : TEXT;
             VAR(*in-out*) in   : INTEGER; 
                  READONLY sep  : SET OF CHAR): CARDINAL =
  VAR
    out : CARDINAL := 0;
    len : CARDINAL := Text.Length(text);
  BEGIN
    WHILE (in < len) AND NOT (Text.GetChar(text, in) IN sep) DO
      INC(in); INC(out);
    END;
    RETURN out;
  END ExplodedItemSize;

PROCEDURE ExplodeItem(text  : TEXT;
        VAR(*in-out*) in    : INTEGER;
           VAR(*out*) chars : Chars;
             READONLY sep   : SET OF CHAR): INTEGER =
  VAR
    out : CARDINAL := 0;
    len : CARDINAL := Text.Length(text);
    ch  : CHAR;
  BEGIN
    WHILE (in < len) DO
      ch := Text.GetChar(text, in);
      IF ch IN sep THEN RETURN out END;
      chars[out] := ch;
      INC(in); INC(out);
    END;
    RETURN out;
  END ExplodeItem;

PROCEDURE ExplodedSize(text: TEXT;  READONLY sep: SET OF CHAR): CARDINAL =
  VAR
    len : CARDINAL := Text.Length(text);
    out : CARDINAL := 0;
    in  : INTEGER  := 0;
  BEGIN
    LOOP
      EVAL ExplodedItemSize(text, (*in-out*)in, sep);
      IF in >= len THEN RETURN out+1; END;
      IF Text.GetChar(text, in) IN sep THEN INC(in); INC(out) END;
    END;
  END ExplodedSize;
      
PROCEDURE Explode(text  : TEXT;
       VAR(*out*) array : ARRAY OF TEXT; 
         READONLY sep   : SET OF CHAR) =
  VAR
    len : CARDINAL := Text.Length(text);
    out : CARDINAL := 0;
    in  : INTEGER  := 0;
    in1 : INTEGER;
    txt : Text8.T;
  BEGIN
    LOOP
      in1 := in;
      txt := Text8.Create (ExplodedItemSize(text, (*in-out*)in1, sep));
      EVAL ExplodeItem(text, (*in-out*)in, txt.contents^, sep);
      array[out] := txt;
      IF in >= len THEN RETURN END;
      IF Text.GetChar(text, in) IN sep THEN INC(in); INC(out) END;
    END;
  END Explode;

BEGIN
END TextConv.


(* In case these are wanted later.

TYPE CharConsumer = PROCEDURE(char: CHAR);

PROCEDURE EncodeCharToConsumer(
    p: CharConsumer;
    charIn: CHAR);
(* Like EncodeChar, but puts away the 1, 2, or 4 encoded characters by
   calls to a consumer. *)

PROCEDURE EncodeCharToConsumer(
    p: CharConsumer;
    charIn: CHAR) =
  VAR (*out*)charsOut: Char4; avail: INTEGER;
  BEGIN
    avail := EncodeChar(charIn, (*out*)charsOut);
    FOR i:=0 TO avail-1 DO p(charsOut[i]) END;
  END EncodeCharToConsumer;

-------

TYPE CharProducer = PROCEDURE():CHAR RAISES ANY;

PROCEDURE DecodeCharFromProducer(
    p: CharProducer;
    VAR (*out*)charOut: CHAR)
    RAISES {Fail};
(* Like DecodeChar, but gets the characters to decode by 1, 2, or 4
   calls to a producer. *)

PROCEDURE DecodeCharFromProducer(
    p: CharProducer;
    VAR (*out*)charOut: CHAR)
    RAISES {Fail} =
  VAR charsIn: Char4; availIn: INTEGER;
  BEGIN
    TRY
      charsIn[0] := p();
      availIn := 1;
      IF charsIn[0] = Escape THEN
        charsIn[1] := p();
        INC(availIn);
        IF charsIn[1] IN Octal THEN
          charsIn[2] := p();
          charsIn[3] := p();
          INC(availIn, 2);
        END;
      END;
    EXCEPT ELSE (* p failure *) RAISE Fail;
    END;
    EVAL DecodeChar(charsIn, availIn, (*out*)charOut);
  END DecodeCharFromProducer;
  
*)

