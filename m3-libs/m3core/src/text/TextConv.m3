(* Copyright (C) 1993 Digital Equipment Corporation.         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Last modified on Wed Nov 24 09:40:32 PST 1993 by kalsow   *)
(*      modified on Fri Jul 16 19:43:17 1993 by luca         *)

MODULE TextConv;
IMPORT Text;

CONST
  Octal = CharSet{'0'..'7'};
  Octal01 = CharSet{'0', '1'};

PROCEDURE EncodedCharSize(charIn: CHAR): INTEGER =
  BEGIN
    IF charIn = Escape THEN RETURN 2;
    ELSIF charIn = VAL(10, CHAR) THEN RETURN 2;
    ELSIF charIn = VAL(13, CHAR) THEN RETURN 2;
    ELSIF charIn = VAL(9, CHAR) THEN RETURN 2;
    ELSIF charIn = VAL(12, CHAR) THEN RETURN 2;
    ELSIF charIn IN Quotes THEN RETURN 2;
    ELSIF charIn IN NonPrinting THEN RETURN 4;
    ELSE RETURN 1;
    END;
  END EncodedCharSize;

PROCEDURE EncodeChar(
    charIn: CHAR; 
    VAR (*out*)charsOut: ARRAY[0..3] OF CHAR)
    : INTEGER =
  BEGIN
    charsOut[0] := Escape;
    IF charIn = Escape THEN charsOut[1] := charIn; RETURN 2;
    ELSIF charIn = VAL(10, CHAR) THEN charsOut[1] := 'n'; RETURN 2;
    ELSIF charIn = VAL(13, CHAR) THEN charsOut[1] := 'r'; RETURN 2;
    ELSIF charIn = VAL(9, CHAR) THEN charsOut[1] := 't'; RETURN 2;
    ELSIF charIn = VAL(12, CHAR) THEN charsOut[1] := 'f'; RETURN 2;
    ELSIF charIn IN Quotes THEN charsOut[1] := charIn; RETURN 2;
    ELSIF charIn IN NonPrinting THEN
      charsOut[1] := VAL((ORD(charIn) DIV 64)+ORD('0'), CHAR);
      charsOut[2] := VAL(((ORD(charIn) MOD 64) DIV 8)+ORD('0'), CHAR);
      charsOut[3] := VAL((ORD(charIn) MOD 8)+ORD('0'), CHAR);
      RETURN 4;
    ELSE charsOut[0] := charIn; RETURN 1;
    END;
  END EncodeChar;

PROCEDURE EncodedCharsSize(
    READONLY charsIn: ARRAY OF CHAR): INTEGER =
  VAR in, out: INTEGER;
  BEGIN
    in := 0;
    out := 0;
    LOOP
      IF in = NUMBER(charsIn) THEN RETURN out END;
      INC(out, EncodedCharSize(charsIn[in]));
      INC(in);
    END;
  END EncodedCharsSize;

PROCEDURE EncodeChars(
    READONLY charsIn: ARRAY OF CHAR; 
    VAR (*out*)charsOut: ARRAY OF CHAR)
    : INTEGER =
  VAR in, out, avail: INTEGER; buf: ARRAY [0..3] OF CHAR;
  BEGIN
    in := 0;
    out := 0;
    LOOP
      IF in = NUMBER(charsIn) THEN RETURN out END;
      avail := EncodeChar(charsIn[in], (*out*)buf);
      FOR i := 0 TO avail-1 DO
        charsOut[out] := buf[i];
        INC(out);
      END;
      INC(in);
    END;
  END EncodeChars;

PROCEDURE Encode(textIn: TEXT; quoted: BOOLEAN:=TRUE): TEXT =
  TYPE Chars = REF ARRAY OF CHAR;
  VAR charsIn, charsOut: Chars; len: INTEGER;
  BEGIN
    charsIn := NEW(Chars, Text.Length(textIn));
    Text.SetChars(charsIn^, textIn);
    len := EncodedCharsSize(charsIn^);
    IF quoted THEN 
      charsOut := NEW(Chars, len+2);
      charsOut^[0] := '\"';
      EVAL EncodeChars(charsIn^, SUBARRAY(charsOut^, 1, len));
      charsOut^[len+1] := '\"';
    ELSE
      charsOut := NEW(Chars, len);
      EVAL EncodeChars(charsIn^, charsOut^);
    END;
    RETURN Text.FromChars(charsOut^);
  END Encode;

PROCEDURE DecodeChar(
    READONLY charsIn: ARRAY[0..3] OF CHAR; availIn: INTEGER;
    VAR (*out*)charOut: CHAR)
    : INTEGER RAISES {Fail} =
  VAR ord: INTEGER;
  BEGIN
    IF availIn < 1 THEN RAISE Fail END;
    IF charsIn[0] = Escape THEN
      IF availIn < 2 THEN RAISE Fail END;
      IF charsIn[1] = Escape THEN charOut := Escape; RETURN 2;
      ELSIF charsIn[1] = 'n' THEN charOut := VAL(10, CHAR); RETURN 2;
      ELSIF charsIn[1] = 'r' THEN charOut := VAL(13, CHAR); RETURN 2;
      ELSIF charsIn[1] = 't' THEN charOut := VAL(9, CHAR); RETURN 2;
      ELSIF charsIn[1] = 'f' THEN charOut := VAL(12, CHAR); RETURN 2;
      ELSIF charsIn[1] IN Quotes THEN charOut := charsIn[1]; RETURN 2;
      ELSIF charsIn[1] IN Octal01 THEN
        IF availIn < 4 THEN RAISE Fail END;
        IF NOT (charsIn[2] IN Octal) OR NOT (charsIn[3] IN Octal) THEN
          RAISE Fail;
        END;
        ord := (ORD(charsIn[1])-ORD('0'))*64 +
                (ORD(charsIn[2])-ORD('0'))*8 +
                (ORD(charsIn[3])-ORD('0'));
        charOut := VAL(ord, CHAR);
        RETURN 4;
      ELSE charOut := charsIn[1]; RETURN 2;
      END;
    ELSE charOut := charsIn[0]; RETURN 1;
    END;
  END DecodeChar;

PROCEDURE DecodedCharsSize(
    READONLY charsIn: ARRAY OF CHAR)
    : INTEGER RAISES {Fail} =
  VAR in, out, avail: INTEGER;  buf: ARRAY [0..3] OF CHAR; charOut: CHAR;
  BEGIN
    in := 0;
    out := 0;
    LOOP
      avail := MIN(NUMBER(charsIn)-in, NUMBER(buf));
      IF avail=0 THEN RETURN out END;
      FOR i:=0 TO avail-1 DO buf[i]:=charsIn[in+i] END;
      INC(in, DecodeChar(buf, avail, (*out*)charOut));
      INC(out);
    END;
  END DecodedCharsSize;

PROCEDURE DecodeChars(
    READONLY charsIn: ARRAY OF CHAR; 
    VAR (*out*)charsOut: ARRAY OF CHAR)
    : INTEGER RAISES {Fail} =
  VAR in, out, avail: INTEGER;  buf: ARRAY [0..3] OF CHAR;
  BEGIN
    in := 0;
    out := 0;
    LOOP
      avail := MIN(NUMBER(charsIn)-in, NUMBER(buf));
      IF avail=0 THEN RETURN out END;
      FOR i:=0 TO avail-1 DO buf[i]:=charsIn[in+i] END;
      INC(in, DecodeChar(buf, avail, (*out*)charsOut[out]));
      INC(out);
    END;
  END DecodeChars;

PROCEDURE Decode(textIn: TEXT; quoted: BOOLEAN:=TRUE): TEXT RAISES {Fail} =
  TYPE Chars = REF ARRAY OF CHAR;
  VAR charsIn, charsOut: Chars; len: INTEGER;
  BEGIN
    len := Text.Length(textIn);
    charsIn := NEW(Chars, len);
    Text.SetChars(charsIn^, textIn);
    IF quoted THEN
      IF (len < 2) OR (charsIn^[0] # '\"') OR (charsIn^[len-1] # '\"')
      THEN RAISE Fail;
      END;
      charsOut := NEW(Chars, DecodedCharsSize(SUBARRAY(charsIn^, 1, len-2)));
      EVAL DecodeChars(SUBARRAY(charsIn^, 1, len-2), (*out*)charsOut^);
    ELSE
      charsOut := NEW(Chars, DecodedCharsSize(charsIn^));
      EVAL DecodeChars(charsIn^, (*out*)charsOut^);
    END;
    RETURN Text.FromChars(charsOut^);
  END Decode;
     
PROCEDURE ImplodedSize(READONLY array: ARRAY OF TEXT): INTEGER =
  VAR out: INTEGER;
  BEGIN
    out := 0;
    FOR i:=0 TO NUMBER(array)-1 DO
      INC(out, Text.Length(array[i]));
    END;
    INC(out, MAX(0,NUMBER(array)-1));
    RETURN out;
  END ImplodedSize;

PROCEDURE Implode(READONLY array: ARRAY OF TEXT; sep: CHAR): TEXT =
  TYPE Chars = REF ARRAY OF CHAR;
  VAR charsOut: Chars; out, len: INTEGER; text: TEXT;
  BEGIN
    charsOut := NEW(Chars, ImplodedSize(array));
    out := 0;
    FOR i:=0 TO NUMBER(array)-1 DO
      text := array[i];
      len := Text.Length(text);
      Text.SetChars(SUBARRAY(charsOut^,out,len), text);
      INC(out, len);
      IF i#NUMBER(array)-1 THEN 
        charsOut[out] := sep;
        INC(out);
      END;
    END;
    RETURN Text.FromChars(charsOut^);
  END Implode;

PROCEDURE ExplodedItemSize(text : TEXT;
             VAR(*in-out*) in   : INTEGER; 
                  READONLY sep  : SET OF CHAR): INTEGER =
  VAR out, len: INTEGER; ch: CHAR;
  BEGIN
    out := 0;
    len := Text.Length(text);
    LOOP
      IF in >= len THEN RETURN out END;
      ch := Text.GetChar(text, in);
      IF ch IN sep THEN RETURN out END;
      INC(in);
      INC(out);
    END;
  END ExplodedItemSize;

PROCEDURE ExplodeItem(text  : TEXT;
        VAR(*in-out*) in    : INTEGER; 
           VAR(*out*) chars : ARRAY OF CHAR;
             READONLY sep   : SET OF CHAR): INTEGER =
  VAR out, len: INTEGER; ch: CHAR;
  BEGIN
    out := 0;
    len := Text.Length(text);
    LOOP
      IF in >= len THEN RETURN out END;
      ch := Text.GetChar(text, in);
      IF ch IN sep THEN RETURN out END;
      INC(in);
      chars[out] := ch;
      INC(out);
    END;
  END ExplodeItem;

PROCEDURE ExplodedSize(text: TEXT;
              READONLY sep: SET OF CHAR): INTEGER =
  VAR in, out, len: INTEGER;
  BEGIN
    in := 0;
    out := 0;
    len := Text.Length(text);
    LOOP
      EVAL ExplodedItemSize(text, (*in-out*)in, sep);
      IF in >= len THEN RETURN out+1; END;
      IF Text.GetChar(text, in) IN sep THEN INC(in); INC(out) END;
    END;
  END ExplodedSize;
      
PROCEDURE Explode(text  : TEXT;
       VAR(*out*) array : ARRAY OF TEXT; 
         READONLY sep   : SET OF CHAR) =
  TYPE Chars = REF ARRAY OF CHAR;
  VAR charsOut: Chars; in, in1, out, len: INTEGER;
  BEGIN
    in := 0;
    out := 0;
    len := Text.Length(text);
    LOOP
      in1 := in;
      charsOut := NEW(Chars, ExplodedItemSize(text, (*in-out*)in1, sep));
      EVAL ExplodeItem(text, (*in-out*)in, charsOut^, sep);
      array[out] := Text.FromChars(charsOut^);
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
  VAR (*out*)charsOut: ARRAY[0..3] OF CHAR; avail: INTEGER;
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
  VAR charsIn: ARRAY[0..3] OF CHAR; availIn: INTEGER;
  BEGIN
    TRY
      charsIn[0] := p();
      availIn := 1;
      IF charsIn[0] = Escape THEN
        charsIn[1] := p();
        INC(availIn);
        IF charsIn[1] IN Octal01 THEN
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

