(* Copyright (C) 1989, 1992, Digital Equipment Corporation                   *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Feb 18 13:14:22 PST 1994 by kalsow                   *)
(*      modified on Wed Mar  4 12:00:42 PST 1992 by muller                   *)

UNSAFE MODULE OldFmt;

IMPORT Text, TextF, Word, Convert;

<*FATAL Convert.Failed*>

PROCEDURE Bool (b: BOOLEAN): Text.T =
  CONST Map = ARRAY BOOLEAN OF Text.T { "FALSE", "TRUE" };
  BEGIN 
    RETURN Map [b];
  END Bool;

PROCEDURE Int (n: INTEGER;  base : Base := 10): Text.T =
  VAR chars : ARRAY [0 .. BITSIZE (INTEGER) + 4] OF CHAR;   used: INTEGER;
  BEGIN
    used := Convert.FromInt (chars, n, base, FALSE);
    RETURN Text.FromChars (SUBARRAY (chars, 0, used));
  END Int;

PROCEDURE Unsigned (n: Word.T; base: Base := 16): Text.T =
  VAR chars : ARRAY [0 .. BITSIZE (INTEGER) + 4] OF CHAR;  used: INTEGER;
  BEGIN
    used := Convert.FromUnsigned (chars, n, base, FALSE);
    RETURN Text.FromChars (SUBARRAY (chars, 0, used));
  END Unsigned;

PROCEDURE Addr (n: ADDRESS;  base : Base := 16): Text.T =
  BEGIN
    IF n = NIL THEN
      RETURN ("NIL");
    ELSE
      RETURN (Unsigned (LOOPHOLE (n, Word.T), base)); END;
  END Addr;

PROCEDURE Ref (r: REFANY;  base : Base := 16): Text.T =
  BEGIN
    IF r = NIL THEN
      RETURN ("NIL");
    ELSE
      RETURN (Unsigned (LOOPHOLE (r, Word.T), base)); END;
  END Ref;

PROCEDURE Real (x: REAL; p: CARDINAL:= 6; s : Style := Style.Mix): Text.T =
  VAR chars: ARRAY [0..100] OF CHAR;  used: INTEGER;
  BEGIN
    used := Convert.FromFloat (chars, x, p, s);
    RETURN Text.FromChars (SUBARRAY (chars, 0, used));
  END Real;

PROCEDURE LongReal (x : LONGREAL; p : CARDINAL:= 6; s := Style.Mix): Text.T =
  VAR chars: ARRAY [0..100] OF CHAR;  used: INTEGER;
  BEGIN
    used := Convert.FromLongFloat (chars, x, p, s);
    RETURN Text.FromChars (SUBARRAY (chars, 0, used));
  END LongReal;

PROCEDURE Char (c: CHAR): Text.T = 
  BEGIN
    RETURN (Text.FromChar (c));
  END Char;

PROCEDURE Pad (text: Text.T;  length: CARDINAL;
               padChar: CHAR := ' ';  align : Align := Align.Right): Text.T =
  VAR buff: ARRAY [0..99] OF CHAR;  len: INTEGER;  pad: Text.T;
  BEGIN
    len := length - Text.Length (text);
    IF (len <= 0) THEN  RETURN text  END;
    FOR i := 0 TO MIN (LAST (buff), len - 1) DO buff [i] := padChar; END;
    pad := Text.FromChars (SUBARRAY (buff, 0, MIN (NUMBER (buff), len)));
    WHILE (len >= NUMBER (buff)) DO
      IF (align = Align.Right)
        THEN text := pad & text;
        ELSE text := text & pad;
      END;
      DEC (len, NUMBER (buff));
    END;
    IF (len > 0) THEN
      IF (align = Align.Right)
        THEN text := Text.Sub (pad, 0, len) & text;
        ELSE text := text & Text.Sub (pad, 0, len);
      END;
    END;
    RETURN text;
  END Pad;


PROCEDURE F(fmt: Text.T; t1, t2, t3, t4, t5: Text.T := NIL): Text.T RAISES {} =
  VAR
    a := ARRAY [0..4] OF Text.T {t1, t2, t3, t4, t5};
    pos: INTEGER := LAST(a);
  BEGIN
    LOOP
      IF pos < 0 OR a[pos] # NIL THEN
        RETURN FN(fmt, SUBARRAY(a, 0, pos + 1));
      ELSE
        DEC(pos);
      END;
    END;
  END F;


CONST
  SpecBufferLast = 31;


TYPE
  (* Padding information *)
  FormatSpecPad = RECORD
    field    : CARDINAL;
    fillChar : CHAR;
    align    : Align;
  END;

  FormatSpec = RECORD
    (* Specification textual position and size *)
    start, length: CARDINAL;
    (* Corresponding argument *)
    arg: Text.T;
    argLength: CARDINAL;
    (* Padding information extracted from the specification *)
    pad: FormatSpecPad;
  END;

  SpecBuffer = ARRAY [0..SpecBufferLast] OF FormatSpec;
  RefSpecBuffer = REF RECORD
    next: RefSpecBuffer := NIL;
    buffer: SpecBuffer;
  END;


PROCEDURE FormatSpecifier(
    fmt: Text.T;
    start: CARDINAL;
    VAR pad: FormatSpecPad)
    : CARDINAL
    RAISES {}=
  VAR
    ch : CHAR    := fmt[start];
    pos: INTEGER := start + 1;
  BEGIN
    (* Alignment *)
    IF ch = '-' THEN
      pad.align := Align.Left;
      ch := fmt[pos]; INC(pos);
    ELSE
      pad.align := Align.Right;
    END;
    (* Pad character *)
    IF ch = '0' THEN
      pad.fillChar := '0';
      ch := fmt[pos]; INC(pos);
    ELSE
      pad.fillChar := ' ';
    END;
    (* Field width *)
    pad.field := 0;
    WHILE '0' <= ch AND ch <= '9' DO
      pad.field := pad.field * 10 + ORD(ch) - ORD('0');
      ch := fmt[pos];
      INC(pos);
    END;
    (* terminating 's' *)
    IF ch = 's' THEN
      RETURN pos - start + 1; (* Add 1 for the initial '%' *)
    ELSE
      RETURN 0;
    END;
  END FormatSpecifier;


PROCEDURE PutSpec(
    READONLY spec: FormatSpec;
    pos: CARDINAL;
    VAR buffer: RefSpecBuffer)
    RAISES {}=
  BEGIN
    DEC(pos, SpecBufferLast + 1);
    IF pos > SpecBufferLast THEN
      PutSpec(spec, pos, buffer.next);
    ELSE
      IF pos = 0 THEN buffer := NEW(RefSpecBuffer) END;
      buffer.buffer[pos] := spec;
    END;
  END PutSpec;


PROCEDURE GetSpec(pos: CARDINAL; buffer: RefSpecBuffer): FormatSpec RAISES {}=
  BEGIN
    DEC(pos, SpecBufferLast + 1);
    IF pos > SpecBufferLast THEN
      RETURN GetSpec(pos, buffer.next);
    ELSE
      RETURN buffer.buffer[pos];
    END;
  END GetSpec;


PROCEDURE FN(fmt: Text.T; READONLY texts: ARRAY OF Text.T): Text.T RAISES {} =
  VAR
    fPos, specs := 0;
    fmtLength := Text.Length(fmt);
    length := fmtLength;
    spec: FormatSpec;
    buffer: SpecBuffer;
    emergencyBuffer: RefSpecBuffer := NIL;
  BEGIN
    (* first scan through 'fmt' looking for format specifiers. Information
     on each one found is stored in 'buffer' or, if 'buffer' overflows,
     'emergencyBuffer' *)
    WHILE fPos < fmtLength DO
      IF fmt[fPos] = '%' THEN
        spec.start := fPos;
        spec.length := FormatSpecifier(fmt, fPos+1, spec.pad);
        IF spec.length # 0 THEN
          spec.arg := texts[specs];
          spec.argLength := Text.Length(spec.arg);
          INC(length, MAX(spec.argLength, spec.pad.field) - spec.length);
          IF specs <= SpecBufferLast THEN
            buffer[specs] := spec;
          ELSE
            PutSpec(spec, specs, emergencyBuffer);
          END;
          INC(specs);
          INC(fPos, spec.length);
        ELSE
          INC(fPos);
        END;
      ELSE
        INC(fPos);
      END;
    END;

    (* does format string match arguments? *)
    IF specs # NUMBER(texts) THEN RAISE Convert.Failed; END;

    (* handle the null case *)
    IF specs = 0 THEN RETURN fmt END;

    (* Now we allocate a result and build it by copying in sections of the
     format string and the arguments *)
    VAR
      result := NEW(Text.T, length+1);
      rPos := 0;
    BEGIN
      fPos := 0;
      FOR i := 0 TO specs - 1 DO

        (* get next spec *)
        IF i <= SpecBufferLast THEN
          spec := buffer[i];
        ELSE
          spec := GetSpec(i, emergencyBuffer);
        END;

        (* copy section of 'fmt' between this and the last spec *)
        WITH fl = spec.start - fPos DO
          IF fl > 0 THEN
            SUBARRAY(result^, rPos, fl) := SUBARRAY(fmt^, fPos, fl);
            INC(rPos, fl);
          END;
        END;
        fPos := spec.start + spec.length;

        (* copy padded argument *)          
        VAR
          al := spec.argLength;
          padding := spec.pad.field - al;
          padChar := spec.pad.fillChar;
        BEGIN
          IF padding > 0 AND spec.pad.align = Align.Right THEN
            WITH limit = rPos + padding DO
              REPEAT result[rPos] := padChar; INC(rPos) UNTIL rPos = limit;
            END;
            padding := 0;
          END;
          IF al > 0 THEN
            SUBARRAY(result^, rPos, al) := SUBARRAY(spec.arg^, 0, al);
            INC(rPos, al);
          END;
          IF padding > 0 AND spec.pad.align = Align.Left THEN
            WITH limit = rPos + padding DO
              REPEAT result[rPos] := padChar; INC(rPos) UNTIL rPos = limit;
            END;
          END;
        END;

      END;

      (* copy tail of format string *)
      WITH fl = fmtLength - fPos DO
        IF fl > 0 THEN
          SUBARRAY(result^, rPos, fl) := SUBARRAY(fmt^, fPos, fl);
        END;
      END;

      RETURN result;

    END;
  END FN;

BEGIN
END OldFmt.

