(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


MODULE M3CLex EXPORTS M3CLex, M3CLexF;

IMPORT Text, Fmt, Thread;
IMPORT Rd, ASCII, RdExtras;
IMPORT M3CToken, M3CHash, M3CReservedWord, M3CSrcPos, M3CLexF;


CONST
  MaxLookahead = 3;
(* Need three characters lookahead for lexing, for example
   3.2E+A
which is not going to parse but lexes as a real literal followed by an
identifier followed by plus followed by another identifier.
  Actual lookahead buffer allows one more character for use when disabling the
lexer *)

REVEAL
  Buffer = BRANDED REF RECORD
    chars: ARRAY [0..2047] OF CHAR;
    overflow: Buffer := NIL;
  END;

REVEAL
  T = M3CLexF.FriendPart BRANDED OBJECT
    currentTok := M3CToken.Void;
    isDisabled := FALSE;
    lookahead: CARDINAL := 0;
    lookaheadBuffer: ARRAY [0..MaxLookahead] OF CHAR;
  OVERRIDES
    init := Init;
    current := Current;
    next := Next;
    position := Position;
    literal := Literal;
    identifier := Identifier;
    disable := Disable;
    disabled := Disabled;
    reset := Reset;
    currentTokenToText := CurrentTokenToText;
    get := Get;
    unget := Unget;
    readId := ReadId;
    readNumericLiteral := ReadNumericLiteral;
    readCharLiteral := ReadCharLiteral;
    readTextLiteral := ReadTextLiteral;
    readCommentOrPragma := ReadCommentOrPragma;
  END;


PROCEDURE Init(
    t: T;
    rd: Rd.T;
    identifiers: M3CReservedWord.Table;
    literals: M3CHash.Table;
    callBack: CallBack): T=
  BEGIN
    t.rd := rd; t. identifiers := identifiers;  t.literals := literals;
    t.callBack := callBack; t.line := 1;
    t.hashValue := NEW(M3CHash.Value).init(); t.tokenBuffer := NEW(Buffer);
    RETURN t;
  END Init;



<*INLINE*> PROCEDURE Get(t: T): CHAR RAISES {Rd.Failure, Rd.EndOfFile}=
  <*FATAL Thread.Alerted*>
  BEGIN
    IF t.lookahead > 0 THEN
      DEC(t.lookahead);
      RETURN t.lookaheadBuffer[t.lookahead];
    ELSE
      RETURN Rd.GetChar(t.rd);
    END;
  END Get;


<*INLINE*> PROCEDURE Unget(t: T; ch: CHAR)=
  BEGIN
    t.lookaheadBuffer[t.lookahead] := ch;
    INC(t.lookahead);
  END Unget;


(* Managing the token buffer; putting characters into the buffer and building
arrays or texts from the completed buffer *)

PROCEDURE BufferToChars(
    buffer: Buffer;
    length: CARDINAL)
    : REF ARRAY OF CHAR
   =
  VAR
    chars := NEW(REF ARRAY OF CHAR, length);
  BEGIN
    VAR
      mod: CARDINAL := length MOD NUMBER(buffer.chars);
      pos: CARDINAL;
    BEGIN
      IF mod = 0 THEN mod := NUMBER(buffer.chars) END;
      pos := length - mod;
      IF mod # 0 THEN
        SUBARRAY(chars^, pos, mod) := SUBARRAY(buffer.chars, 0, mod);
      END;
      WHILE pos # 0 DO
        buffer := buffer.overflow;
        DEC(pos, NUMBER(buffer.chars));
        SUBARRAY(chars^, pos, NUMBER(buffer.chars)) := buffer.chars;
      END;
    END;
    RETURN chars;
  END BufferToChars;


PROCEDURE BufferToText(buffer: Buffer; length: CARDINAL): TEXT=
  BEGIN
    IF length <= NUMBER(buffer.chars) THEN
      RETURN Text.FromChars(SUBARRAY(buffer.chars, 0, length));
    ELSE
      RETURN Text.FromChars(BufferToChars(buffer, length)^);
    END;
  END BufferToText;


PROCEDURE AddOverflow(buffer: Buffer): Buffer=
  BEGIN
    RETURN NEW(Buffer, overflow := buffer);
  END AddOverflow;


<*INLINE*> PROCEDURE BufferPut(
    VAR buffer: Buffer;
    pos: CARDINAL;
    ch: CHAR)
   =
  VAR
    mod: CARDINAL := pos MOD NUMBER(buffer.chars);
  BEGIN
    IF mod = 0 AND pos # 0 THEN buffer := AddOverflow(buffer) END;
    buffer.chars[mod] := ch;
  END BufferPut;


<*INLINE*> PROCEDURE HashAndBufferPut(
    ch: CHAR;
    hashValue: M3CHash.Value;
    VAR buffer: Buffer;
    VAR pos: CARDINAL)
   =
  BEGIN
    BufferPut(buffer, pos, ch);
    hashValue.addCharToValue(ch);
    INC(pos);
  END HashAndBufferPut;


(* Reading an identifier or reserved word *)

<*INLINE*> PROCEDURE IdOrReservedWord(
    t: T;
    hashValue: M3CHash.Value;
    READONLY chars: ARRAY OF CHAR)
    : M3CToken.T
   =
  VAR
    id := t.identifiers.enterCharsWithValue(hashValue, chars);
  BEGIN
    TYPECASE id OF <*NOWARN*>
    | M3CReservedWord.Id(r) =>
        RETURN M3CReservedWord.Token(r);
    | Symbol_rep(s) =>
        t.cur_identifier := s;
        RETURN M3CToken.Identifier;
    END;
  END IdOrReservedWord;


PROCEDURE ReadId(t: T; firstCh: CHAR): M3CToken.T RAISES {Rd.Failure}=
  CONST
    IdentChars = ASCII.AlphaNumerics + ASCII.Set{'_'};
  VAR
    ch := firstCh;
    hashValue := t.hashValue;
    buffer := t.tokenBuffer;
    pos: CARDINAL := 0;
  BEGIN
    hashValue.reset();
    TRY
      REPEAT
        HashAndBufferPut(ch, hashValue, buffer, pos);
        ch := Get(t);
      UNTIL NOT ch IN IdentChars;
      Unget(t, ch);
    EXCEPT
    | Rd.EndOfFile =>
    END;
    INC(t.offset, pos - 1);
    IF pos <= NUMBER(buffer.chars) THEN
      WITH chars = SUBARRAY(buffer.chars, 0, pos) DO
        RETURN IdOrReservedWord(t, hashValue, chars);
      END;
    ELSE
      RETURN IdOrReservedWord(t, hashValue, BufferToChars(buffer, pos)^);
    END;
  END ReadId;


(* Reading numeric literals *)

CONST
  BadLiteralTail = " (bad literal)"; (* Appended to all bad literals *)

PROCEDURE EnterLiteral(
    t: T;
    ok: BOOLEAN;
    hashValue: M3CHash.Value;
    buffer: Buffer;
    length: CARDINAL)
   =
  BEGIN
    IF NOT ok THEN
      FOR i := 0 TO Text.Length(BadLiteralTail) - 1 DO
        HashAndBufferPut(
            Text.GetChar(BadLiteralTail, i), hashValue, buffer, length);
      END;
    END;
    IF length <= NUMBER(buffer.chars) THEN
      WITH chars = SUBARRAY(buffer.chars, 0, length) DO
        t.cur_literal := t.literals.enterCharsWithValue(hashValue, chars);
      END;
    ELSE
      t.cur_literal := t.literals.enterCharsWithValue(
          hashValue, BufferToChars(buffer, length)^);
    END;
  END EnterLiteral;


PROCEDURE CheckedGet(t: T; VAR eof: BOOLEAN): CHAR RAISES {Rd.Failure}=
  BEGIN
    TRY
      eof := FALSE;
      RETURN Get(t);
    EXCEPT
    | Rd.EndOfFile =>
        eof := TRUE;
        RETURN '\000';
    END;
  END CheckedGet;


PROCEDURE CalculateBase(buffer: Buffer; pos: CARDINAL): CARDINAL=
  VAR
    mod := pos MOD NUMBER(buffer.chars);
    val := ORD(buffer.chars[mod]) - ORD('0');
  BEGIN
    IF pos > 0 THEN
      IF mod = 0 THEN buffer := buffer.overflow END;
      INC(val, CalculateBase(buffer, pos - 1) * 10);
    END;
    RETURN val;
  END CalculateBase;


PROCEDURE HexValue(ch: CHAR; VAR val: CARDINAL): BOOLEAN=
  BEGIN
    IF 'a' <= ch AND ch <= 'z' THEN
      val := ORD(ch) - ORD('a') + 10;
      RETURN TRUE;
    ELSIF 'A' <= ch AND ch <= 'Z' THEN
      val := ORD(ch) - ORD('A') + 10;
      RETURN TRUE;
    ELSIF '0' <= ch AND ch <= '9' THEN
      val := ORD(ch) - ORD('0');
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END; (* if *)
  END HexValue;


PROCEDURE ReadHexDigits(
    t: T;
    hashValue: M3CHash.Value;
    VAR buffer: Buffer;
    VAR pos: CARDINAL)
    : BOOLEAN
    RAISES {Rd.Failure}=
  VAR
    start := pos;
    eof: BOOLEAN;
    ch := CheckedGet(t, eof);
    val, base: CARDINAL;
    ok := TRUE;
  BEGIN
    IF HexValue(ch, val) THEN
      IF pos > 2 THEN
        base := 17; (* will force error; saves worry about overflow *)
      ELSE
        base := CalculateBase(buffer, pos - 1);
      END;
      IF 2 > base OR base > 16 THEN base := 16; ok := FALSE END;
      HashAndBufferPut('_', hashValue, buffer, pos);
      REPEAT
        IF val >= base THEN ok := FALSE END;
        HashAndBufferPut(ch, hashValue, buffer, pos);
        ch := CheckedGet(t, eof);
      UNTIL NOT HexValue(ch, val);
    END;
    IF NOT eof THEN Unget(t, ch) END;
    IF pos = start THEN Unget(t, '_') END;
    RETURN ok;
  END ReadHexDigits;


PROCEDURE ReadExponent(
    t: T;
    exponent: CHAR;
    hashValue: M3CHash.Value;
    VAR buffer: Buffer;
    VAR pos: CARDINAL)
    : M3CToken.T
    RAISES {Rd.Failure}=
  VAR
    sign := '\000';
    eof: BOOLEAN;
    ch := CheckedGet(t, eof);
  BEGIN
    IF ch = '-' OR ch = '+' THEN
      sign := ch;
      ch := CheckedGet(t, eof);
    END;
    IF ch IN ASCII.Digits THEN
      HashAndBufferPut(exponent, hashValue, buffer, pos);
      IF sign # '\000' THEN
        HashAndBufferPut(sign, hashValue, buffer, pos);
      END;
      REPEAT
        HashAndBufferPut(ch, hashValue, buffer, pos);
        ch := CheckedGet(t, eof);
      UNTIL NOT ch IN ASCII.Digits;
      IF NOT eof THEN Unget(t, ch) END;
      IF ASCII.Upper[exponent] = 'D' THEN
        RETURN M3CToken.LongRealLiteral;
      ELSIF ASCII.Upper[exponent] = 'X' THEN
        RETURN M3CToken.ExtendedLiteral;
      ELSE
        RETURN M3CToken.RealLiteral;
      END;
    ELSE
      IF NOT eof THEN Unget(t, ch) END;
      IF sign # '\000' THEN Unget(t, sign) END;
      Unget(t, exponent);
      RETURN M3CToken.RealLiteral;
    END;
  END ReadExponent;


PROCEDURE ReadRealOrRange(
    t: T;
    hashValue: M3CHash.Value;
    VAR buffer: Buffer;
    VAR pos: CARDINAL)
    : M3CToken.T
    RAISES {Rd.Failure}=
  VAR
    eof: BOOLEAN;
    ch := CheckedGet(t, eof);
  BEGIN
    IF ch IN ASCII.Digits THEN
      HashAndBufferPut('.', hashValue, buffer, pos);
      REPEAT
        HashAndBufferPut(ch, hashValue, buffer, pos);
        ch := CheckedGet(t, eof);
      UNTIL NOT ch IN ASCII.Digits;
      IF ch IN ASCII.Set{'D', 'd', 'E', 'e', 'X', 'x'} THEN
        RETURN ReadExponent(t, ch, hashValue, buffer, pos);
      ELSE
        IF NOT eof THEN Unget(t, ch) END;
        RETURN M3CToken.RealLiteral;
      END;
    ELSE
      Unget(t, ch);
      Unget(t, '.');
      RETURN M3CToken.IntegerLiteral;
    END;
  END ReadRealOrRange;


PROCEDURE ReadNumericLiteral(
    t: T;
    firstCh: CHAR)
    : M3CToken.T
    RAISES {Rd.Failure}=
  VAR
    ch := firstCh;
    hashValue := t.hashValue;
    buffer := t.tokenBuffer;
    pos: CARDINAL := 0;
    result := M3CToken.IntegerLiteral;
    ok := TRUE;
  BEGIN
    hashValue.reset();
    TRY
      REPEAT
        HashAndBufferPut(ch, hashValue, buffer, pos);
        ch := Get(t);
      UNTIL NOT ch IN ASCII.Digits;
      IF ch = '_' THEN
        ok := ReadHexDigits(t, hashValue, buffer, pos);
      ELSIF ch = '.' THEN
        result := ReadRealOrRange(t, hashValue, buffer, pos);
      ELSE
        Unget(t, ch);
      END;
    EXCEPT
    | Rd.EndOfFile =>
    END;
    INC(t.offset, pos - 1);
    EnterLiteral(t, ok, hashValue, buffer, pos);
    RETURN result;
  END ReadNumericLiteral;


(* Reading character and text literals *)

PROCEDURE ReadEscape(
    t: T;
    hashValue: M3CHash.Value;
    VAR buffer: Buffer;
    VAR pos: CARDINAL)
    : BOOLEAN
    RAISES {Rd.Failure, Rd.EndOfFile}=
  CONST
    OctalDigits = ASCII.Set{'0'..'7'};
    ValidEscapes = ASCII.Set{
        'n', 't', 'r', 'f', '\\', '\'', '\"'} +
        OctalDigits;
  VAR
    ch: CHAR;
  BEGIN
    ch := Get(t);
    IF ch IN ValidEscapes THEN
      HashAndBufferPut(ch, hashValue, buffer, pos);
      IF ch IN OctalDigits THEN
        FOR i := 1 TO 2 DO
          ch := Get(t);
          IF ch IN OctalDigits THEN
            HashAndBufferPut(ch, hashValue, buffer, pos);
          ELSE
            Unget(t, ch);
            RETURN FALSE;
          END;
        END;
      END;
      RETURN TRUE;
    ELSE
      Unget(t, ch);
      RETURN FALSE;
    END;
  END ReadEscape;


PROCEDURE ReadCharLiteral(t: T): M3CToken.T RAISES {Rd.Failure}=
  VAR
    ch: CHAR;
    hashValue := t.hashValue;
    buffer := t.tokenBuffer;
    pos: CARDINAL := 0;
    ok := TRUE;
  BEGIN
    hashValue.reset();
    HashAndBufferPut('\'', hashValue, buffer, pos);
    TRY
      ch := Get(t);
      IF ch IN ASCII.Graphics - ASCII.Set{'\''} THEN
        HashAndBufferPut(ch, hashValue, buffer, pos);
        IF ch = '\\' THEN
          ok := ReadEscape(t, hashValue, buffer, pos);
        END;
        ch := Get(t);
        IF ch = '\'' THEN
          HashAndBufferPut(ch, hashValue, buffer, pos);
        ELSE
          Unget(t, ch);
          ok := FALSE;
        END;
      ELSE
        Unget(t, ch);
        ok := FALSE;
      END;
    EXCEPT
    | Rd.EndOfFile => ok := FALSE;
    END;
    INC(t.offset, pos - 1);
    EnterLiteral(t, ok, hashValue, buffer, pos);
    RETURN M3CToken.CharLiteral;
  END ReadCharLiteral;


PROCEDURE ReadTextLiteral(t: T): M3CToken.T RAISES {Rd.Failure}=
  VAR
    ch: CHAR;
    hashValue := t.hashValue;
    buffer := t.tokenBuffer;
    pos: CARDINAL := 0;
    ok := TRUE;
  BEGIN
    hashValue.reset();
    HashAndBufferPut('\"', hashValue, buffer, pos);
    TRY
      LOOP
        ch := Get(t);
        IF ch IN ASCII.Graphics THEN
          HashAndBufferPut(ch, hashValue, buffer, pos);
          IF ch = '\\' THEN
            IF NOT ReadEscape(t, hashValue, buffer, pos) THEN ok := FALSE END;
          ELSIF ch = '\"' THEN
            EXIT;
          ELSE
            (* loop *)
          END;
        ELSE
          Unget(t, ch);
          ok := FALSE;
          EXIT;
        END;
      END;
    EXCEPT
    | Rd.EndOfFile => ok := FALSE;
    END;
    INC(t.offset, pos - 1);
    EnterLiteral(t, ok, hashValue, buffer, pos);
    RETURN M3CToken.TextLiteral;
  END ReadTextLiteral;


(* Comments and pragmas *)

PROCEDURE ReadCommentOrPragmaSection(
    t: T;
    READONLY endOfSection: ASCII.Set;
    VAR buffer: Buffer;
    VAR pos: CARDINAL)
    : CHAR
    RAISES {Rd.Failure, Rd.EndOfFile}=
  <*FATAL Thread.Alerted *>
  BEGIN
    WITH n = NUMBER(buffer.chars) DO
      LOOP
        VAR
          mod: CARDINAL := pos MOD n;
          wanted: CARDINAL := n - mod;
          got: CARDINAL;
        BEGIN
          IF mod = 0 AND pos # 0 THEN buffer := AddOverflow(buffer) END;
          got := RdExtras.GetUntil(t.rd,
              SUBARRAY(buffer.chars, mod, wanted), endOfSection);
          INC(pos, got);
          IF got > wanted THEN
            (* buffer overflow - loop to get new overflow section added *)
            DEC(pos);
          ELSE
            VAR
              save: Buffer := NIL;
            BEGIN
              IF mod = 0 AND got = 0 AND pos # 0 THEN
                (* new overflow section is empty and may not be needed if we
                 hit end of stream *)
                save := buffer;
                buffer := save.overflow;
              END;
              WITH ch = Rd.GetChar(t.rd) DO
                IF save # NIL THEN
                  save.chars[0] := ch;
                  buffer := save;
                ELSE
                  BufferPut(buffer, pos, ch);
                END;
                INC(pos);
                RETURN ch;
              END;
            END;
          END;
        END;
      END;
    END;
  END ReadCommentOrPragmaSection;


PROCEDURE ReadCommentOrPragma(t: T; isComment: BOOLEAN) RAISES {Rd.Failure}=
  <*FATAL Thread.Alerted*>
  VAR
    nesting := 0;
    pos: CARDINAL := 2;
    startOfLine := pos - t.offset;
    startChar, endChar, ch: CHAR;
    endOfSection: ASCII.Set;
    buffer := t.tokenBuffer;
  CONST
    EndOfLine = ASCII.Set{'\n', '\f'};
  BEGIN
    IF isComment THEN
      startChar := '(';
      endChar := ')';
      endOfSection := EndOfLine + ASCII.Set{'(', '*'};
    ELSE
      startChar := '<';
      endChar := '>';
      endOfSection := EndOfLine + ASCII.Set{'<', '*'};
    END;

    BufferPut(buffer, 0, startChar);
    BufferPut(buffer, 1, '*');
    (* we know the lookahead buffer is empty at this point so we can safely
     use 'ReadCommentSection' which ignores the lookahead buffer *)
    TRY
      REPEAT
        ch := ReadCommentOrPragmaSection(t, endOfSection, buffer, pos);
        REPEAT
          VAR
            next: CHAR;
          BEGIN
            IF ch # startChar AND ch # '*' THEN
              (* must be newline *)
              INC(t.linesInToken);
              startOfLine := pos;
            END;
            next := Rd.GetChar(t.rd);
            BufferPut(buffer, pos, next); INC(pos);
            IF ch = startChar THEN
              IF next = '*' THEN INC(nesting); EXIT END;
            ELSIF ch = '*' THEN
              IF next = endChar THEN DEC(nesting); EXIT END;
            END;
            ch := next;
          END;
        UNTIL NOT ch IN endOfSection;
      UNTIL nesting < 0;
    EXCEPT
    | Rd.EndOfFile =>
    END;
    WITH text = BufferToText(buffer, pos) DO
      INC(t.line, t.linesInToken);
      t.offset := pos - startOfLine;
      IF startChar = '(' THEN
        t.callBack.comment(text);
      ELSE
        t.callBack.pragma(text);
      END;
    END;
    t.linesInToken := 0;
  END ReadCommentOrPragma;


(* Get next token *)

PROCEDURE GetNext(t: T) RAISES {Rd.Failure}=
  VAR
    ch: CHAR;
  BEGIN
    TRY
      t.linesInToken := 0;
      t.currentTok := M3CToken.Void;
      LOOP
        t.startOfToken := t.offset;
        ch := Get(t); INC(t.offset);
        CASE ch OF
        | '\t', ' ', '\013', '\f', '\r' =>
        | '\n' =>
            INC(t.line);
            t.offset := 0;
        | 'A'..'Z', 'a'..'z' =>
            t.currentTok := ReadId(t, ch);
            EXIT;
        | '0'..'9'=>
            t.currentTok := ReadNumericLiteral(t, ch);
            EXIT;
        | '\'' =>
            t.currentTok := ReadCharLiteral(t);
            EXIT;
        | '\"' =>
            t.currentTok := ReadTextLiteral(t);
            EXIT;
        | '+' =>
            t.currentTok := M3CToken.Plus;
            EXIT;
        | '-' =>
            t.currentTok := M3CToken.Minus;
            EXIT;
        | '*' =>
            t.currentTok := M3CToken.Times;
            EXIT;
        | '/' =>
            t.currentTok := M3CToken.Divide;
            EXIT;
        | '<' =>
            t.currentTok := M3CToken.LessThan;
            ch := Get(t); INC(t.offset);
            IF ch = '=' THEN
              t.currentTok := M3CToken.LessThanOrEqual;
              EXIT;
            ELSIF ch = ':' THEN
              t.currentTok := M3CToken.Subtype;
              EXIT;
            ELSIF ch = '*' THEN
              ReadCommentOrPragma(t, IsPragma);
            ELSE
              Unget(t, ch); DEC(t.offset);
              EXIT;
            END; (* if *)
        | '>' =>
            ch := Get(t); INC(t.offset);
            IF ch = '=' THEN
              t.currentTok := M3CToken.GreaterThanOrEqual;
            ELSE
              Unget(t, ch); DEC(t.offset);
              t.currentTok := M3CToken.GreaterThan;
            END; (* if *)
            EXIT;
        | '#' =>
            t.currentTok := M3CToken.NotEqual;
            EXIT;
        | '=' =>
            t.currentTok := M3CToken.Equal;
            ch := Get(t); INC(t.offset);
            IF ch = '>' THEN
              t.currentTok := M3CToken.Implies;
            ELSE
              Unget(t, ch); DEC(t.offset);
            END; (* if *)
            EXIT;
        | '{' =>
            t.currentTok := M3CToken.CurlyBra;
            EXIT;
        | '}' =>
            t.currentTok := M3CToken.CurlyKet;
            EXIT;
        | '[' =>
            t.currentTok := M3CToken.SquareBra;
            EXIT;
        | ']' =>
            t.currentTok := M3CToken.SquareKet;
            EXIT;
        | '(' =>
            t.currentTok := M3CToken.Bra;
            ch := Get(t); INC(t.offset);
            IF ch = '*' THEN
              ReadCommentOrPragma(t, IsComment);
            ELSE
              Unget(t, ch); DEC(t.offset);
              EXIT;
            END; (* if *)
        | ')' =>
            t.currentTok := M3CToken.Ket;
            EXIT;
        | ';' =>
            t.currentTok := M3CToken.Semicolon;
            EXIT;
        | '|' =>
            t.currentTok := M3CToken.Bar;
            EXIT;
        | '^' =>
            t.currentTok := M3CToken.Dereference;
            EXIT;
        | '.' =>
            t.currentTok := M3CToken.Dot;
            ch := Get(t); INC(t.offset);
            IF ch = '.' THEN
              t.currentTok := M3CToken.Range;
            ELSE
              Unget(t, ch); DEC(t.offset);
            END; (* if *)
            EXIT;
        | ':' =>
            t.currentTok := M3CToken.Colon;
            ch := Get(t); INC(t.offset);
            IF ch = '=' THEN
              t.currentTok := M3CToken.Becomes;
            ELSE
              Unget(t, ch); DEC(t.offset);
            END; (* if *)
            EXIT;
        | ',' =>
            t.currentTok := M3CToken.Comma;
            EXIT;
        | '&' =>
            t.currentTok := M3CToken.Ampersand;
            EXIT;
        ELSE
          IF t.isDisabled THEN
            Unget(t, '\000'); DEC(t.offset);
            EXIT;
          ELSE
            t.callBack.badChar(ch);
          END;
        END; (* case *)
      END;
    EXCEPT
    | Rd.EndOfFile =>
    END;
  END GetNext;


<*INLINE*> PROCEDURE Current(t: T): M3CToken.T=
  BEGIN
    RETURN t.currentTok;
  END Current;


<*INLINE*> PROCEDURE Next(t: T): M3CToken.T RAISES {Rd.Failure}=
  BEGIN
    GetNext(t);
    RETURN t.currentTok
  END Next;


<*INLINE*> PROCEDURE Position(t: T): M3CSrcPos.T=
  BEGIN
    RETURN M3CSrcPos.Pack(t.line - t.linesInToken, t.startOfToken);
  END Position;


<*INLINE*> PROCEDURE Literal(t: T): Literal_rep=
  BEGIN
    RETURN t.cur_literal;
  END Literal;


<*INLINE*> PROCEDURE Identifier(t: T): Symbol_rep=
  BEGIN
    RETURN t.cur_identifier;
  END Identifier;


PROCEDURE Reset(t: T; pos := M3CSrcPos.Null; s: Rd.T := NIL)=
  BEGIN
    IF s # NIL THEN
      t.rd := s;
      IF pos = M3CSrcPos.Null THEN
        t.line := 1; t.offset := 0;
      END;
    END;
    IF pos # M3CSrcPos.Null THEN
      t.line := M3CSrcPos.Unpack(pos, t.offset);
    END;
    t.currentTok := M3CToken.Void;
    t.cur_identifier := NIL;
    t.cur_literal := NIL;
    t.isDisabled := FALSE;
    t.linesInToken := 0;
    t.startOfToken := 0;
    t.lookahead := 0;
  END Reset;


PROCEDURE Disable(t: T)=
  BEGIN
    Unget(t, '\000');
    t.isDisabled := TRUE;
  END Disable;


PROCEDURE Disabled(t: T): BOOLEAN=
  BEGIN
    RETURN t.isDisabled;
  END Disabled;


PROCEDURE TokenToText(token: M3CToken.T): TEXT=
  BEGIN
    CASE token OF <*NOWARN*>
    | ORD(FIRST(M3CToken.ReservedWord))..ORD(LAST(M3CToken.ReservedWord)) =>
        RETURN M3CToken.Texts[VAL(token, M3CToken.E)];
    | M3CToken.Identifier =>
        RETURN "identifier"
    | M3CToken.CharLiteral =>
        RETURN "char literal";
    | M3CToken.TextLiteral =>
        RETURN "text literal";
    | M3CToken.IntegerLiteral =>
        RETURN "integer literal";
    | M3CToken.RealLiteral =>
        RETURN "real literal";
    | M3CToken.LongRealLiteral =>
        RETURN "longreal literal";
    | M3CToken.ExtendedLiteral =>
        RETURN "extended literal";
    | M3CToken.Plus =>
        RETURN "\'+\'";
    | M3CToken.Minus =>
        RETURN "\'-\'";
    | M3CToken.Times =>
        RETURN "\'*\'";
    | M3CToken.Divide =>
        RETURN "\'/\'";
    | M3CToken.Equal =>
        RETURN "\'=\'";
    | M3CToken.NotEqual =>
        RETURN "\'#\'";
    | M3CToken.LessThan =>
        RETURN "\'<\'";
    | M3CToken.GreaterThan =>
        RETURN "\'>\'";
    | M3CToken.LessThanOrEqual =>
        RETURN "\'<=\'";
    | M3CToken.GreaterThanOrEqual =>
        RETURN "\'>=\'";
    | M3CToken.Ampersand =>
        RETURN "\'&\'";
    | M3CToken.Dereference =>
        RETURN "\'^\'";
    | M3CToken.Dot =>
        RETURN "\'.\'";
    | M3CToken.Bra =>
        RETURN "\'(\'";
    | M3CToken.Ket =>
        RETURN "\')\'";
    | M3CToken.CurlyBra =>
        RETURN "\'{\'";
    | M3CToken.CurlyKet =>
        RETURN "\'}\'";
    | M3CToken.SquareBra =>
        RETURN "\'[\'";
    | M3CToken.SquareKet =>
        RETURN "\']\'";
    | M3CToken.Becomes =>
        RETURN "\':=\'";
    | M3CToken.Semicolon =>
        RETURN "\';\'";
    | M3CToken.Comma =>
        RETURN "\',\'";
    | M3CToken.Colon =>
        RETURN "\':\'";
    | M3CToken.Bar =>
        RETURN "\'|\'";
    | M3CToken.Range =>
        RETURN "\'..\'";
    | M3CToken.Subtype =>
        RETURN "\'<:\'";
    | M3CToken.Implies =>
        RETURN "\'=>\'";
    | M3CToken.Void =>
        RETURN "end of stream";
    END; (* case *)
  END TokenToText;


PROCEDURE CurrentTokenToText(t: T): TEXT=
  VAR
    text := TokenToText(t.currentTok);
  BEGIN
    CASE t.currentTok OF
    | M3CToken.Identifier =>
        text := Fmt.F("identifier \'%s\'", t.cur_identifier.toText());
    | ORD(FIRST(M3CToken.Literal))..ORD(LAST(M3CToken.Literal)) =>
        text := Fmt.F("%s %s", text, t.cur_literal.toText());
    ELSE
    END;
    RETURN text;
  END CurrentTokenToText;


BEGIN
END M3CLex.
