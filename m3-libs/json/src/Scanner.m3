MODULE Scanner;

IMPORT Rd, Text, Thread, Fmt;
IMPORT Keyword;

CONST
  EOFChar   = '\000';
  MaxBuf    = 1024;

VAR (* CONST after intialization *)
  WhiteSpace    := ARRAY CHAR OF BOOLEAN { FALSE, .. };
  AlphaNumerics := ARRAY CHAR OF BOOLEAN { FALSE, .. };
  Digits        := ARRAY CHAR OF BOOLEAN { FALSE, .. };
  OctalDigits   := ARRAY CHAR OF BOOLEAN { FALSE, .. };
  HexDigits     := ARRAY CHAR OF BOOLEAN { FALSE, .. };
  CommentAlert  := ARRAY CHAR OF BOOLEAN { FALSE, .. };
  CharAlert     := ARRAY CHAR OF BOOLEAN { FALSE, .. };

TYPE
  Public = T OBJECT METHODS
    initFromRd  (source        : Rd.T;
                 skip_comments := TRUE;
                 split_pragmas := TRUE): T;
    initFromBuf (buf           : Buf;
                 skip_comments := TRUE;
                 split_pragmas := TRUE): T;
  END;

REVEAL
  Default = Public BRANDED "Scanner.Default" OBJECT
      skip_comments   : BOOLEAN  := FALSE;
      split_pragmas   : BOOLEAN  := FALSE;
      line_start      : CARDINAL := 0;
      next_offset     : CARDINAL := 0;
      next_line       : CARDINAL := 0;
      next_line_start : CARDINAL := 0;
      input_len       : CARDINAL := 0;
      source          : Rd.T;
    METHODS
      getCh(offset : CARDINAL) : CHAR := GetCh;
    OVERRIDES
      next        := GetToken;
      toText      := ToText;
      className   := ClassName;
      initFromRd  := InitFromRd;
      initFromBuf := InitFromBuf;
    END;

PROCEDURE ToText (t: Default): TEXT =
  BEGIN
    RETURN Text.FromChars (SUBARRAY (t.buffer^, t.offset, t.length));
  END ToText;

PROCEDURE ClassName (<*UNUSED*> t: Default;   tk: INTEGER): TEXT =
  BEGIN
    IF (FIRST (TokenName) <= tk) AND (tk <= LAST (TokenName))
      THEN RETURN TokenName [tk];
      ELSE RETURN NIL;
    END;
  END ClassName;

PROCEDURE InitFromRd (t: Default;
                      source        : Rd.T;
                      skip_comments : BOOLEAN;
                      split_pragmas : BOOLEAN): T =
  VAR len0 (*, len1*): INTEGER;
  BEGIN
    t.skip_comments := skip_comments;
    t.split_pragmas := split_pragmas;
    t.line          := 1;
    t.next_line     := 1;

    (* read the source *)
    TRY
      t.source := source;
      t.buffer := NEW(Buf, MaxBuf);
      len0 := Rd.GetSub (source, t.buffer^);
      IF len0 < MaxBuf THEN
        t.buffer[len0] := EOFChar;
      END;
    EXCEPT
    | Rd.Failure     => Err (t, 0, "I/O failure reading the source");
    | Thread.Alerted => Err (t, 0, "*alerted*");
    END;
    RETURN t;
  END InitFromRd;

PROCEDURE InitFromBuf (t: Default;
                       buf           : Buf;
                       skip_comments : BOOLEAN;
                       split_pragmas : BOOLEAN): T =
  BEGIN
    t.skip_comments := skip_comments;
    t.split_pragmas := split_pragmas;
    t.line          := 1;
    t.next_line     := 1;
    t.buffer        := buf;
    RETURN t;
  END InitFromBuf;

PROCEDURE Err (t: Default;  offset: CARDINAL;  msg: TEXT) =
  BEGIN
    t.token       := TK_Error;
    t.msg         := msg;
    t.length      := offset - t.offset;
    t.next_offset := offset;
  END Err;

PROCEDURE GetCh(t : Default; offset : CARDINAL) : CHAR =
  VAR
    n,m : CARDINAL;
    new : Buf;
  BEGIN
    IF offset >= NUMBER(t.buffer^) THEN
      n := NUMBER(t.buffer^);
      new := NEW(Buf, n + n);
      SUBARRAY(new^,0,n) := t.buffer^;
      TRY
        m := Rd.GetSub (t.source, SUBARRAY(new^,n,n));
      EXCEPT
      | Rd.Failure, Thread.Alerted => Err(t,offset,"Read failure");
      END;
      IF m < n THEN
        new[n+m] := EOFChar;
      END;
      t.buffer := new;
    END;
    RETURN t.buffer [offset];
  END GetCh;

PROCEDURE GetToken (t: Default) =
  VAR
    offset := t.next_offset;
    ch := t.getCh(offset);
  BEGIN
    t.line       := t.next_line;
    t.line_start := t.next_line_start;

    LOOP
      (* skip white space *)
      WHILE (WhiteSpace[ch]) DO
        INC (offset);
        IF (ch = '\n') THEN
          INC (t.line);
          INC (t.next_line);
          t.line_start := offset;
          t.next_line_start := offset;
        END;
        ch := t.getCh(offset);

      END;

      (* remember where this token starts *)
      t.offset := offset;
      t.start  := offset;
      t.column := offset - t.line_start;

      CASE ch OF

      | 'a'..'z', 'A'..'Z' =>
          (* scan an identifier *)
          WHILE AlphaNumerics[ch] DO INC (offset); ch := t.getCh(offset); END;
          t.token := Keyword.Classify (SUBARRAY (t.buffer^, t.offset,
                                                 offset - t.offset));
          EXIT;
      | '-', '0'..'9' => ScanNumber (t);                     RETURN;
      | '\"'     => ScanText (t);                            RETURN;
      | ','      => t.token := TK_Comma;      INC (offset);  EXIT;
      | ':'      => t.token := TK_Colon;      INC (offset);  EXIT;
      | '['      => t.token := TK_L_bracket;  INC (offset);  EXIT;
      | '{'      => t.token := TK_L_brace;    INC (offset);  EXIT;
      | ']'      => t.token := TK_R_bracket;  INC (offset);  EXIT;
      | '}'      => t.token := TK_R_brace;    INC (offset);  EXIT;
      | EOFChar  => t.token := TK_EOF;                       EXIT;
      ELSE
        INC (offset);
        Err (t, offset, "Illegal character: " & Fmt.Int (ORD (ch)));
        EXIT;
      END; (*case*)
    END; (*loop*)

    (* record the length of the token and where we are *)
    t.length := offset - t.offset;
    t.next_offset := offset;
  END GetToken;

PROCEDURE ScanNumber (t: Default) =
  VAR
    offset := t.offset;
    ch := t.getCh(offset);
  BEGIN
    IF (ch = '-') THEN
      INC (offset); ch := t.getCh(offset);
    END;

    IF NOT Digits[ch] THEN
      Err(t, offset, "missing integer digits");
      RETURN;
    END;

    (* scan the decimal digits *)
    WHILE Digits[ch] DO  INC (offset); ch := t.getCh(offset); END;
    (* assume integer *)
    t.token := TK_Card_const;

    IF (ch = '.') THEN
      (* scan a floating point number *)
      INC (offset);
      ch := t.getCh(offset);
      t.token := TK_Real_const;

      (* scan the fractional digits *)
      IF Digits[ch] THEN
        WHILE Digits[ch] DO INC (offset); ch := t.getCh(offset); END;
      ELSE
        Err (t, offset, "missing digits in real fraction");
        RETURN;
      END;
    END;

    (* check for the exponent *)
    IF (ch = 'e') OR (ch = 'E') THEN
      t.token := TK_Real_const;
    ELSE (* it was an int constant *)
      t.next_offset := offset;
      t.length := offset - t.offset;
      RETURN;
    END;
    INC (offset);
    ch := t.getCh(offset); (* eat the e/E *)

    (* get the exponent sign *)
    IF (ch = '+') OR (ch = '-') THEN
      INC (offset); 
      ch := t.getCh(offset);
    END;

    (* finally, get the exponent digits *)
    IF Digits[ch] THEN
      WHILE Digits[ch] DO  INC (offset); ch := t.getCh(offset); END;
    ELSE
      Err (t, offset, "missing digits in real exponent");
    END;

    t.length := offset - t.offset;
    t.next_offset := offset;
  END ScanNumber;

PROCEDURE ScanText (t: Default) =
  VAR
    offset : CARDINAL := t.offset+1;
    ch := t.getCh(offset);
  BEGIN
    LOOP
      IF NOT CharAlert [ch] THEN
        INC (offset); ch := t.getCh(offset);
      ELSIF (ch = '\"') THEN
        INC (offset);
        EXIT;
      ELSIF (ch = '\n') OR (ch = '\r') OR (ch = '\f') OR (ch ='\t') THEN
        Err (t, offset, "end-of-line encountered in text literal");
        RETURN;
      ELSIF (ch = '\\') THEN
        INC (offset);
        IF NOT ScanEscape (t, offset) THEN
          Err (t, offset, "unknown escape sequence in text literal");
          RETURN;
        END;
        ch := t.getCh(offset);
      ELSIF (ch = EOFChar) THEN
        Err (t, offset, "EOF encountered in text literal");
        RETURN;
      ELSE (* a simple character *)
        INC (offset);
        ch := t.getCh(offset);
      END;
    END;

    t.token := TK_Text_const;
    (* to remove the double quotes from token *)
    INC(t.offset);
    t.length := offset - t.offset;
    (* to remove the double quotes from token *)
    DEC(t.length);
    t.next_offset := offset;
  END ScanText;

PROCEDURE ScanEscape (t: Default;  VAR offset: CARDINAL): BOOLEAN =
  VAR 
    ch := t.getCh(offset);
  BEGIN
    INC (offset);
    IF (ch = 'n') OR (ch = 't') OR (ch = 'r') OR (ch = 'f')
      OR (ch = 'b') OR (ch = '/')
      OR (ch = '\\') OR (ch = '\"') THEN
      RETURN TRUE;
    END;
    IF (ch = 'u') THEN
      ch := t.getCh(offset); INC (offset);
      IF NOT HexDigits [ch] THEN RETURN FALSE END;
      ch := t.getCh(offset); INC (offset);
      IF NOT HexDigits [ch] THEN RETURN FALSE END;
      ch := t.getCh(offset); INC (offset);
      IF NOT HexDigits [ch] THEN RETURN FALSE END;
      ch := t.getCh(offset); INC (offset);
      RETURN HexDigits [ch];
    ELSE
      RETURN FALSE;
    END;
  END ScanEscape;

BEGIN
  WhiteSpace [' ']  := TRUE;
  WhiteSpace ['\n'] := TRUE;
  WhiteSpace ['\t'] := TRUE;
  WhiteSpace ['\r'] := TRUE;

  AlphaNumerics ['_'] := TRUE;
  FOR c := 'a' TO 'z' DO AlphaNumerics [c] := TRUE END;
  FOR c := 'A' TO 'Z' DO AlphaNumerics [c] := TRUE END;
  FOR c := '0' TO '9' DO AlphaNumerics [c] := TRUE END;

  FOR c := '0' TO '9' DO Digits [c] := TRUE END;
  FOR c := '0' TO '7' DO OctalDigits [c] := TRUE END;
  FOR c := '0' TO '9' DO HexDigits [c] := TRUE END;
  FOR c := 'a' TO 'f' DO HexDigits [c] := TRUE END;
  FOR c := 'A' TO 'F' DO HexDigits [c] := TRUE END;

  CommentAlert ['*']     := TRUE;
  CommentAlert ['(']     := TRUE;
  CommentAlert [EOFChar] := TRUE;
  CommentAlert ['\n']    := TRUE;

  CharAlert ['\''] := TRUE;
  CharAlert ['\"'] := TRUE;
  CharAlert ['\n'] := TRUE;
  CharAlert ['\r'] := TRUE;
  CharAlert ['\f'] := TRUE;
  CharAlert ['\t'] := TRUE;
  CharAlert ['\\'] := TRUE;
  CharAlert [EOFChar] := TRUE;
END Scanner.
