(* Copyright (C) 1992, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(* File: Scanner.m3                                              *)
(* Last modified on Thu Dec  8 10:44:07 PST 1994 by kalsow       *)
(*      modified on Sat Mar 16 00:25:08 1991 by muller           *)
(*      modified on Fri Oct 19 10:52:56 1990 by nr@princeton.edu *)

MODULE M3Scanner;

IMPORT Rd, Thread, Fmt;
IMPORT M3Token, M3ID;

CONST
  EOFChar   = '\000';

TYPE
  TK = M3Token.T;

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
  Default = Public BRANDED "M3Scanner.Default" OBJECT
      skip_comments   : BOOLEAN  := FALSE;
      split_pragmas   : BOOLEAN  := FALSE;
      line_start      : CARDINAL := 0;
      next_offset     : CARDINAL := 0;
      next_line       : CARDINAL := 0;
      next_line_start : CARDINAL := 0;
    OVERRIDES
      next        := GetToken;
      initFromRd  := InitFromRd;
      initFromBuf := InitFromBuf;
    END;

PROCEDURE InitFromRd (t: Default;
                      source        : Rd.T;
                      skip_comments : BOOLEAN;
                      split_pragmas : BOOLEAN): T =
  VAR len0, len1: INTEGER;
  BEGIN
    t.skip_comments := skip_comments;
    t.split_pragmas := split_pragmas;
    t.line          := 1;
    t.next_line     := 1;

    (* read the source *)
    TRY
      len0 := Rd.Length (source);
      t.buffer := NEW (Buf, MAX (0, len0) + 1);
      IF (len0 < 0) THEN Err (t, 0, "can't tell how much source there is") END;
      len1 := Rd.GetSub (source, t.buffer^);
      IF (len0 # len1) THEN Err (t, 0, "couldn't read the entire source") END;
      t.buffer [len0] := EOFChar;
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
    t.token       := TK.Error;
    t.msg         := msg;
    t.length      := offset - t.offset;
    t.next_offset := offset;
  END Err;

PROCEDURE GetToken (t: Default) =
  VAR
    buf    := t.buffer;
    offset := t.next_offset;
    ch     := buf [offset];
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
        ch := buf [offset];
      END;

      (* remember where this token starts *)
      t.offset := offset;
      t.start  := offset;
      t.column := offset - t.line_start;

      CASE ch OF

      | 'a'..'z', 'A'..'Z' =>
          (* scan an identifier *)
          WHILE AlphaNumerics[ch] DO INC (offset); ch := buf[offset]; END;
          t.token := M3ID.Classify (SUBARRAY (buf^, t.offset, offset - t.offset));
          EXIT;

      | '0'..'9' => ScanNumber (t);                          RETURN;
      | '\''     => ScanChar (t);                            RETURN;
      | '\"'     => ScanText (t);                            RETURN;
      | '+'      => t.token := TK.Plus;       INC (offset);  EXIT;
      | '-'      => t.token := TK.Minus;      INC (offset);  EXIT;
      | '/'      => t.token := TK.Slash;      INC (offset);  EXIT;
      | '&'      => t.token := TK.Ampersand;  INC (offset);  EXIT;
      | ','      => t.token := TK.Comma;      INC (offset);  EXIT;
      | ';'      => t.token := TK.Semi;       INC (offset);  EXIT;
      | '['      => t.token := TK.L_bracket;  INC (offset);  EXIT;
      | '{'      => t.token := TK.L_brace;    INC (offset);  EXIT;
      | '^'      => t.token := TK.Arrow;      INC (offset);  EXIT;
      | '#'      => t.token := TK.Sharp;      INC (offset);  EXIT;
      | ')'      => t.token := TK.R_paren;    INC (offset);  EXIT;
      | ']'      => t.token := TK.R_bracket;  INC (offset);  EXIT;
      | '}'      => t.token := TK.R_brace;    INC (offset);  EXIT;
      | '|'      => t.token := TK.Bar;        INC (offset);  EXIT;
      | EOFChar  => t.token := TK.EOF;                       EXIT;

      | '*' => (* '*>' '*' *)
	    INC (offset);  ch := buf [offset];
            IF (ch = '>')
	      THEN  t.token := TK.End_pragma;  INC (offset);
              ELSE  t.token := TK.Asterisk;
            END;
            EXIT;

      | '=' => (*  '='  '=>'  *)
            INC (offset);  ch := buf [offset];
            IF (ch = '>')
	      THEN  t.token := TK.Implies;  INC (offset);
              ELSE  t.token := TK.Equal;
            END;
            EXIT;

      | ':' => (*  ':'  ':='  *)
            INC (offset);  ch := buf [offset];

            IF (ch = '=')
	      THEN  t.token := TK.Assign;  INC (offset);
              ELSE  t.token := TK.Colon;
            END;
            EXIT;

      | '.' => (*  '.'  '..'  *)
            INC (offset);  ch := buf [offset];
            IF (ch = '.')
	      THEN  t.token := TK.Dot_dot;  INC (offset);
              ELSE  t.token := TK.Dot;
            END;
            EXIT;

      | '(' => (*  '('*'  '('  *)
            INC (offset);  ch := buf [offset];
            IF    (ch # '*')      THEN  t.token := TK.L_paren; EXIT;
            ELSIF ScanComment (t) THEN  RETURN;
            ELSE  offset := t.offset;  ch := buf [offset];
            END;

      | '>' => (*  '>'  '>='  *)
            INC (offset);  ch := buf [offset];
            IF (ch = '=')
	      THEN  t.token := TK.Gr_equal;  INC (offset);
              ELSE  t.token := TK.Greater;
            END;
            EXIT;

      | '<' => (*  '<'  '<='  '<:'  '<*' *)
            INC (offset);  ch := buf [offset];
            IF    (ch = '=') THEN  t.token := TK.Ls_equal; INC (offset);
            ELSIF (ch = ':') THEN  t.token := TK.Subtype;  INC (offset);
            ELSIF (ch # '*') THEN  t.token := TK.Less;
            ELSIF (NOT t.split_pragmas) THEN  ScanPragma (t);  RETURN;
            ELSE  t.token := TK.Begin_pragma;  INC (offset);
            END;
            EXIT;

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
    ch     := t.buffer [offset];
  BEGIN
    (* scan the decimal digits *)
    WHILE Digits[ch] DO  INC (offset);  ch := t.buffer [offset];  END;
           
    IF (ch = '_') THEN
      (* scan a based integer *)
      INC (offset);  ch := t.buffer [offset];
      IF HexDigits[ch] THEN
        WHILE HexDigits[ch] DO  INC (offset);  ch := t.buffer [offset];  END;
        t.token := TK.Card_const;
      ELSE
        Err (t, offset, "illegal based integer: missing digits");
        RETURN;
      END;
               
    ELSIF (ch = '.') AND (t.buffer [offset+1] = '.') THEN
      (* we saw  "dddd.." *)
      t.token := TK.Card_const;
                  
    ELSIF (ch = '.') THEN
      (* scan a floating point number *)
      INC (offset);  ch := t.buffer [offset];
                     
      (* scan the fractional digits *)
      IF Digits[ch] THEN
        WHILE Digits[ch] DO  INC (offset);  ch := t.buffer [offset];  END;
      ELSE
        Err (t, offset, "missing digits in real fraction");
        RETURN;
      END;
                        
      (* check for the exponent *)
      IF (ch = 'e') OR (ch = 'E') THEN
        t.token := TK.Real_const;
      ELSIF (ch = 'd') OR (ch = 'D') THEN
        t.token := TK.Longreal_const;
      ELSIF (ch = 'x') OR (ch = 'X') THEN
        t.token := TK.Extended_const;
      ELSE (* real constant with no exponent *)
        t.token := TK.Real_const;
        t.next_offset := offset;
        t.length := offset - t.offset;
        RETURN;
      END;
      INC (offset);  ch := t.buffer [offset]; (* eat the e/d/x *)
                           
      (* get the exponent sign *)
      IF (ch = '+') OR (ch = '-') THEN
        INC (offset);  ch := t.buffer [offset];
      END;
                              
      (* finally, get the exponent digits *)
      IF Digits[ch] THEN
        WHILE Digits[ch] DO  INC (offset);  ch := t.buffer [offset];  END;
      ELSE
        Err (t, offset, "missing digits in real exponent");
      END;
                                 
    ELSE
      (* scanned a simple decimal integer *)
      t.token := TK.Card_const;
                                    
    END;
                                    
    t.length := offset - t.offset;
    t.next_offset := offset;
  END ScanNumber;
                                    
PROCEDURE ScanChar (t: Default) =
  VAR
    offset : CARDINAL := t.offset+1;
    ch     : CHAR     := t.buffer [offset];
  BEGIN
    IF NOT CharAlert [ch] THEN
      (* a simple character literal *)
      INC (offset);  ch := t.buffer[offset];
    ELSIF (ch = '\'') THEN
      INC (offset);
      Err (t, offset, "missing character in character literal");
      RETURN;
    ELSIF (ch = '\n') OR (ch = '\r') OR (ch = '\f') THEN
      Err (t, offset, "end-of-line encountered in character literal");
      RETURN;
    ELSIF (ch = '\\') THEN
      INC (offset);
      IF NOT ScanEscape (t, offset) THEN
        Err (t, offset, "unknown escape sequence in character literal");
        RETURN;
      END;
      ch := t.buffer [offset];
    ELSIF (ch = EOFChar) THEN
      Err (t, offset, "EOF encountered in character literal");
      RETURN;
    ELSE
      (* a simple character literal *)
      INC (offset);  ch := t.buffer[offset];
    END;

    IF (ch # '\'') THEN
      Err (t, offset, "missing closing quote on character literal");
      RETURN;
    END;
    INC (offset); (* eat the closing quote *)

    t.token := TK.Char_const;
    t.length := offset - t.offset;
    t.next_offset := offset;
  END ScanChar;

PROCEDURE ScanText (t: Default) =
  VAR
    offset : CARDINAL := t.offset+1;
    ch     : CHAR     := t.buffer [offset];
  BEGIN
    LOOP
      IF NOT CharAlert [ch] THEN
        INC (offset);  ch := t.buffer [offset];
      ELSIF (ch = '\"') THEN
        INC (offset);
        EXIT;
      ELSIF (ch = '\n') OR (ch = '\r') OR (ch = '\f') THEN
        Err (t, offset, "end-of-line encountered in text literal");
        RETURN;
      ELSIF (ch = '\\') THEN
        INC (offset);
        IF NOT ScanEscape (t, offset) THEN
          Err (t, offset, "unknown escape sequence in text literal");
          RETURN;
        END;
        ch := t.buffer [offset];
      ELSIF (ch = EOFChar) THEN
        Err (t, offset, "EOF encountered in text literal");
        RETURN;
      ELSE (* a simple character *)
        INC (offset);  ch := t.buffer [offset];
      END;
    END;

    t.token := TK.Text_const;
    t.length := offset - t.offset;
    t.next_offset := offset;
  END ScanText;

PROCEDURE ScanEscape (t: Default;  VAR offset: CARDINAL): BOOLEAN =
  VAR ch := t.buffer [offset];
  BEGIN
    INC (offset);
    IF (ch = 'n') OR (ch = 't') OR (ch = 'r') OR (ch = 'f') 
      OR (ch = '\\') OR (ch = '\'') OR (ch = '\"') THEN
      RETURN TRUE;
    END;
    IF NOT OctalDigits [ch] THEN RETURN FALSE END;
    ch := t.buffer [offset];  INC (offset);
    IF NOT OctalDigits [ch] THEN RETURN FALSE END;
    ch := t.buffer [offset];  INC (offset);
    RETURN OctalDigits [ch];
  END ScanEscape;

PROCEDURE ScanComment (t: Default): BOOLEAN =
  VAR
    nest    := 1;
    start   := t.offset;
    offset  := start + 2; (* skip the opening '(' and '*' *)
    ch      := t.buffer [offset];
    my_line := t.line;
    l_start := t.line_start;
  BEGIN
    WHILE (nest > 0) DO
      (************** UNSAFE VERSION **********************
      VAR p: UNTRACED REF CHAR;
      p := ADR (t.buffer[offset]);
      WHILE NOT CommentAlert[ch] DO INC (p);  ch := p^; END;
      offset := p - ADR (t.buffer[0]);
      ******************************************************)
      WHILE NOT CommentAlert[ch] DO INC (offset);  ch := t.buffer[offset]; END;
      IF (ch = '*') THEN
        INC (offset);  ch := t.buffer [offset];
        IF (ch = ')') THEN
          INC (offset);  ch := t.buffer[offset];
          DEC (nest);
        END;
      ELSIF (ch = '(') THEN
        INC (offset);  ch := t.buffer [offset];
        IF (ch = '*') THEN
          INC (offset);  ch := t.buffer[offset];
          INC (nest);
        END;
      ELSIF (ch = EOFChar) THEN
        t.next_line         := my_line;
        t.next_line_start   := l_start;
        Err (t, offset, "EOF encountered in comment");
        RETURN TRUE;
      ELSIF (ch = '\n') THEN
        INC (offset);  ch := t.buffer [offset];
        INC (my_line);  l_start := offset;
      ELSE
        INC (offset);  ch := t.buffer [offset];
      END;
    END;

    IF t.skip_comments THEN
      t.offset     := offset;
      t.line       := my_line;
      t.line_start := l_start;
      RETURN FALSE;
    ELSE
      t.token           := TK.Comment;
      t.length          := offset - t.offset;
      t.next_offset     := offset;
      t.next_line       := my_line;
      t.next_line_start := l_start;
      RETURN TRUE;
    END;
  END ScanComment;

PROCEDURE ScanPragma (t: Default) =
  VAR
    start   := t.offset;
    offset  := start + 2; (* skip the opening '<' and '*' *)
    ch      := t.buffer [offset];
    my_line := t.line;
    l_start := t.line_start;
  BEGIN
    (* scan the entire pragma as a string *)
    WHILE (ch # '*') OR (t.buffer[offset+1] # '>') DO
      IF (ch = EOFChar) THEN
        Err (t, offset, "EOF encountered in pragma");
        RETURN;
      END;
      INC (offset);
      IF (ch = '\n') THEN  INC (my_line);  l_start := offset;  END;
      ch := t.buffer [offset];
    END;
    INC (offset, 2);

    t.token           := TK.Begin_pragma;
    t.length          := offset - t.offset;
    t.next_offset     := offset;
    t.next_line       := my_line;
    t.next_line_start := l_start;
  END ScanPragma;


BEGIN
  WhiteSpace [' ']  := TRUE;
  WhiteSpace ['\n'] := TRUE;
  WhiteSpace ['\t'] := TRUE;
  WhiteSpace ['\r'] := TRUE;
  WhiteSpace ['\f'] := TRUE;

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
  CharAlert ['\\'] := TRUE;
  CharAlert [EOFChar] := TRUE;
END M3Scanner.
