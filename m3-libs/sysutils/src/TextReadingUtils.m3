(* Copyright 1999-2002 elego Software Solutions GmbH, Berlin, Germany.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(*--------------------------------------------------------------------*)
MODULE TextReadingUtils EXPORTS TextReadingUtils;

IMPORT Rd, ASCII, Thread, Text, TextSeq, TextRd;
FROM Thread IMPORT Alerted;

(*--------------------------------------------------------------------*)
CONST SingleQuote = '\'';
CONST DoubleQuote = '\"';
CONST delimiters  = ASCII.Set{SingleQuote, DoubleQuote};
CONST StringChars = ASCII.All - delimiters;

(*--------------------------------------------------------------------*)
PROCEDURE GetString(rd : Rd.T) : TEXT
  RAISES {Rd.Failure, Rd.EndOfFile, Alerted} =
  VAR t : TEXT := "";
      c : CHAR;
  BEGIN
    c := RdExtras_Skip(rd, StringChars, FALSE);
    t := RdExtras_GetText(rd, ASCII.Set{}, ASCII.Set{c}, FALSE);
    RETURN t;
  END GetString;

(*--------------------------------------------------------------------*)
PROCEDURE GetToken(rd : Rd.T; skip := ASCII.Spaces;
                   terminate := ASCII.Spaces; unget := FALSE) : TEXT
  RAISES {Rd.Failure, Rd.EndOfFile, Alerted} =
  BEGIN
    RETURN RdExtras_GetText(rd, skip, terminate, unget);
  END GetToken;

(*--------------------------------------------------------------------*)
PROCEDURE GetTokenOrString(rd : Rd.T; skip := ASCII.Spaces;
                           terminate := ASCII.Spaces) : TEXT
  RAISES {Rd.Failure, Rd.EndOfFile, Alerted} =
  VAR c : CHAR;
  BEGIN
    c := RdExtras_Skip(rd, skip, TRUE);
    IF c = SingleQuote OR c = DoubleQuote THEN
      RETURN GetString(rd);
    ELSE
      RETURN RdExtras_GetText(rd, ASCII.Set{}, terminate);
    END;
  END GetTokenOrString;

(*--------------------------------------------------------------------*)
PROCEDURE GetStringOrLine(rd : Rd.T) : TEXT
  RAISES {Rd.Failure, Rd.EndOfFile, Alerted} =
  VAR c : CHAR;
  BEGIN
    c := RdExtras_Skip(rd, ASCII.Spaces, TRUE);
    IF c = SingleQuote OR c = DoubleQuote THEN
      RETURN GetString(rd);
    ELSE
      RETURN Rd.GetLine(rd);
    END;
  END GetStringOrLine;

(*--------------------------------------------------------------------*)
PROCEDURE Tokenize(t : TEXT; sep := ASCII.Spaces) : TextSeq.T =
  VAR
    rd := TextRd.New(t);
    res := NEW(TextSeq.T).init();
  BEGIN
    TRY
      WHILE NOT Rd.EOF(rd) DO
        WITH token = GetTokenOrString(rd, sep, sep) DO
          res.addhi(token);
        END;
      END;
    EXCEPT ELSE
    END;
    RETURN res;
  END Tokenize;

(*--------------------------------------------------------------------*)

PROCEDURE RdExtras_Skip(
    s: Rd.T;
    READONLY skip := ASCII.Spaces;
    unget := TRUE)
    : CHAR
    RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted}=
  VAR ch: CHAR;
  BEGIN
    REPEAT
      ch := Rd.GetChar(s);
    UNTIL NOT(ch IN skip);
    IF unget THEN Rd.UnGetChar(s) END;
    RETURN ch;
  END RdExtras_Skip;

(*--------------------------------------------------------------------*)

PROCEDURE RdExtras_GetText(
    s: Rd.T;
    READONLY skip := ASCII.Set{};
    READONLY terminate := ASCII.Spaces;
    unget := TRUE)
    : TEXT
    RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted}=
  VAR chars: ARRAY [0..255] OF CHAR;
      result: TEXT := "";
      len: CARDINAL;
  BEGIN
    EVAL RdExtras_Skip(s, skip);
    REPEAT
      len := RdExtras_GetUntil(s, chars, terminate, unget);
      result := result & Text.FromChars(SUBARRAY(chars, 0,
                                                 MIN(len, NUMBER(chars))));
    UNTIL len <= NUMBER(chars);
    RETURN result;
  END RdExtras_GetText;

(*--------------------------------------------------------------------*)

PROCEDURE RdExtras_GetUntil(
    s: Rd.T;
    VAR chars: ARRAY OF CHAR;
    READONLY terminate := ASCII.Spaces;
    unget := TRUE)
    : CARDINAL
    RAISES {Rd.Failure, Thread.Alerted}=
  VAR ch: CHAR; i := 0;
  BEGIN
    LOOP
      TRY
        ch := Rd.GetChar(s);
        IF ch IN terminate THEN
          IF unget THEN Rd.UnGetChar(s) END;
          EXIT
        END;
        IF i = NUMBER(chars) THEN
          INC(i);
          EXIT
        ELSE chars[i] := ch; INC(i);
        END;
      EXCEPT Rd.EndOfFile => EXIT;
      END;
    END;
    RETURN i;
  END RdExtras_GetUntil;

(*--------------------------------------------------------------------*)

BEGIN (* empty module body *)
END TextReadingUtils.
