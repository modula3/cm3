(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE RdExtras;

IMPORT Rd, Thread, ASCII, Text;

PROCEDURE Skip(
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
  END Skip;

PROCEDURE GetUntil(
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
  END GetUntil;

PROCEDURE GetChars(
    s: Rd.T;
    VAR chars: ARRAY OF CHAR;
    READONLY skip := ASCII.Spaces;
    READONLY terminate := ASCII.Spaces;
    unget := TRUE)
    : CARDINAL
    RAISES {Rd.Failure, Rd.EndOfFile, Thread.Alerted}=
  BEGIN
    EVAL Skip(s, skip);
    RETURN GetUntil(s, chars, terminate, unget);
  END GetChars;

PROCEDURE GetText(
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
    EVAL Skip(s, skip);
    REPEAT
      len := GetUntil(s, chars, terminate, unget);
      result := result & Text.FromChars(SUBARRAY(chars, 0,
                                                 MIN(len, NUMBER(chars))));
    UNTIL len <= NUMBER(chars);
    RETURN result;
  END GetText;

BEGIN    
END RdExtras.
