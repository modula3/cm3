(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: BracedCode.m3,v 1.1.1.1 2001-09-19 11:40:10 wagner Exp $ *)

MODULE BracedCode;
IMPORT CharCodes;
IMPORT CharRange;
IMPORT Rd, Thread;
IMPORT FileRdErr;
<* FATAL Rd.Failure, Thread.Alerted *>

PROCEDURE FindChar(rd: Rd.T; which: CharRange.T) RAISES {Rd.EndOfFile} =
  VAR
    inQuote: BOOLEAN := FALSE;
    braceLevel, commentLevel: INTEGER := 0;
    ch: CHAR;
  PROCEDURE CheckComment(char1, char2: CHAR; levelDelta: INTEGER)
    RAISES {Rd.EndOfFile} =
    VAR
      c: CHAR;
    BEGIN
      IF NOT inQuote THEN
        REPEAT
          c := Rd.GetChar(rd);
        UNTIL c # char1;
        IF c = char2 THEN
          commentLevel := MAX(commentLevel + levelDelta, 0);
        ELSE
          Rd.UnGetChar(rd);
        END;
      END;
    END CheckComment;
  BEGIN
    REPEAT
      ch := Rd.GetChar(rd);
      IF NOT inQuote AND braceLevel = 0 AND
        commentLevel = 0 AND ch IN which THEN
        braceLevel := -1;
      ELSE
        CASE ch OF
        | '\134' => EVAL Rd.GetChar(rd); (* \\ *)
        | '\042' => IF commentLevel=0 THEN inQuote := NOT inQuote; END;
        | '\047' => EVAL CharCodes.GetChar(rd); EVAL Rd.GetChar(rd); (* 'c' *)
        | '(' => CheckComment('(', '*', 1);
        | '*' => CheckComment('*', ')', -1);
        | '{' => IF NOT inQuote AND commentLevel = 0 THEN INC(braceLevel); END;
        | '}' => IF NOT inQuote AND commentLevel = 0 THEN DEC(braceLevel); END;
        ELSE
        END;
      END;
    UNTIL braceLevel < 0;
  END FindChar;

PROCEDURE Match(rd: Rd.T): TEXT =
  VAR
    oldPos, len: INTEGER;
    result: TEXT;
  BEGIN
    TRY
      oldPos := Rd.Index(rd);
      FindChar(rd, CharRange.T{'}'});
      len := Rd.Index(rd) - oldPos - 1;
      Rd.Seek(rd, oldPos);
      result := Rd.GetText(rd, len);
      EVAL Rd.GetChar(rd);
      RETURN result;
    EXCEPT
      Rd.EndOfFile =>
      Rd.Seek(rd, oldPos);
      FileRdErr.E(rd, "can't find closing '}'");
      RETURN NIL;
    END;
  END Match;

PROCEDURE GetAhead(rd: Rd.T): TEXT =
  VAR
    c: CHAR;
    oldPos := Rd.Index(rd);
  BEGIN
    TRY
      REPEAT
        c := Rd.GetChar(rd);
      UNTIL NOT c IN CharRange.WhiteSpace;
      IF c = '{' THEN
        RETURN Match(rd);
      END;
    EXCEPT
      Rd.EndOfFile =>
    END;
    Rd.Seek(rd, oldPos);
    RETURN "";
  END GetAhead;

BEGIN
END BracedCode.
