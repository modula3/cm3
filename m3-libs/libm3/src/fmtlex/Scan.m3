(* Copyright (C) 1990, Digital Equipment Corporation.		*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)
(*								*)
(* Last modified on Fri Feb 25 15:10:55 PST 1994 by kalsow      *)
(*      modified on Thu Feb 24 12:07:10 PST 1994 by heydon      *)

(* NOTE: This is a quick and dirty implementation.  Please
    rewrite me to avoid all the allocations.  *)

MODULE Scan;

IMPORT Rd, Thread, FloatMode, Lex, Text, TextRd, Word, Long;
<*FATAL Rd.Failure, Thread.Alerted*>

PROCEDURE Skip (txt: TEXT;  len, start: INTEGER;  blanks: BOOLEAN): INTEGER =
  (* Return the index of the first character of "txt" at or beyond "start"
     that's not in "chars". *)
  VAR
    i   : CARDINAL := NUMBER(buf);
    buf : ARRAY [0..63] OF CHAR;
  BEGIN
    LOOP
      IF (start >= len) THEN RETURN len; END;
      IF (i >= NUMBER (buf)) THEN i := 0; Text.SetChars (buf, txt, start);  END;
      IF (buf[i] IN Lex.Blanks) # blanks THEN RETURN start; END;
      INC (start);  INC (i);
    END;
  END Skip;

PROCEDURE ScanWord (txt: TEXT): Rd.T RAISES {Lex.Error} =
  (* Ensure that "txt" contains exactly one non-blank substring,
     and return its span [start..stop) *)
  VAR
    len    := Text.Length (txt);
    start  := Skip (txt, len, 0,     blanks := TRUE);
    stop   := Skip (txt, len, start, blanks := FALSE);
    finish := Skip (txt, len, stop,  blanks := TRUE);
  BEGIN
    IF finish < len THEN RAISE Lex.Error; END;
    RETURN TextRd.New (Text.Sub (txt, start, stop-start));
  END ScanWord;

PROCEDURE Bool(txt: TEXT): BOOLEAN RAISES {Lex.Error} =
  VAR rd := ScanWord(txt); res := Lex.Bool(rd);
  BEGIN
    IF NOT Rd.EOF(rd) THEN RAISE Lex.Error END;
    RETURN res
  END Bool;

PROCEDURE Int(txt: TEXT; defaultBase: [2..16]): INTEGER 
    RAISES {Lex.Error, FloatMode.Trap} =
  VAR rd := ScanWord(txt); res := Lex.Int(rd, defaultBase);
  BEGIN
    IF NOT Rd.EOF(rd) THEN RAISE Lex.Error END;
    RETURN res
  END Int;

PROCEDURE Unsigned(txt: TEXT; defaultBase: [2..16]): Word.T
    RAISES {Lex.Error, FloatMode.Trap} =
  VAR rd := ScanWord(txt); res := Lex.Unsigned(rd, defaultBase);
  BEGIN
    IF NOT Rd.EOF(rd) THEN RAISE Lex.Error END;
    RETURN res
  END Unsigned;

PROCEDURE LongInt(txt: TEXT; defaultBase: [2..16]): LONGINT
    RAISES {Lex.Error, FloatMode.Trap} =
  VAR rd := ScanWord(txt); res := Lex.LongInt(rd, defaultBase);
  BEGIN
    IF NOT Rd.EOF(rd) THEN RAISE Lex.Error END;
    RETURN res
  END LongInt;

PROCEDURE LongUnsigned(txt: TEXT; defaultBase: [2..16]): Long.T
    RAISES {Lex.Error, FloatMode.Trap} =
  VAR rd := ScanWord(txt); res := Lex.LongUnsigned(rd, defaultBase);
  BEGIN
    IF NOT Rd.EOF(rd) THEN RAISE Lex.Error END;
    RETURN res
  END LongUnsigned;

PROCEDURE Real(txt: TEXT): REAL
  RAISES {Lex.Error, FloatMode.Trap} =
  VAR rd := ScanWord(txt); res := Lex.Real(rd);
  BEGIN
    IF NOT Rd.EOF(rd) THEN RAISE Lex.Error END;
    RETURN res
  END Real;

PROCEDURE LongReal(txt: TEXT): LONGREAL
  RAISES {Lex.Error, FloatMode.Trap} =
  VAR rd := ScanWord(txt); res := Lex.LongReal(rd);
  BEGIN
    IF NOT Rd.EOF(rd) THEN RAISE Lex.Error END;
    RETURN res
  END LongReal;

PROCEDURE Extended(txt: TEXT): EXTENDED
  RAISES {Lex.Error, FloatMode.Trap} =
  VAR rd := ScanWord(txt); res := Lex.Extended(rd);
  BEGIN
    IF NOT Rd.EOF(rd) THEN RAISE Lex.Error END;
    RETURN res
  END Extended;

BEGIN
END Scan.
