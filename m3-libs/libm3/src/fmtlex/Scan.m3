(* Copyright (C) 1990, Digital Equipment Corporation.		*)
(* All rights reserved.						*)
(* See the file COPYRIGHT for a full description.		*)
(*								*)
(* Last modified on Fri Feb 25 15:10:55 PST 1994 by kalsow      *)
(*      modified on Thu Feb 24 12:07:10 PST 1994 by heydon      *)

(* NOTE: This is a quick and dirty implementation.  Please
    rewrite me to avoid all the allocations.  *)

MODULE Scan;

IMPORT Rd, Thread, FloatMode, Lex, Text, TextRd, TextF, Word;
<*FATAL Rd.Failure, Thread.Alerted*>

PROCEDURE ScanWord (txt: TEXT): Rd.T RAISES {Lex.Error} =
  (* Ensure that "txt" contains exactly on non-blank substring,
     and return its span [start..stop) *)
  VAR len := LAST (txt^);  x := 0;  start, stop: INTEGER;
  BEGIN
    (* skip leading white space *)
    WHILE (x < len) AND (txt[x] IN Lex.Blanks) DO INC (x); END;
    start := x;

    (* skip the non-blank word *)
    WHILE (x < len) AND (NOT txt[x] IN Lex.Blanks) DO INC (x); END;
    stop := x;

    (* verify that the rest of the string is blank *)
    WHILE (x < len) DO
      IF (NOT txt[x] IN Lex.Blanks) THEN RAISE Lex.Error; END;
      INC (x);
    END;

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
