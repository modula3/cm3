(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

MODULE TextClass;

IMPORT Word;

PROCEDURE GetChar (t: TEXT;  i: CARDINAL): CHAR =
  BEGIN
    RETURN VAL (Word.And (ORD (t.get_wide_char (i)), 16_ff), CHAR);
  END GetChar;

PROCEDURE GetWideChar (t: TEXT;  i: CARDINAL): WIDECHAR =
  BEGIN
    RETURN VAL (ORD (t.get_char (i)), WIDECHAR);
  END GetWideChar;

PROCEDURE GetChars (t: TEXT;  VAR a: ARRAY OF CHAR;  start: CARDINAL) =
  VAR
    info : Info;
    cnt  : INTEGER;
    next : CARDINAL := 0;
    buf  : ARRAY [0..127] OF WIDECHAR;
  BEGIN
    t.get_info (info);
    cnt := MIN (NUMBER (a), info.length - start);
    WHILE (cnt > 0) DO
      t.get_wide_chars (buf, start);
      FOR i := FIRST (buf) TO LAST (buf) DO
        IF (cnt = 0) THEN RETURN END;
        a[next] := VAL (Word.And (ORD (buf[i]), 16_ff), CHAR);
        INC (next);  DEC (cnt);
      END;
      INC (start, NUMBER (buf));
    END;
  END GetChars;

PROCEDURE GetWideChars (t: TEXT;  VAR a: ARRAY OF WIDECHAR;  start: CARDINAL) =
  VAR
    info : Info;
    cnt  : INTEGER;
    next : CARDINAL := 0;
    buf  : ARRAY [0..127] OF CHAR;
  BEGIN
    t.get_info (info);
    cnt := MIN (NUMBER (a), info.length - start);
    WHILE (cnt > 0) DO
      t.get_chars (buf, start);
      FOR i := FIRST (buf) TO LAST (buf) DO
        IF (cnt = 0) THEN RETURN END;
        a[next] := VAL (ORD (buf[i]), WIDECHAR);
        INC (next);  DEC (cnt);
      END;
      INC (start, NUMBER (buf));
    END;
  END GetWideChars;

BEGIN
END TextClass.
