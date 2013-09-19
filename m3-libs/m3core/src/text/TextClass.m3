(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE TextClass;

IMPORT Word;
IMPORT TextStats;  
FROM TextStats IMPORT Op; 

(* This is tricky and a bit dangerous.  If somebody were to allocate
   an object of type TEXT=Text.T, there would be infinite mutual recursion
   between GetChar and GetWideChar, and between GetChars and GetWideChars.
   This relys on TEXT being abstract, i.e. never allocated, only subtyped,
   and on the fact that every non-abstract subtype of Text.T overrides
   at least one of each pair above with something that does not call the
   other. *) 

PROCEDURE GetChar (t: TEXT;  i: CARDINAL): CHAR =
  VAR Result: CHAR;
  VAR Wide: WIDECHAR;
  BEGIN
    TextStats.NoteGround (Op.GetChar); 
    TextStats.NoteGround (Op.get_wide_char); 
    Wide := t.get_wide_char (i);
    TextStats.NoteFinished (Op.get_wide_char); 
    Result := VAL (Word.And (ORD (Wide), 16_ff), CHAR);
(*4*)
    TextStats.NoteFinished (Op.GetChar); 
    RETURN Result; 
  END GetChar;

PROCEDURE GetWideChar (t: TEXT;  i: CARDINAL): WIDECHAR =
  VAR Result : WIDECHAR;
  BEGIN
    TextStats.NoteGround (Op.GetWideChar); 
    TextStats.NoteGround (Op.get_char); 
    Result := VAL (ORD (t.get_char (i)), WIDECHAR);
    TextStats.NoteFinished (Op.get_char); 
    TextStats.NoteFinished (Op.GetWideChar); 
    RETURN Result
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
      TextStats.NoteGround (Op.get_wide_chars); 
      t.get_wide_chars (buf, start);
      TextStats.NoteFinished (Op.get_wide_chars); 
      FOR i := FIRST (buf) TO LAST (buf) DO
        IF (cnt = 0) THEN RETURN END;
        a[next] := VAL (Word.And (ORD (buf[i]), 16_ff), CHAR);
(*4*)
        INC (next);  DEC (cnt);
     (* IF i < LAST(buf) THEN TextStats.NoteIter (Op.get_chars) END *) 
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
      TextStats.NoteGround (Op.get_chars); 
      t.get_chars (buf, start);
      TextStats.NoteFinished (Op.get_chars); 
      FOR i := FIRST (buf) TO LAST (buf) DO
        IF (cnt = 0) THEN RETURN END;
        a[next] := VAL (ORD (buf[i]), WIDECHAR);
        INC (next);  DEC (cnt);
     (* IF i < LAST(buf) THEN TextStats.NoteIter (Op.get_wide_chars) END *) 
      END;
      INC (start, NUMBER (buf));
    END;
  END GetWideChars;

BEGIN
END TextClass.
