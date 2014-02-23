(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE TextClass;

IMPORT Word;
(*47 IMPORT TextStats; 74*)  
(*47 FROM TextStats IMPORT Op;  74*)

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
    (*47 TextStats.NoteGround (Op.GetChar); 74*) 
    (*47 TextStats.NoteGround (Op.get_wide_char); 74*) 
    Wide := t.get_wide_char (i);
    (*47 TextStats.NoteFinished (Op.get_wide_char); 74*) 
    Result := VAL (Word.And (ORD (Wide), 16_ff), CHAR);
(*4*)
    (*47 TextStats.NoteFinished (Op.GetChar); 74*) 
    RETURN Result; 
  END GetChar;

PROCEDURE GetWideChar (t: TEXT;  i: CARDINAL): WIDECHAR =
  VAR Result : WIDECHAR;
  BEGIN
    (*47 TextStats.NoteGround (Op.GetWideChar); 74*) 
    (*47 TextStats.NoteGround (Op.get_char); 74*) 
    Result := VAL (ORD (t.get_char (i)), WIDECHAR);
    (*47 TextStats.NoteFinished (Op.get_char); 74*) 
    (*47 TextStats.NoteFinished (Op.GetWideChar); 74*) 
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
      (*47 TextStats.NoteGround (Op.get_wide_chars); 74*) 
      t.get_wide_chars (buf, start);
      (*47 TextStats.NoteFinished (Op.get_wide_chars); 74*) 
      FOR i := FIRST (buf) TO LAST (buf) DO
        IF (cnt = 0) THEN RETURN END;
        a[next] := VAL (Word.And (ORD (buf[i]), 16_ff), CHAR);
(*4*)
        INC (next);  DEC (cnt);
     (* IF i < LAST(buf) THEN (*47 TextStats.NoteIter (Op.get_chars); 74*) END; *) 
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
      (*47 TextStats.NoteGround (Op.get_chars); 74*) 
      t.get_chars (buf, start);
      (*47 TextStats.NoteFinished (Op.get_chars); 74*) 
      FOR i := FIRST (buf) TO LAST (buf) DO
        IF (cnt = 0) THEN RETURN END;
        a[next] := VAL (ORD (buf[i]), WIDECHAR);
        INC (next);  DEC (cnt);
     (* IF i < LAST(buf) THEN (*47 TextStats.NoteIter (Op.get_wide_chars); 74*) END; *) 
      END;
      INC (start, NUMBER (buf));
    END;
  END GetWideChars;

BEGIN
END TextClass.
