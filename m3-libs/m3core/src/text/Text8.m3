(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE Text8;

IMPORT TextClass, Text8Short;

(*47 IMPORT TextStats; 74*)

REVEAL
  T = Public BRANDED "Text8.T" OBJECT OVERRIDES
    get_info  := T8GetInfo;
    get_char  := T8GetChar;
    get_chars := T8GetChars;
  END;

PROCEDURE New (READONLY a: ARRAY OF CHAR): TEXT =
  VAR n := NUMBER (a);  t: T;
  BEGIN
    IF n <= Text8Short.MaxLength THEN RETURN Text8Short.New (a); END;
    t := Create (n);
    IF (n > 0) THEN SUBARRAY (t.contents^, 0, n) := a; END;
    RETURN t;
  END New;

PROCEDURE Create (n: CARDINAL): T =
  VAR t := NEW (T);
  BEGIN
    t.contents := NEW (REF ARRAY OF CHAR, n + 1);
    t.contents[n] := '\000';
    (*47 TextStats.NoteAllocText8(t); 74*)
    (*47 TextStats.NoteAllocText8Chars(t.contents); 74*)
    RETURN t;
  END Create;

PROCEDURE T8GetInfo (t: T;  VAR info: TextClass.Info) =
  BEGIN
    info.start  := ADR (t.contents[0]);
    info.length := MAX (0, LAST (t.contents^));
    info.wide   := FALSE;
  END T8GetInfo;

PROCEDURE T8GetChar (t: T;  i: CARDINAL): CHAR =
  BEGIN
    IF i = LAST (t.contents^) THEN (* force a subscript fault *) INC (i) END;
    RETURN t.contents[i];
  END T8GetChar;

PROCEDURE T8GetChars (t: T;  VAR a: ARRAY OF CHAR;  start: CARDINAL) =
  VAR n := MIN (NUMBER (a), LAST (t.contents^) - start);
  BEGIN
    IF (n > 0) THEN
      SUBARRAY (a, 0, n) := SUBARRAY (t.contents^, start, n);
    END;
  END T8GetChars;

BEGIN
END Text8.
