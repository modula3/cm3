(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE Text8Short;

IMPORT TextClass;

IMPORT TextStats;

REVEAL
  T = Public BRANDED "Text8Short.T" OBJECT OVERRIDES
    get_info  := T8SGetInfo;
    get_char  := T8SGetChar;
    get_chars := T8SGetChars;
  END;

PROCEDURE New (READONLY a: ARRAY OF CHAR): T =
  VAR t := NEW (T);
  BEGIN
    t.len := NUMBER (a);
    IF (t.len > 0) THEN SUBARRAY (t.contents, 0, t.len) := a; END;
    t.contents[t.len] := '\000';
    TextStats.NoteAllocText8Short(t);
    RETURN t;
  END New;

PROCEDURE T8SGetInfo (t: T;  VAR info: TextClass.Info) =
  BEGIN
    info.start  := ADR (t.contents[0]);
    info.length := t.len;
    info.wide   := FALSE;
  END T8SGetInfo;

PROCEDURE T8SGetChar (t: T;  i: CARDINAL): CHAR =
  BEGIN
    IF i >= t.len THEN (* force a subscript fault *) i := LAST (INTEGER); END;
    RETURN t.contents[i];
  END T8SGetChar;

PROCEDURE T8SGetChars (t: T;  VAR a: ARRAY OF CHAR;  start: CARDINAL) =
  VAR n := MIN (NUMBER (a), t.len - start);
  BEGIN
    IF (n > 0) THEN
      SUBARRAY (a, 0, n) := SUBARRAY (t.contents, start, n);
    END;
  END T8SGetChars;

BEGIN
END Text8Short.
