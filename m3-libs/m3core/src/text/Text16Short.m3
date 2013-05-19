(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE Text16Short;

IMPORT TextClass;

IMPORT TextStats;

REVEAL
  T = Public BRANDED "Text16Short.T" OBJECT OVERRIDES
    get_info       := T16SGetInfo;
    get_wide_char  := T16SGetChar;
    get_wide_chars := T16SGetChars;
  END;

PROCEDURE New (READONLY a: ARRAY OF WIDECHAR): T =
  VAR t := NEW (T);
  BEGIN
    t.len := NUMBER (a);
    IF (t.len > 0) THEN SUBARRAY (t.contents, 0, t.len) := a; END;
    t.contents[t.len] := VAL(0,WIDECHAR);
    TextStats.NoteAllocText16Short(t);
    RETURN t;
  END New;

PROCEDURE T16SGetInfo (t: T;  VAR info: TextClass.Info) =
  BEGIN
    info.start  := ADR (t.contents[0]);
    info.length := t.len;
    info.wide   := TRUE;
  END T16SGetInfo;

PROCEDURE T16SGetChar (t: T;  i: CARDINAL): WIDECHAR =
  BEGIN
    IF i >= t.len THEN (* force a subscript fault *) i := LAST (INTEGER); END;
    RETURN t.contents[i];
  END T16SGetChar;

PROCEDURE T16SGetChars (t: T;  VAR a: ARRAY OF WIDECHAR;  start: CARDINAL) =
  VAR n := MIN (NUMBER (a), t.len - start);
  BEGIN
    IF (n > 0) THEN
      SUBARRAY (a, 0, n) := SUBARRAY (t.contents, start, n);
    END;
  END T16SGetChars;

BEGIN
END Text16Short.
