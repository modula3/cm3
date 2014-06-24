(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* NOTE on naming.  This source file was named when WIDECHAR was always 16-bits.
   Today, it can be that, or 32 bits, with upper bound of 16_10FFFF, for full
   Unicode range.  It's tedious to change many source file names in CVS, so
   the "16" remains. *)  

UNSAFE MODULE Text16;

IMPORT TextClass, Text16Short;

(*47 IMPORT TextStats;  74*)

REVEAL
  T = Public BRANDED "Text16.T" OBJECT OVERRIDES
    get_info       := T16GetInfo;
    get_wide_char  := T16GetWideChar;
    get_wide_chars := T16GetWideChars;
  END;

PROCEDURE New (READONLY a: ARRAY OF WIDECHAR): TEXT =
  VAR n := NUMBER (a);  t: T;
  BEGIN
    IF n <= Text16Short.MaxLength THEN RETURN Text16Short.New (a); END;
    t := Create (n);
    IF (n > 0) THEN SUBARRAY (t.contents^, 0, n) := a; END;
    RETURN t;
  END New;

PROCEDURE Create (n: CARDINAL): T =
  VAR t := NEW (T);
  BEGIN
    t.contents := NEW (REF ARRAY OF WIDECHAR, n + 1);
    t.contents[n] := VAL(0,WIDECHAR);
    (*47 TextStats.NoteAllocText16(t); 74*)
    (*47 TextStats.NoteAllocText16Chars(t.contents); 74*)
    RETURN t
  END Create;

PROCEDURE T16GetInfo (t: T;  VAR info: TextClass.Info) =
  BEGIN
    info.start  := ADR (t.contents[0]);
    info.length := MAX (0, LAST (t.contents^));
    info.wide   := TRUE;
  END T16GetInfo;

PROCEDURE T16GetWideChar (t: T;  i: CARDINAL): WIDECHAR =
  BEGIN
    IF i >= LAST (t.contents^) THEN (* force a subscript fault *) INC (i) END;
    RETURN t.contents[i];
  END T16GetWideChar;

PROCEDURE T16GetWideChars (t: T;  VAR a: ARRAY OF WIDECHAR;  start: CARDINAL) =
  VAR n := MIN (NUMBER (a), LAST (t.contents^) - start);
  BEGIN
    IF (n > 0) THEN
      SUBARRAY (a, 0, n) := SUBARRAY (t.contents^, start, n);
    END;
  END T16GetWideChars;

BEGIN
END Text16.
