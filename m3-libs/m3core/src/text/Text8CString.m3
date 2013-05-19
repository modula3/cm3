(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE Text8CString;

IMPORT Ctypes, Cstring, TextClass;

REVEAL
  T = Public BRANDED "Text8CString.T" OBJECT OVERRIDES
    get_info  := T8CGetInfo;
    get_char  := T8CGetChar;
    get_chars := T8CGetChars;
  END;

PROCEDURE New (s: Ctypes.char_star): TEXT =
  BEGIN
    RETURN NEW (T, str := s);
  END New;

PROCEDURE T8CGetInfo (t: T;  VAR info: TextClass.Info) =
  BEGIN
    info.start  := t.str;
    info.length := Cstring.strlen (t.str);
    info.wide   := FALSE;
  END T8CGetInfo;

PROCEDURE T8CGetChar (t: T;  i: CARDINAL): CHAR =
  VAR len := Cstring.strlen (t.str);
  BEGIN
    IF i >= len THEN (* force a subscript fault *) i := -1; <*NOWARN*> END;
    RETURN LOOPHOLE (t.str + i * ADRSIZE (CHAR), UNTRACED REF CHAR)^;
  END T8CGetChar;

PROCEDURE T8CGetChars (t: T;  VAR a: ARRAY OF CHAR;  start: CARDINAL) =
  VAR
    len := Cstring.strlen (t.str);
    n   := MIN (NUMBER (a), len - start);
  BEGIN
    IF (n > 0) THEN
      EVAL Cstring.memcpy (ADR (a[0]), t.str + start * ADRSIZE (CHAR),
                           n * BYTESIZE (CHAR));
    END;
  END T8CGetChars;

BEGIN
END Text8CString.
