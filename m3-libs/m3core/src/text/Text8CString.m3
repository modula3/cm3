(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

UNSAFE MODULE Text8CString;

IMPORT Ctypes, Cstring, TextClass;

REVEAL
  T = Public BRANDED "Text8CString.T" OBJECT OVERRIDES
    get_info  := GetInfo;
    get_char  := GetChar;
    get_chars := GetChars;
  END;

PROCEDURE New (s: Ctypes.char_star): TEXT =
  BEGIN
    RETURN NEW (T, str := s);
  END New;

PROCEDURE GetInfo (t: T;  VAR info: TextClass.Info) =
  BEGIN
    info.start  := t.str;
    info.length := Cstring.strlen (t.str);
    info.wide   := FALSE;
  END GetInfo;

PROCEDURE GetChar (t: T;  i: CARDINAL): CHAR =
  VAR len := Cstring.strlen (t.str);
  BEGIN
    IF i >= len THEN (* force a subscript fault *) i := -1; <*NOWARN*> END;
    RETURN LOOPHOLE (t.str + i * ADRSIZE (CHAR), UNTRACED REF CHAR)^;
  END GetChar;

PROCEDURE GetChars (t: T;  VAR a: ARRAY OF CHAR;  start: CARDINAL) =
  VAR
    len := Cstring.strlen (t.str);
    n   := MIN (NUMBER (a), len - start);
  BEGIN
    IF (n > 0) THEN
      EVAL Cstring.memcpy (ADR (a[0]), t.str + start * ADRSIZE (CHAR),
                           n * BYTESIZE (CHAR));
    END;
  END GetChars;

BEGIN
END Text8CString.
