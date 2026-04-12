(* Copyright (C) 1989, 1992, Digital Equipment Corporation                   *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Feb 18 13:16:50 PST 1994 by kalsow                   *)
(*      modified on Wed Mar  4 11:58:26 PST 1992 by muller                   *)


UNSAFE MODULE OldScan;

IMPORT Text, Word, Convert;

PROCEDURE Bool (t: Text.T): BOOLEAN RAISES {BadFormat} =
BEGIN
  IF Text.Equal (t, "TRUE") THEN
    RETURN TRUE;
  ELSIF Text.Equal (t, "FALSE") THEN
    RETURN FALSE;
  ELSE
    RAISE BadFormat; END;
END Bool;

PROCEDURE Int (t: Text.T): INTEGER RAISES {BadFormat} =
VAR
      x : UNTRACED REF ARRAY OF CHAR;
  value : INTEGER;
   used : INTEGER;

BEGIN
  x := NEW (UNTRACED REF ARRAY OF CHAR, Text.Length(t));
  Text.SetChars (x^, t);
  value := Convert.ToInt (x^, used);
  IF used = 0 THEN
    DISPOSE (x);
    RAISE BadFormat;
  ELSE
    DISPOSE (x);
    RETURN value; END;
END Int;

PROCEDURE Unsigned (t: Text.T): Word.T RAISES {BadFormat} =
VAR
      x : UNTRACED REF ARRAY OF CHAR;
  value : Word.T;
   used : INTEGER;

BEGIN
  x := NEW (UNTRACED REF ARRAY OF CHAR, Text.Length(t));
  Text.SetChars (x^, t);
  value := Convert.ToUnsigned (x^, used);
  IF used = 0 THEN
    DISPOSE (x);
    RAISE BadFormat;
  ELSE
    DISPOSE (x);
    RETURN value; END;
END Unsigned;

PROCEDURE Real (t: Text.T): REAL RAISES {BadFormat} =
<*FATAL Convert.Failed*>
VAR
      x : UNTRACED REF ARRAY OF CHAR;
  value : REAL;
   used : INTEGER;

BEGIN
  x := NEW (UNTRACED REF ARRAY OF CHAR, Text.Length(t));
  Text.SetChars (x^, t);
  value := Convert.ToFloat (x^, used);
  IF used = 0 THEN
    DISPOSE (x);
    RAISE BadFormat;
  ELSE
    DISPOSE (x);
    RETURN value; END;
END Real;


PROCEDURE LongReal (t: Text.T): LONGREAL RAISES {BadFormat} =
<*FATAL Convert.Failed*>
VAR
      x : UNTRACED REF ARRAY OF CHAR;
  value : LONGREAL;
   used : INTEGER;

BEGIN
  x := NEW (UNTRACED REF ARRAY OF CHAR, Text.Length(t));
  Text.SetChars (x^, t);
  value := Convert.ToLongFloat (x^, used);
  IF used = 0 THEN
    DISPOSE (x);
    RAISE BadFormat;
  ELSE
    DISPOSE (x);
    RETURN value; END;
END LongReal;


PROCEDURE Char (t: Text.T): CHAR RAISES {BadFormat} =
BEGIN
  IF Text.Length (t) = 0 THEN
    RAISE BadFormat;
  ELSE
    RETURN Text.GetChar (t, 0); END;
END Char;

BEGIN
END OldScan.
