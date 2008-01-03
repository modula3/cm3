(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE String8;

IMPORT Cstring, Word;

TYPE Ptr = UNTRACED REF CHAR;

PROCEDURE Equal (a, b: ADDRESS;  len: CARDINAL): BOOLEAN =
  BEGIN
    IF (len = 0) THEN RETURN TRUE; END;
    IF (a = NIL) OR (b = NIL) THEN RETURN FALSE; END;
    RETURN Cstring.memcmp (a, b, len) = 0;
  END Equal;

PROCEDURE Compare (a: ADDRESS;  len_a: CARDINAL;
                   b: ADDRESS;  len_b: CARDINAL): [-1..1] =
  VAR i: INTEGER;
  BEGIN
    i := Cstring.memcmp (a, b, MIN (len_a, len_b));
    IF    (i < 0)         THEN  RETURN -1;
    ELSIF (i > 0)         THEN  RETURN +1;
    ELSIF (len_a < len_b) THEN  RETURN -1;
    ELSIF (len_a > len_b) THEN  RETURN +1;
    ELSE                        RETURN 0;
    END;
  END Compare;

PROCEDURE Hash (a: ADDRESS;  len: CARDINAL;  initial: INTEGER): INTEGER =
  VAR p := LOOPHOLE (a, Ptr);  result := initial;
  BEGIN
    IF (p = NIL) THEN RETURN result; END;
    WHILE (len > 0) DO
      result := Word.Xor (Word.LeftRotate (result, 13), ORD (p^));
      INC (p, ADRSIZE (p^));  DEC (len);
    END;
    RETURN result;
  END Hash;

PROCEDURE FindChar (a: ADDRESS;  len: CARDINAL;  c: CHAR): INTEGER =
  VAR p := LOOPHOLE (a, Ptr);  cnt := len;
  BEGIN
    IF (p = NIL) THEN RETURN -1; END;
    WHILE (cnt > 0) DO
      IF (p^ = c) THEN RETURN len - cnt; END;
      INC (p, ADRSIZE (p^));  DEC (cnt);
    END;
    RETURN -1;
  END FindChar;

PROCEDURE FindCharR (a: ADDRESS;  len: CARDINAL;  c: CHAR): INTEGER =
  VAR p := LOOPHOLE (a + len * ADRSIZE (CHAR), Ptr);
  BEGIN
    IF (p = NIL) THEN RETURN -1; END;
    WHILE (len > 0) DO
      DEC (p, ADRSIZE (p^));  DEC (len);
      IF (p^ = c) THEN RETURN len; END;
    END;
    RETURN -1;
  END FindCharR;

PROCEDURE ArrayStart (READONLY a: ARRAY OF CHAR): ADDRESS =
  BEGIN
    IF NUMBER (a) < 1 THEN RETURN NIL; END;
    RETURN ADR (a[0]);
  END ArrayStart;

BEGIN
END String8.
