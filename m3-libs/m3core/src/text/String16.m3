(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

UNSAFE MODULE String16;

IMPORT Word;

TYPE Ptr = UNTRACED REF WIDECHAR;

PROCEDURE Equal (a, b: ADDRESS;  len: CARDINAL): BOOLEAN =
  VAR pa := LOOPHOLE (a, Ptr);  pb := LOOPHOLE (b, Ptr);
  BEGIN
    IF (len = 0) THEN RETURN TRUE; END;
    IF (a = NIL) OR (b = NIL) THEN RETURN FALSE; END;
    WHILE (len > 0) AND (pa^ = pb^) DO
      DEC (len);  INC (pa, ADRSIZE (pa^));  INC (pb, ADRSIZE (pb^));
    END;
    RETURN (len = 0);
  END Equal;

PROCEDURE Compare (a: ADDRESS;  len_a: CARDINAL;
                   b: ADDRESS;  len_b: CARDINAL): [-1..1] =
  CONST Map = ARRAY BOOLEAN OF [-1..+1] { +1, -1 };
  VAR
    pa  := LOOPHOLE (a, Ptr);
    pb  := LOOPHOLE (b, Ptr);
    len := MIN (len_a, len_b);
  BEGIN
    WHILE (len > 0) AND (pa^ = pb^) DO
      DEC (len);  INC (pa, ADRSIZE (pa^));  INC (pb, ADRSIZE (pb^));
    END;
    IF    (len # 0)       THEN RETURN Map [pa^ < pb^];
    ELSIF (len_a # len_b) THEN RETURN Map [len_a < len_b];
    ELSE                       RETURN 0;
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

PROCEDURE FindChar (a: ADDRESS;  len: CARDINAL;  c: WIDECHAR): INTEGER =
  VAR p := LOOPHOLE (a, Ptr);  cnt := len;
  BEGIN
    IF (p = NIL) THEN RETURN -1; END;
    WHILE (cnt > 0) DO
      IF (p^ = c) THEN RETURN len - cnt; END;
      INC (p, ADRSIZE (p^));  DEC (cnt);
    END;
    RETURN -1;
  END FindChar;

PROCEDURE FindCharR (a: ADDRESS;  len: CARDINAL;  c: WIDECHAR): INTEGER =
  VAR p := LOOPHOLE (a + len * ADRSIZE (WIDECHAR), Ptr);
  BEGIN
    IF (p = NIL) THEN RETURN -1; END;
    WHILE (len > 0) DO
      DEC (p, ADRSIZE (p^));  DEC (len);
      IF (p^ = c) THEN RETURN len; END;
    END;
    RETURN -1;
  END FindCharR;

PROCEDURE ArrayStart (READONLY a: ARRAY OF WIDECHAR): ADDRESS =
  BEGIN
    IF NUMBER (a) < 1 THEN RETURN NIL; END;
    RETURN ADR (a[0]);
  END ArrayStart;

BEGIN
END String16.
