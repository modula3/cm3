(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

(* "Text.Sub" is implemented in "TextSub". *)
(* "Text.Cat" is implemented in "TextCat". *)

UNSAFE MODULE Text;

IMPORT Word, TextClass, Text8, Text16, String8, String16;

TYPE Info = TextClass.Info;

PROCEDURE Length (t: T): CARDINAL =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    RETURN i.length;
  END Length;

PROCEDURE Empty (t: T): BOOLEAN =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    RETURN i.length < 1;
  END Empty;


PROCEDURE Equal (t, u: T): BOOLEAN =
  VAR info_t, info_u: Info;
  BEGIN
    t.get_info (info_t);
    u.get_info (info_u);
    IF (info_t.length # info_u.length) THEN RETURN FALSE; END;
    IF (info_t.length = 0)             THEN RETURN TRUE;  END;

    IF   (info_t.start = NIL)
      OR (info_u.start = NIL)
      OR (info_t.wide # info_u.wide) THEN
      RETURN EqualBuf (t, u, info_t.length);
    ELSIF NOT info_t.wide THEN
      RETURN String8.Equal (info_t.start, info_u.start, info_t.length);
    ELSE
      RETURN String16.Equal (info_t.start, info_u.start, info_t.length);
    END;
  END Equal;

PROCEDURE EqualBuf (t, u: TEXT;  len: CARDINAL): BOOLEAN =
  VAR
    i     : CARDINAL := 0;
    j     : INTEGER;
    buf_t : ARRAY [0..63] OF WIDECHAR;
    buf_u : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    j := len - NUMBER (buf_t);  (* last index that will fill a buffer *)
    WHILE (i <= j) DO
      t.get_wide_chars (buf_t, i);
      u.get_wide_chars (buf_u, i);
      IF (buf_t # buf_u) THEN RETURN FALSE; END;
      INC (i, NUMBER (buf_t));
    END;

    j := len - i;  (* # remaining characters *)
    IF (j <= 0) THEN RETURN TRUE; END;

    t.get_wide_chars (buf_t, i);
    u.get_wide_chars (buf_u, i);
    RETURN SUBARRAY (buf_t, 0, j) = SUBARRAY (buf_u, 0, j);
  END EqualBuf;


PROCEDURE Compare (t, u: T): [-1..1] =
  VAR info_t, info_u: Info;
  BEGIN
    t.get_info (info_t);
    u.get_info (info_u);

    IF   (info_t.start = NIL)
      OR (info_u.start = NIL)
      OR (info_t.wide # info_u.wide) THEN
      RETURN CompareBuf (t, u, info_t.length, info_u.length);

    ELSIF NOT info_t.wide THEN
      RETURN String8.Compare (info_t.start, info_t.length,
                              info_u.start, info_u.length);
    ELSE
      RETURN String16.Compare (info_t.start, info_t.length,
                               info_u.start, info_u.length);
    END;
  END Compare;

PROCEDURE CompareBuf (t, u: TEXT;  len_t, len_u: CARDINAL): [-1 .. 1] =
  CONST Map = ARRAY BOOLEAN OF [-1..1] { 1, -1 };
  VAR
    min_len : CARDINAL   := MIN (len_t, len_u);
    cur_len : CARDINAL   := 0;
    i       : CARDINAL   := 0;
    j       : [-1 .. +1] := 0;
    buf_t   : ARRAY [0..63] OF WIDECHAR;
    buf_u   : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    WHILE (i < min_len) DO
      t.get_wide_chars (buf_t, i);
      u.get_wide_chars (buf_u, i);
      cur_len := MIN (min_len - i, NUMBER (buf_t));
      j := String16.Compare (ADR (buf_t[0]), cur_len, ADR (buf_u[0]), cur_len);
      IF (j # 0) THEN RETURN j; END;
      INC (i, NUMBER (buf_t));
    END;

    IF (len_t = len_u) THEN RETURN 0; END;
    RETURN Map [len_t < len_u];
  END CompareBuf;

PROCEDURE Cat (t, u: TEXT): TEXT =
  BEGIN
    RETURN t & u;  (* => call RTHooks.Concat(t, u) *)
  END Cat;

PROCEDURE Hash (t: T): Word.T =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    IF (i.start = NIL) THEN  RETURN HashBuf (t, i.length);
    ELSIF NOT i.wide   THEN  RETURN String8.Hash (i.start, i.length, i.length);
    ELSE                     RETURN String16.Hash (i.start, i.length, i.length);
    END;
  END Hash;

PROCEDURE HashBuf (t: T;  len: CARDINAL): Word.T =
  VAR
    result : Word.T   := len;
    start  : CARDINAL := 0;
    buf    : ARRAY [0..127] OF WIDECHAR;
  BEGIN
    WHILE start < len DO
      t.get_wide_chars (buf, start);
      result := String16.Hash (ADR (buf[0]),
                  MIN (len - start, NUMBER (buf)), result);
      INC (start, NUMBER (buf));
    END;
    RETURN result;
  END HashBuf;


PROCEDURE HasWideChars (t: T): BOOLEAN =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    RETURN i.wide;
  END HasWideChars;



PROCEDURE GetChar (t: T; i: CARDINAL): CHAR =
  BEGIN
    RETURN t.get_char (i);
  END GetChar;

PROCEDURE GetWideChar (t: T; i: CARDINAL): WIDECHAR =
  BEGIN
    RETURN t.get_wide_char (i);
  END GetWideChar;



PROCEDURE SetChars (VAR a: ARRAY OF CHAR;  t: T;  start: CARDINAL) =
  BEGIN
    t.get_chars (a, start);
  END SetChars;

PROCEDURE SetWideChars (VAR a: ARRAY OF WIDECHAR;  t: T;  start: CARDINAL) =
  BEGIN
    t.get_wide_chars (a, start);
  END SetWideChars;



VAR fromCharCache := ARRAY CHAR OF T {NIL, ..}; (* 1-char texts *)

PROCEDURE FromChar (c: CHAR): T =
  VAR buf: ARRAY [0..0] OF CHAR;
  BEGIN
    IF fromCharCache [c] = NIL THEN
      buf [0] := c;
      fromCharCache[c] := Text8.New (buf);
    END;
    RETURN fromCharCache [c]
  END FromChar;

PROCEDURE FromWideChar (c: WIDECHAR): T =
  VAR buf: ARRAY [0..0] OF WIDECHAR;
  BEGIN
    IF ORD (c) <= ORD (LAST (CHAR))
      THEN RETURN FromChar (VAL (ORD (c), CHAR));
      ELSE buf[0] := c;  RETURN Text16.New (buf);
    END;
  END FromWideChar;



PROCEDURE FromChars (READONLY a: ARRAY OF CHAR): T =
  VAR n := NUMBER (a);
  BEGIN
    IF (n = 0) THEN RETURN "" END;
    IF (n = 1) THEN RETURN FromChar (a [0]) END;
    RETURN Text8.New (a);
  END FromChars;

PROCEDURE FromWideChars (READONLY a: ARRAY OF WIDECHAR): T =
  VAR n := NUMBER (a);
  BEGIN
    IF (n = 0) THEN RETURN "" END;
    IF (n = 1) THEN RETURN FromWideChar (a [0]) END;
    RETURN Text16.New (a);
  END FromWideChars;



PROCEDURE FindChar (t: T;  c: CHAR;  start := 0): INTEGER =
  VAR i: Info;  res: INTEGER;
  BEGIN
    t.get_info (i);
    IF (i.start = NIL) THEN
      res := FindCharBuf (t, VAL (ORD (c), WIDECHAR), start, i.length);
    ELSIF NOT i.wide THEN
      res := String8.FindChar (i.start + start * ADRSIZE (CHAR),
                               i.length - start, c);
      IF (res >= 0) THEN INC (res, start); END;
    ELSE
      res := String16.FindChar (i.start + start * ADRSIZE (WIDECHAR),
                                i.length - start, VAL (ORD (c), WIDECHAR));
      IF (res >= 0) THEN INC (res, start); END;
    END;
    RETURN res;
  END FindChar;

PROCEDURE FindWideChar (t: T;  c: WIDECHAR;  start := 0): INTEGER =
  VAR i: Info;  res: INTEGER;
  BEGIN
    t.get_info (i);
    IF (i.start = NIL) THEN
      res := FindCharBuf (t, c, start, i.length);
    ELSIF NOT i.wide THEN
      IF (ORD (c) > ORD (LAST (CHAR))) THEN RETURN -1; END;
      res := String8.FindChar (i.start + start * ADRSIZE (CHAR),
                               i.length - start,
                               VAL (Word.And (ORD (c), 16_ff), CHAR));
      IF (res >= 0) THEN INC (res, start); END;
    ELSE
      res := String16.FindChar (i.start + start * ADRSIZE (WIDECHAR),
                                i.length - start, c);
      IF (res >= 0) THEN INC (res, start); END;
    END;
    RETURN res;
  END FindWideChar;

PROCEDURE FindCharBuf (t: T;  c: WIDECHAR;
                       start: INTEGER;  len: CARDINAL): INTEGER =
  VAR
    i   : CARDINAL := MAX (0, start);
    j   : CARDINAL := NUMBER (buf);
    buf : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    LOOP
      IF (i >= len) THEN RETURN -1 END;
      IF (j >= NUMBER (buf)) THEN  j := 0;  t.get_wide_chars (buf, i);  END;
      IF (buf[j] = c) THEN RETURN i END;
      INC (i);  INC (j);
    END;
  END FindCharBuf;



PROCEDURE FindCharR (t: T;  c: CHAR;  start := LAST (INTEGER)): INTEGER =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    IF (start < 0) THEN
      RETURN -1;
    ELSIF (i.start = NIL) THEN
      RETURN FindCharRBuf (t, VAL (ORD (c), WIDECHAR), start, i.length);
    ELSIF NOT i.wide THEN
      RETURN String8.FindCharR (i.start, MIN (i.length, start), c);
    ELSE
      RETURN String16.FindCharR (i.start, MIN (i.length, start),
                                 VAL (ORD (c), WIDECHAR));
    END;
  END FindCharR;

PROCEDURE FindWideCharR (t: T;  c: WIDECHAR;  start := LAST (INTEGER)): INTEGER =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    IF (start < 0) THEN
      RETURN -1;
    ELSIF (i.start = NIL) THEN
      RETURN FindCharRBuf (t, c, start, i.length);
    ELSIF NOT i.wide THEN
      RETURN String8.FindCharR (i.start, MIN (i.length, start),
                                VAL (Word.And (ORD (c), 16_ff), CHAR));
    ELSE
      RETURN String16.FindCharR (i.start, MIN (i.length, start), c);
    END;
  END FindWideCharR;

PROCEDURE FindCharRBuf (t: TEXT;  c: WIDECHAR;
                        start: INTEGER;  len: CARDINAL): INTEGER =
  VAR
    i   : INTEGER  := MIN (len-1, start);
    j   : INTEGER  := -1;
    buf : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    LOOP
      IF (i < 0) THEN RETURN -1 END;
      IF (j < 0) THEN
        t.get_wide_chars (buf, MAX (0, i - LAST (buf)));
        j := MIN (i, LAST (buf));
      END;
      IF (buf[j] = c) THEN RETURN i END;
      DEC (i);  DEC (j);
    END;
  END FindCharRBuf;

BEGIN
END Text.


