(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

(* "Text.Sub" is implemented in "TextSub". *)
(* (Most of)"Text.Cat" is implemented in "TextCat". *)

UNSAFE MODULE Text;

IMPORT Word, TextClass, TextStats, Text8, Text16, String8, String16;

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
  VAR Result : BOOLEAN; 
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.Equal); 74*) 
    IF TextClass.Old 
    THEN Result := OldEqual (t, u);
    ELSE Result := NewEqual (t, u);
    END;
    (*47 TextStats.NoteFinished (TextStats.Op.Equal); 74*) 
    RETURN Result
  END Equal;

PROCEDURE OldEqual (t, u: T): BOOLEAN =
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
  END OldEqual;

PROCEDURE EqualBuf (t, u: TEXT;  len: CARDINAL): BOOLEAN =
  VAR
    i     : CARDINAL := 0;
    j     : INTEGER;
    buf_t : ARRAY [0..63] OF WIDECHAR;
    buf_u : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    j := len - NUMBER (buf_t);  (* last index that will fill a buffer *)
    WHILE (i <= j) DO
      (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
      t.get_wide_chars (buf_t, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
      u.get_wide_chars (buf_u, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      IF (buf_t # buf_u) THEN RETURN FALSE; END;
      INC (i, NUMBER (buf_t));
      (*47 TextStats.NoteIter (TextStats.Op.Equal); 74*) 
    END;

    j := len - i;  (* # remaining characters *)
    IF (j <= 0) THEN RETURN TRUE; END;

    (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
    t.get_wide_chars (buf_t, i);
    (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
    (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
    u.get_wide_chars (buf_u, i);
    (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
    RETURN SUBARRAY (buf_t, 0, j) = SUBARRAY (buf_u, 0, j);
  END EqualBuf;

PROCEDURE NewEqual (t, u: T): BOOLEAN =
  VAR info_t, info_u: Info;
  BEGIN
    t.get_info (info_t);
    u.get_info (info_u);
    IF (info_t.length # info_u.length) THEN RETURN FALSE; END;
    IF (info_t.length = 0)             THEN RETURN TRUE;  END;

    IF info_t.wide 
    THEN 
      IF NOT info_u.wide OR info_t.start = NIL OR info_u.start = NIL 
      THEN RETURN EqualBufAny (t, u, info_t.length)
      ELSE (* info_t.wide AND info_u.wide 
              AND info_t.start # NIL AND info_u.start # NIL *) 
        RETURN String16.Equal (info_t.start, info_u.start, info_t.length)
      END (* IF *) 
    ELSE 
      IF info_u.wide  
      THEN RETURN EqualBufAny (t, u, info_t.length) 
      ELSIF info_t.start = NIL OR info_u.start = NIL
      THEN RETURN EqualBuf8 (t, u, info_t.length) 
      ELSE (* NOT info_t.wide AND NOT info_u.wide 
              AND info_t.start # NIL AND info_u.start # NIL *) 
        RETURN String8.Equal (info_t.start, info_u.start, info_t.length)
      END (* IF *) 
    END (* IF *) 
  END NewEqual;

PROCEDURE EqualBuf8 (t, u: TEXT;  len: CARDINAL): BOOLEAN =
  (* PRE: len = Length(t) = Length(u). *) 
  (* PRE: Neither t nor u contains any wide chars. *) 
  VAR
    i     : CARDINAL := 0;
    j     : INTEGER;
    buf_t : ARRAY [0..63] OF CHAR;
    buf_u : ARRAY [0..63] OF CHAR;
  BEGIN
    j := len - NUMBER (buf_t);  (* last index that will fill a buffer *)
    WHILE (i <= j) DO
      (*47 TextStats.NoteGround (TextStats.Op.get_chars); 74*) 
      t.get_chars (buf_t, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_chars); 74*) 
      (*47 TextStats.NoteGround (TextStats.Op.get_chars); 74*) 
      u.get_chars (buf_u, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_chars); 74*) 
      IF (buf_t # buf_u) THEN RETURN FALSE; END;
      INC (i, NUMBER (buf_t));
      (*47 TextStats.NoteIter (TextStats.Op.Equal); 74*) 
    END;

    j := len - i;  (* # remaining characters *)
    IF (j <= 0) THEN RETURN TRUE; END;

    (*47 TextStats.NoteGround (TextStats.Op.get_chars); 74*) 
    t.get_chars (buf_t, i);
    (*47 TextStats.NoteFinished (TextStats.Op.get_chars); 74*) 
    (*47 TextStats.NoteGround (TextStats.Op.get_chars); 74*) 
    u.get_chars (buf_u, i);
    (*47 TextStats.NoteFinished (TextStats.Op.get_chars); 74*) 
    RETURN SUBARRAY (buf_t, 0, j) = SUBARRAY (buf_u, 0, j);
  END EqualBuf8;

PROCEDURE EqualBufAny (t, u: TEXT;  len: CARDINAL): BOOLEAN =
  (* PRE: len = Length(t) = Length(u). *) 
  VAR
    i     : CARDINAL := 0;
    j     : INTEGER;
    buf_t : ARRAY [0..63] OF WIDECHAR;
    buf_u : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    j := len - NUMBER (buf_t);  (* last index that will fill a buffer *)
    WHILE (i <= j) DO
      (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
      t.get_wide_chars (buf_t, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
      u.get_wide_chars (buf_u, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      IF (buf_t # buf_u) THEN RETURN FALSE; END;
      INC (i, NUMBER (buf_t));
      (*47 TextStats.NoteIter (TextStats.Op.Equal); 74*) 
    END;

    j := len - i;  (* # remaining characters *)
    IF (j <= 0) THEN RETURN TRUE; END;

    (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
    t.get_wide_chars (buf_t, i);
    (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
    (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
    u.get_wide_chars (buf_u, i);
    (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
    RETURN SUBARRAY (buf_t, 0, j) = SUBARRAY (buf_u, 0, j);
  END EqualBufAny;

PROCEDURE Compare (t, u: T): [-1..1] =
  VAR Result : [-1..1]; 
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.Compare); 74*) 
    IF TextClass.Old 
    THEN Result := OldCompare (t, u);
    ELSE Result := NewCompare (t, u);
    END;
    (*47 TextStats.NoteFinished (TextStats.Op.Compare); 74*) 
    RETURN Result
  END Compare;

PROCEDURE OldCompare (t, u: T): [-1..1] =
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
  END OldCompare;

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
      (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
      t.get_wide_chars (buf_t, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
      u.get_wide_chars (buf_u, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      cur_len := MIN (min_len - i, NUMBER (buf_t));
      j := String16.Compare (ADR (buf_t[0]), cur_len, ADR (buf_u[0]), cur_len);
      IF (j # 0) THEN RETURN j; END;
      INC (i, NUMBER (buf_t));
      (*47 TextStats.NoteIter (TextStats.Op.Compare); 74*) 
    END;

    IF (len_t = len_u) THEN RETURN 0; END;
    RETURN Map [len_t < len_u];
  END CompareBuf;

PROCEDURE NewCompare (t, u: T): [-1..1] =
  VAR info_t, info_u: Info;
  BEGIN
    t.get_info (info_t);
    u.get_info (info_u);

    IF info_t.wide 
    THEN 
      IF NOT info_u.wide OR info_t.start = NIL OR info_u.start = NIL 
      THEN RETURN CompareBufAny (t, u, info_t.length, info_u.length)
      ELSE (* info_t.wide AND info_u.wide 
              AND info_t.start # NIL AND info_u.start # NIL *) 
        RETURN String16.Compare (info_t.start, info_t.length,
                                 info_u.start, info_u.length)
      END (* IF *) 
    ELSE 
      IF info_u.wide  
      THEN RETURN CompareBufAny (t, u, info_t.length, info_u.length)
      ELSIF info_t.start = NIL OR info_u.start = NIL
      THEN RETURN CompareBuf8 (t, u, info_t.length, info_u.length)
      ELSE (* NOT info_t.wide AND NOT info_u.wide 
              AND info_t.start # NIL AND info_u.start # NIL *) 
        RETURN String8.Compare (info_t.start, info_t.length,
                                info_u.start, info_u.length);
      END (* IF *) 
    END (* IF *) 
  END NewCompare;

CONST Map = ARRAY BOOLEAN OF [-1..1] { 1, -1 };

PROCEDURE CompareBuf8 (t, u: TEXT;  len_t, len_u: CARDINAL): [-1 .. 1] =
  (* PRE: len_t = Length(t) AND len_u = Length(u). *) 
  (* PRE: Neither t nor u contains any wide chars. *) 
  VAR
    min_len : CARDINAL   := MIN (len_t, len_u);
    cur_len : CARDINAL   := 0;
    i       : CARDINAL   := 0;
    j       : [-1 .. +1] := 0;
    buf_t   : ARRAY [0..63] OF CHAR;
    buf_u   : ARRAY [0..63] OF CHAR;
  BEGIN
    WHILE (i < min_len) DO
      (*47 TextStats.NoteGround (TextStats.Op.get_chars); 74*) 
      t.get_chars (buf_t, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_chars); 74*) 
      (*47 TextStats.NoteGround (TextStats.Op.get_chars); 74*) 
      u.get_chars (buf_u, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_chars); 74*) 
      cur_len := MIN (min_len - i, NUMBER (buf_t));
      j := String8.Compare (ADR (buf_t[0]), cur_len, ADR (buf_u[0]), cur_len);
      IF (j # 0) THEN RETURN j; END;
      INC (i, NUMBER (buf_t));
      (*47 TextStats.NoteIter (TextStats.Op.Compare); 74*) 
    END;

    IF (len_t = len_u) THEN RETURN 0; END;
    RETURN Map [len_t < len_u];
  END CompareBuf8;

PROCEDURE CompareBufAny (t, u: TEXT;  len_t, len_u: CARDINAL): [-1 .. 1] =
  (* PRE: len_t = Length(t) AND len_u = Length(u). *) 
  VAR
    min_len : CARDINAL   := MIN (len_t, len_u);
    cur_len : CARDINAL   := 0;
    i       : CARDINAL   := 0;
    j       : [-1 .. +1] := 0;
    buf_t   : ARRAY [0..63] OF WIDECHAR;
    buf_u   : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    WHILE (i < min_len) DO
      (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
      t.get_wide_chars (buf_t, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
      u.get_wide_chars (buf_u, i);
      (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      cur_len := MIN (min_len - i, NUMBER (buf_t));
      j := String16.Compare (ADR (buf_t[0]), cur_len, ADR (buf_u[0]), cur_len);
      IF (j # 0) THEN RETURN j; END;
      INC (i, NUMBER (buf_t));
      (*47 TextStats.NoteIter (TextStats.Op.Compare); 74*) 
    END;

    IF (len_t = len_u) THEN RETURN 0; END;
    RETURN Map [len_t < len_u];
  END CompareBufAny;

PROCEDURE Cat (t, u: TEXT): TEXT =
  BEGIN
    RETURN t & u;  (* => call RTHooks.Concat(t, u) *)
  END Cat;

PROCEDURE Hash (t: T): Word.T =
  VAR Result : Word.T; 
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.Hash); 74*) 
    IF TextClass.Old 
    THEN Result := OldHash (t);
    ELSE Result := NewHash (t);
    END;
    (*47 TextStats.NoteFinished (TextStats.Op.Hash); 74*) 
    RETURN Result
  END Hash;

PROCEDURE OldHash (t: T): Word.T =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    IF (i.start = NIL) THEN  RETURN HashBuf (t, i.length);
    ELSIF NOT i.wide   THEN  RETURN String8.Hash (i.start, i.length, i.length);
    ELSE                     RETURN String16.Hash (i.start, i.length, i.length);
    END;
  END OldHash;

PROCEDURE HashBuf (t: T;  len: CARDINAL): Word.T =
  VAR
    result : Word.T   := len;
    start  : CARDINAL := 0;
    buf    : ARRAY [0..127] OF WIDECHAR;
  BEGIN
    WHILE start < len DO
      (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
      t.get_wide_chars (buf, start);
      (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      result := String16.Hash (ADR (buf[0]),
                  MIN (len - start, NUMBER (buf)), result);
      INC (start, NUMBER (buf));
      (*47 TextStats.NoteIter (TextStats.Op.Hash); 74*) 
    END;
    RETURN result;
  END HashBuf;

PROCEDURE NewHash (t: T): Word.T =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    IF (i.start = NIL) 
    THEN IF NOT i.wide
         THEN RETURN HashBuf8 (t, i.length)
         ELSE RETURN HashBufAny (t, i.length)
         END
    ELSE IF NOT i.wide   
         THEN RETURN String8.Hash (i.start, i.length, i.length)
         ELSE RETURN String16.Hash (i.start, i.length, i.length)
         END
    END;
  END NewHash;

PROCEDURE HashBuf8 (t: T;  len: CARDINAL): Word.T =
  (* PRE: len = Length(t). *) 
  (* PRE: t contains no wide characters. *) 
  VAR
    result : Word.T   := len;
    start  : CARDINAL := 0;
    buf    : ARRAY [0..127] OF CHAR;
  BEGIN
    WHILE start < len DO
      (*47 TextStats.NoteGround (TextStats.Op.get_chars); 74*) 
      t.get_chars (buf, start);
      (*47 TextStats.NoteFinished (TextStats.Op.get_chars); 74*) 
      result := String8.Hash (ADR (buf[0]),
                  MIN (len - start, NUMBER (buf)), result);
      INC (start, NUMBER (buf));
      (*47 TextStats.NoteIter (TextStats.Op.Hash); 74*) 
    END;
    RETURN result;
  END HashBuf8;

PROCEDURE HashBufAny (t: T;  len: CARDINAL): Word.T =
  (* PRE: len = Length(t). *) 
  VAR
    result : Word.T   := len;
    start  : CARDINAL := 0;
    buf    : ARRAY [0..127] OF WIDECHAR;
  BEGIN
    WHILE start < len DO
      (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
      t.get_wide_chars (buf, start);
      (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      result := String16.Hash (ADR (buf[0]),
                  MIN (len - start, NUMBER (buf)), result);
      INC (start, NUMBER (buf));
      (*47 TextStats.NoteIter (TextStats.Op.Hash); 74*) 
    END;
    RETURN result;
  END HashBufAny;

PROCEDURE HasWideChars (t: T): BOOLEAN =
  VAR Result : BOOLEAN; 
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.HasWideChars); 74*) 
    IF TextClass.Old 
    THEN Result := NewHasWideChars (t);
(* Since OldHasWideChars, though very efficient, is buggy, we don't use it. *) 
    ELSE Result := NewHasWideChars (t);
    END;
    (*47 TextStats.NoteFinished (TextStats.Op.HasWideChars); 74*) 
    RETURN Result
  END HasWideChars;

PROCEDURE OldHasWideChars (t: T): BOOLEAN =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    RETURN i.wide;
  END OldHasWideChars;

PROCEDURE NewHasWideChars (t: T): BOOLEAN =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    IF NOT i.wide THEN RETURN FALSE;
    ELSIF i.start = NIL THEN RETURN HasWideCharsBuf16 (t, i.length);
    ELSE RETURN String16.HasWideChars (i.start, i.length);
    END;
  END NewHasWideChars;

PROCEDURE HasWideCharsBuf16 (t: T;  len: CARDINAL): BOOLEAN =
  (* PRE: len = Length(t). *) 
  VAR
    start  : CARDINAL := 0;
    buf    : ARRAY [0..127] OF WIDECHAR;
  BEGIN
    WHILE start < len DO
      (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
      t.get_wide_chars (buf, start);
      (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      IF String16.HasWideChars 
           (ADR (buf[0]), MIN (len - start, NUMBER (buf)))
      THEN RETURN TRUE; 
      ELSE 
        INC (start, NUMBER (buf));
        (*47 TextStats.NoteIter (TextStats.Op.HasWideChars); 74*) 
      END; 
    END;
    RETURN FALSE;
  END HasWideCharsBuf16;

PROCEDURE GetChar (t: T; i: CARDINAL): CHAR =
  VAR Result : CHAR;
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.GetChar); 74*) 
    (*47 TextStats.NoteGround (TextStats.Op.get_char); 74*) 
    Result := t.get_char (i);
    (*47 TextStats.NoteFinished (TextStats.Op.get_char); 74*) 
    (*47 TextStats.NoteFinished (TextStats.Op.GetChar); 74*) 
    RETURN Result 
  END GetChar;

PROCEDURE GetWideChar (t: T; i: CARDINAL): WIDECHAR =
  VAR Result : WIDECHAR;
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.GetWideChar); 74*) 
    (*47 TextStats.NoteGround (TextStats.Op.get_wide_char); 74*) 
    Result := t.get_wide_char (i);
    (*47 TextStats.NoteFinished (TextStats.Op.get_wide_char); 74*) 
    (*47 TextStats.NoteFinished (TextStats.Op.GetWideChar); 74*) 
    RETURN Result 
  END GetWideChar;

PROCEDURE SetChars (VAR a: ARRAY OF CHAR;  t: T;  start: CARDINAL) =
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.SetChars); 74*) 
    (*47 TextStats.NoteGround (TextStats.Op.get_chars); 74*) 
    t.get_chars (a, start);
    (*47 TextStats.NoteFinished (TextStats.Op.get_chars); 74*) 
    (*47 TextStats.NoteFinished (TextStats.Op.SetChars); 74*) 
  END SetChars;

PROCEDURE SetWideChars (VAR a: ARRAY OF WIDECHAR;  t: T;  start: CARDINAL) =
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.SetWideChars); 74*) 
    (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
    t.get_wide_chars (a, start);
    (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
    (*47 TextStats.NoteFinished (TextStats.Op.SetWideChars); 74*) 
  END SetWideChars;

VAR fromCharCache := ARRAY CHAR OF T {NIL, ..}; (* 1-char texts *)

PROCEDURE FromChar (c: CHAR): T =
  VAR Result: T; 
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.FromChar); 74*) 
    Result := FromCharInner (c);
    (*47 TextStats.NoteFinished (TextStats.Op.FromChar); 74*) 
    RETURN Result;
  END FromChar;

PROCEDURE FromCharInner (c: CHAR): T =
  VAR buf: ARRAY [0..0] OF CHAR;
  BEGIN
    IF fromCharCache [c] = NIL THEN
      buf [0] := c;
      fromCharCache[c] := Text8.New (buf);
    END;
    RETURN fromCharCache [c];
  END FromCharInner;

PROCEDURE FromWideChar (c: WIDECHAR): T =
  VAR buf: ARRAY [0..0] OF WIDECHAR;
  VAR Result: T; 
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.FromWideChar); 74*) 
    IF ORD (c) <= ORD (LAST (CHAR))
      THEN Result := FromCharInner (VAL (ORD (c), CHAR));
      ELSE buf[0] := c;  Result := Text16.New (buf);
    END;
    (*47 TextStats.NoteFinished (TextStats.Op.FromWideChar); 74*) 
    RETURN Result;
  END FromWideChar;

PROCEDURE FromChars (READONLY a: ARRAY OF CHAR): T =
  VAR n := NUMBER (a);
  VAR Result: T; 
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.FromChars); 74*) 
    IF n = 0 THEN Result := "" 
    ELSIF n = 1 THEN Result := FromCharInner (a [0])
    ELSE Result := Text8.New (a);
    END; 
    (*47 TextStats.NoteFinished (TextStats.Op.FromChars); 74*) 
    RETURN Result;
  END FromChars;

PROCEDURE FromWideChars (READONLY a: ARRAY OF WIDECHAR): T =
  VAR n := NUMBER (a);
  VAR Result: T; 
  BEGIN
    (*47 TextStats.NoteGround (TextStats.Op.FromWideChars); 74*) 
    IF n = 0 THEN Result := ""
    ELSIF n = 1 THEN Result := FromWideChar (a [0])
    ELSE Result := Text16.New (a)
    END; 
    (*47 TextStats.NoteFinished (TextStats.Op.FromWideChars); 74*) 
    RETURN Result;
  END FromWideChars;

VAR FindCharOp: TextStats.Op; 

PROCEDURE FindChar (t: T;  c: CHAR;  start := 0): INTEGER =
  VAR Result : INTEGER; 
  BEGIN
    FindCharOp := TextStats.Op.FindChar;
    (*47 TextStats.NoteGround (TextStats.Op.FindChar); 74*) 
    IF TextClass.Old
    THEN Result := OldFindChar (t, c, start); 
    ELSE Result := NewFindChar (t, c, start);
    END;
    (*47 TextStats.NoteFinished (TextStats.Op.FindChar); 74*) 
    RETURN Result
  END FindChar; 

PROCEDURE FindWideChar (t: T;  c: WIDECHAR;  start := 0): INTEGER =
  VAR Result : INTEGER; 
  BEGIN
    FindCharOp := TextStats.Op.FindWideChar;
    (*47 TextStats.NoteGround (TextStats.Op.FindWideChar); 74*) 
    IF TextClass.Old
    THEN Result := OldFindWideChar (t, c, start); 
    ELSE Result := NewFindWideChar (t, c, start);
    END;
    (*47 TextStats.NoteFinished (TextStats.Op.FindWideChar); 74*) 
    RETURN Result
  END FindWideChar; 

PROCEDURE OldFindChar (t: T;  c: CHAR;  start := 0): INTEGER =
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
  END OldFindChar;

PROCEDURE OldFindWideChar (t: T;  c: WIDECHAR;  start := 0): INTEGER =
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
  END OldFindWideChar;

PROCEDURE FindCharBuf (t: T;  c: WIDECHAR;
                       start: INTEGER;  len: CARDINAL): INTEGER =
  VAR
    i   : CARDINAL := MAX (0, start);
    j   : CARDINAL := NUMBER (buf);
    buf : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    LOOP
      IF (i >= len) THEN RETURN -1 END;
      IF (j >= NUMBER (buf)) THEN  
        j := 0;  
        (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
        t.get_wide_chars (buf, i);  
        (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
      END;
      IF (buf[j] = c) THEN RETURN i END;
      INC (i);  INC (j);
      (*47 TextStats.NoteIter (FindCharOp); 74*) 
    END;
  END FindCharBuf;

PROCEDURE NewFindChar (t: T;  c: CHAR;  start := 0): INTEGER =
  VAR i: Info;  res: INTEGER;
  BEGIN
    IF start < 0 THEN RETURN -1
    ELSE 
      t.get_info (i);
      IF start >= i.length THEN RETURN -1
      ELSIF i.wide THEN 
        IF i.start = NIL 
        THEN
          RETURN FindCharBufAny (t, VAL (ORD(c), WIDECHAR), start, i.length)
        ELSE
          res := String16.FindChar (i.start + start * ADRSIZE (WIDECHAR),
                                    i.length - start, VAL (ORD (c), WIDECHAR));
          IF (res >= 0) THEN INC (res, start); END;
          RETURN res
        END
      ELSE (* No wide chars in t. *) 
        IF i.start = NIL 
        THEN
          RETURN FindCharBuf8 (t, c, start, i.length)
        ELSE 
          res := String8.FindChar (i.start + start * ADRSIZE (CHAR),
                                   i.length - start, c);
          IF (res >= 0) THEN INC (res, start); END;
          RETURN res
        END 
      END
    END
  END NewFindChar;

PROCEDURE NewFindWideChar (t: T;  c: WIDECHAR;  start := 0): INTEGER =
  VAR i: Info;  res: INTEGER;
  BEGIN
    IF start < 0 THEN RETURN -1
    ELSE 
      t.get_info (i);
      IF start >= i.length THEN RETURN -1
      ELSIF i.wide 
      THEN 
        IF i.start = NIL 
        THEN
          RETURN FindCharBufAny (t, c, start, i.length);
        ELSE 
          res := String16.FindChar (i.start + start * ADRSIZE (WIDECHAR),
                                    i.length - start, c);
          IF res >= 0 THEN INC (res, start) END;
          RETURN res
        END 
      ELSE (* No wide chars in t. *) 
        IF ORD (c) > ORD (LAST (CHAR)) 
        THEN RETURN -1
        ELSIF i.start = NIL 
        THEN 
          RETURN FindCharBuf8 (t, VAL(ORD(c),CHAR), start, i.length);
        ELSE
          res := String8.FindChar (i.start + start * ADRSIZE (CHAR),
                                   i.length - start, VAL (ORD(c),CHAR));
          IF res >= 0 THEN INC (res, start) END;
          RETURN res
        END
      END
    END
  END NewFindWideChar;

PROCEDURE FindCharBuf8 (t: T;  c: CHAR;
                        start: CARDINAL;  len: CARDINAL): INTEGER =
  (* PRE: len = Length(t). *) 
  (* PRE: t contains no wide characters. *) 
  VAR
    i   : CARDINAL := start;
    j   : CARDINAL := NUMBER (buf);
    buf : ARRAY [0..63] OF CHAR;
  BEGIN
    LOOP
      IF (i >= len) THEN RETURN -1 END;
      IF (j >= NUMBER (buf)) THEN  
        j := 0;  
        (*47 TextStats.NoteGround (TextStats.Op.get_chars); 74*) 
        t.get_chars (buf, i);  
        (*47 TextStats.NoteFinished (TextStats.Op.get_chars); 74*) 
        END;
      IF (buf[j] = c) THEN RETURN i END;
      INC (i);  INC (j);
      (*47 TextStats.NoteIter (FindCharOp); 74*) 
    END;
  END FindCharBuf8;

PROCEDURE FindCharBufAny (t: T;  c: WIDECHAR;
                       start: CARDINAL;  len: CARDINAL): INTEGER =
  (* PRE: len = Length(t). *) 
  VAR
    i   : CARDINAL := start;
    j   : CARDINAL := NUMBER (buf);
    buf : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    LOOP
      IF (i >= len) THEN RETURN -1 END;
      IF (j >= NUMBER (buf)) THEN  
        j := 0;  
        (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
        t.get_wide_chars (buf, i);  
        (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
        END;
      IF (buf[j] = c) THEN RETURN i END;
      INC (i);  INC (j);
      (*47 TextStats.NoteIter (FindCharOp); 74*) 
    END;
  END FindCharBufAny;

PROCEDURE FindCharR (t: T;  c: CHAR; start := LAST (INTEGER)): INTEGER =
  VAR Result : INTEGER; 
  BEGIN
    FindCharOp := TextStats.Op.FindCharR;
    (*47 TextStats.NoteGround (TextStats.Op.FindCharR); 74*) 
    IF TextClass.Old
    THEN Result := OldFindCharR (t, c, start); 
    ELSE Result := NewFindCharR (t, c, start);
    END;
    (*47 TextStats.NoteFinished (TextStats.Op.FindCharR); 74*) 
    RETURN Result
  END FindCharR; 

PROCEDURE FindWideCharR 
  (t: T;  c: WIDECHAR;  start := LAST (INTEGER)): INTEGER =
  VAR Result : INTEGER; 
  BEGIN
    FindCharOp := TextStats.Op.FindWideCharR;
    (*47 TextStats.NoteGround (TextStats.Op.FindWideCharR); 74*) 
    IF TextClass.Old
    THEN Result := OldFindWideCharR (t, c, start); 
    ELSE Result := NewFindWideCharR (t, c, start);
    END;
    (*47 TextStats.NoteFinished (TextStats.Op.FindWideCharR); 74*) 
    RETURN Result
  END FindWideCharR; 

PROCEDURE OldFindCharR (t: T;  c: CHAR;  start := LAST (INTEGER)): INTEGER =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    IF (start < 0) THEN
      RETURN -1;
    ELSIF (i.start = NIL) THEN
      RETURN FindCharRBuf (t, VAL (ORD (c), WIDECHAR), start, i.length);
    ELSIF NOT i.wide THEN
      RETURN String8.FindCharR (i.start, MIN (i.length-1, start)+1, c);
    ELSE
      RETURN String16.FindCharR (i.start, MIN (i.length-1, start)+1,
                                 VAL (ORD (c), WIDECHAR));
    END;
  END OldFindCharR;

PROCEDURE OldFindWideCharR 
  (t: T;  c: WIDECHAR;  start := LAST (INTEGER)): INTEGER =
  VAR i: Info;
  BEGIN
    t.get_info (i);
    IF (start < 0) THEN
      RETURN -1;
    ELSIF (i.start = NIL) THEN
      RETURN FindCharRBuf (t, c, start, i.length);
    ELSIF NOT i.wide THEN
      RETURN String8.FindCharR (i.start, MIN (i.length-1, start)+1,
                                VAL (Word.And (ORD (c), 16_ff), CHAR));
    ELSE
      RETURN String16.FindCharR (i.start, MIN (i.length-1, start)+1, c);
    END;
  END OldFindWideCharR;

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
        (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
        t.get_wide_chars (buf, MAX (0, i - LAST (buf)));
        (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
        j := MIN (i, LAST (buf));
      END;
      IF (buf[j] = c) THEN RETURN i END;
      DEC (i);  DEC (j);
      (*47 TextStats.NoteIter (FindCharOp); 74*) 
    END;
  END FindCharRBuf;

PROCEDURE NewFindCharR (t: T;  c: CHAR;  start := LAST (INTEGER)): INTEGER =
  VAR i: Info;
  BEGIN
    IF start < 0 THEN RETURN -1
    ELSE 
      t.get_info (i);
      IF i.wide THEN 
        IF i.start = NIL 
        THEN
          RETURN FindCharRBufAny (t, VAL (ORD(c), WIDECHAR), start, i.length)
        ELSE
          RETURN 
            String16.FindCharR 
              (i.start, MIN (i.length-1, start)+1, VAL (ORD(c), WIDECHAR))
        END
      ELSE (* No wide chars in t. *) 
        IF i.start = NIL 
        THEN
          RETURN FindCharRBuf8 (t, c, start, i.length)
        ELSE 
          RETURN String8.FindCharR (i.start, MIN (i.length-1, start)+1, c)
        END 
      END
    END
  END NewFindCharR;

PROCEDURE NewFindWideCharR 
  (t: T; c: WIDECHAR; start := LAST (INTEGER)): INTEGER =
  VAR i: Info; 
  BEGIN
    IF start < 0 THEN RETURN -1
    ELSE 
      t.get_info (i);
      IF i.wide 
      THEN 
        IF i.start = NIL 
        THEN
          RETURN FindCharRBufAny (t, c, start, i.length)
        ELSE 
          RETURN String16.FindCharR (i.start, MIN (i.length-1, start)+1, c)
        END 
      ELSE (* No wide chars in t. *) 
        IF ORD (c) > ORD (LAST (CHAR)) 
        THEN RETURN -1
        ELSIF i.start = NIL 
        THEN 
          RETURN FindCharRBuf8 (t, VAL(ORD(c),CHAR), start, i.length);
        ELSE
          RETURN 
            String8.FindCharR 
              (i.start, MIN (i.length-1, start)+1, VAL (ORD(c), CHAR))
        END
      END
    END 
  END NewFindWideCharR;

PROCEDURE FindCharRBuf8 (t: TEXT;  c: CHAR;
                         start: CARDINAL;  len: CARDINAL): INTEGER =
  (* PRE: len = Length(t). *) 
  (* PRE: t contains no wide characters. *) 
  VAR
    i   : INTEGER  := MIN (len-1, start);
    j   : INTEGER  := -1;
    buf : ARRAY [0..63] OF CHAR;
  BEGIN
    LOOP
      IF (i < 0) THEN RETURN -1 END;
      IF (j < 0) THEN
        (*47 TextStats.NoteGround (TextStats.Op.get_chars); 74*) 
        t.get_chars (buf, MAX (0, i - LAST (buf)));
        (*47 TextStats.NoteFinished (TextStats.Op.get_chars); 74*) 
        j := MIN (i, LAST (buf));
      END;
      IF (buf[j] = c) THEN RETURN i END;
      DEC (i);  DEC (j);
      (*47 TextStats.NoteIter (FindCharOp); 74*) 
    END
  END FindCharRBuf8;

PROCEDURE FindCharRBufAny (t: TEXT;  c: WIDECHAR;
                        start: CARDINAL;  len: CARDINAL): INTEGER =
  (* PRE: len = Length(t). *) 
  VAR
    i   : INTEGER  := MIN (len-1, start);
    j   : INTEGER  := -1;
    buf : ARRAY [0..63] OF WIDECHAR;
  BEGIN
    LOOP
      IF (i < 0) THEN RETURN -1 END;
      IF (j < 0) THEN
        (*47 TextStats.NoteGround (TextStats.Op.get_wide_chars); 74*) 
        t.get_wide_chars (buf, MAX (0, i - LAST (buf)));
        (*47 TextStats.NoteFinished (TextStats.Op.get_wide_chars); 74*) 
        j := MIN (i, LAST (buf));
      END;
      IF (buf[j] = c) THEN RETURN i END;
      DEC (i);  DEC (j);
      (*47 TextStats.NoteIter (FindCharOp); 74*) 
    END;
  END FindCharRBufAny;

BEGIN
END Text.


