(* Copyright 1997, Critical Mass, Inc.  All rights reserved. *)

UNSAFE MODULE TextCat EXPORTS RTHooks, TextCat;

IMPORT TextClass;

REVEAL
  T = Public BRANDED "TextCat.T" OBJECT OVERRIDES
    get_info       := MyGetInfo;
    get_char       := MyGetChar;
    get_wide_char  := MyGetWideChar;
    get_chars      := MyGetChars;
    get_wide_chars := MyGetWideChars;
  END;

PROCEDURE New (t, u: TEXT): TEXT =
  BEGIN
    RETURN Concat (t, u);
  END New;

(* RTHooks.Concat -- called by the inline "&" operator *)
PROCEDURE Concat (t, u: TEXT): TEXT =
  VAR ti, ui: TextClass.Info;
  BEGIN
    t.get_info (ti);  IF (ti.length <= 0) THEN RETURN u; END;
    u.get_info (ui);  IF (ui.length <= 0) THEN RETURN t; END;
    RETURN NEW (T, a := t, b := u,
                a_len := ti.length, b_len := ui.length,
                a_or_b_wide := ti.wide OR ui.wide);
  END Concat;

PROCEDURE NewMulti (READONLY x: ARRAY OF TEXT): TEXT =
  BEGIN
    RETURN MultiCat (x);
  END NewMulti;

(* RTHooks.MultiCat *)
PROCEDURE MultiCat (READONLY x: ARRAY OF TEXT): TEXT =
  VAR result: TEXT;
  BEGIN
    IF NUMBER (x) <= 0 THEN RETURN "";   END;
    IF NUMBER (x) = 1  THEN RETURN x[0]; END;

    result := x[LAST(x)];
    FOR i := LAST(x) - 1 TO 0 BY -1 DO
      result := NEW (T, a := x[i], b := result);
    END;
    RETURN result;
  END MultiCat;

PROCEDURE MyGetInfo (t: T;  VAR info: TextClass.Info) =
  BEGIN
    info.start  := NIL;
    info.length := t.a_len + t.b_len;
    info.wide   := t.a_or_b_wide;
  END MyGetInfo;

PROCEDURE MyGetChar (t: T;  index: CARDINAL): CHAR =
  BEGIN
    IF (index < t.a_len) THEN RETURN t.a.get_char (index); END;
    DEC (index, t.a_len);

    IF (index < t.b_len) THEN RETURN t.b.get_char (index); END;
    DEC (index, t.b_len);

    index := -1;  (* force a range fault *) <*NOWARN*>
  END MyGetChar;

PROCEDURE MyGetWideChar (t: T;  index: CARDINAL): WIDECHAR =
  BEGIN
    IF (index < t.a_len) THEN RETURN t.a.get_wide_char (index); END;
    DEC (index, t.a_len);

    IF (index < t.b_len) THEN RETURN t.b.get_wide_char (index); END;
    DEC (index, t.b_len);

    index := -1;  (* force a range fault *) <*NOWARN*>
  END MyGetWideChar;

PROCEDURE MyGetChars (t: T;  VAR a: ARRAY OF CHAR;  start: CARDINAL) =
  VAR u: TEXT;  a_offset, t_offset, u_offset: CARDINAL := 0;
  BEGIN
    u := t.a;
    IF (t_offset + t.a_len > start) THEN
      u_offset := MAX (start - t_offset, 0);
      u.get_chars (SUBARRAY (a, a_offset, NUMBER (a) - a_offset), u_offset);
      INC (a_offset, t.a_len - u_offset);
      IF (a_offset >= NUMBER (a)) THEN RETURN; END;
    END;
    INC (t_offset, t.a_len);

    u := t.b;
    IF (t_offset + t.b_len > start) THEN
      u_offset := MAX (start - t_offset, 0);
      u.get_chars (SUBARRAY (a, a_offset, NUMBER (a) - a_offset), u_offset);
      INC (a_offset, t.b_len - u_offset);
      IF (a_offset >= NUMBER (a)) THEN RETURN; END;
    END;
    INC (t_offset, t.b_len);
  END MyGetChars;

PROCEDURE MyGetWideChars (t: T;  VAR a: ARRAY OF WIDECHAR;  start: CARDINAL) =
  VAR u: TEXT;  a_offset, t_offset, u_offset: CARDINAL := 0;
  BEGIN
    u := t.a;
    IF (t_offset + t.a_len > start) THEN
      u_offset := MAX (start - t_offset, 0);
      u.get_wide_chars (SUBARRAY (a, a_offset, NUMBER (a) - a_offset), u_offset);
      INC (a_offset, t.a_len - u_offset);
      IF (a_offset >= NUMBER (a)) THEN RETURN; END;
    END;
    INC (t_offset, t.a_len);

    u := t.b;
    IF (t_offset + t.b_len > start) THEN
      u_offset := MAX (start - t_offset, 0);
      u.get_wide_chars (SUBARRAY (a, a_offset, NUMBER (a) - a_offset), u_offset);
      INC (a_offset, t.b_len - u_offset);
      IF (a_offset >= NUMBER (a)) THEN RETURN; END;
    END;
    INC (t_offset, t.b_len);
  END MyGetWideChars;

BEGIN
END TextCat.

